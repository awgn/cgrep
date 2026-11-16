/*
 * High-performance POSIX I/O, directory scanning, and SIMD prefiltering for cgrep.
 */
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef __aarch64__
#include <arm_neon.h>
#endif

#ifndef _WIN32
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

/* --------------------------------------------------------------------------
 * Fast 64-bit unrolled 7-bit ASCII validation
 * -------------------------------------------------------------------------- */
int cgrep_is_ascii(const uint8_t *buf, size_t len) {
    size_t i = 0;
    while (i < len && (((uintptr_t)(buf + i)) & 7u) != 0) {
        if (buf[i] >= 0x80u) return 0;
        i++;
    }
    const uint64_t mask = 0x8080808080808080ULL;
    while (i + 32 <= len) {
        uint64_t w0, w1, w2, w3;
        memcpy(&w0, buf + i, 8);
        memcpy(&w1, buf + i + 8, 8);
        memcpy(&w2, buf + i + 16, 8);
        memcpy(&w3, buf + i + 24, 8);
        if (((w0 | w1 | w2 | w3) & mask) != 0) return 0;
        i += 32;
    }
    while (i + 8 <= len) {
        uint64_t w;
        memcpy(&w, buf + i, 8);
        if ((w & mask) != 0) return 0;
        i += 8;
    }
    while (i < len) {
        if (buf[i] >= 0x80u) return 0;
        i++;
    }
    return 1;
}

/* --------------------------------------------------------------------------
 * Vectorized newline ('\n' = 0x0A) counter
 * -------------------------------------------------------------------------- */
size_t cgrep_count_newlines(const uint8_t *buf, size_t offset, size_t len) {
    const uint8_t *p = buf + offset;
    size_t count = 0;
    size_t i = 0;

#ifdef __aarch64__
    const uint8x16_t nl = vdupq_n_u8(0x0Au);
    while (i + 16 * 255 <= len) {
        uint8x16_t acc = vdupq_n_u8(0);
        for (int k = 0; k < 255; k++) {
            uint8x16_t v = vld1q_u8(p + i);
            uint8x16_t eq = vceqq_u8(v, nl);
            acc = vsubq_u8(acc, eq); /* eq lanes are 0xFF (-1) or 0x00 */
            i += 16;
        }
        count += (size_t)vaddlvq_u8(acc);
    }
    if (i + 16 <= len) {
        uint8x16_t acc = vdupq_n_u8(0);
        while (i + 16 <= len) {
            uint8x16_t v = vld1q_u8(p + i);
            uint8x16_t eq = vceqq_u8(v, nl);
            acc = vsubq_u8(acc, eq);
            i += 16;
        }
        count += (size_t)vaddlvq_u8(acc);
    }
#endif

    while (i < len) {
        const uint8_t *hit = (const uint8_t *)memchr(p + i, 0x0A, len - i);
        if (!hit) break;
        count++;
        i = (size_t)(hit - p) + 1;
    }
    return count;
}

/* --------------------------------------------------------------------------
 * Case-sensitive & ASCII case-insensitive substring primitives
 * -------------------------------------------------------------------------- */
static inline uint8_t ascii_lower_u8(uint8_t b) {
    return (uint8_t)((b >= 'A' && b <= 'Z') ? (b | 0x20u) : b);
}

static inline int eq_ci_slice(const uint8_t *hay, const uint8_t *ndl_lower, size_t len) {
    for (size_t k = 0; k < len; k++) {
        if (ascii_lower_u8(hay[k]) != ndl_lower[k]) return 0;
    }
    return 1;
}

int cgrep_memcontains_cs(const uint8_t *hptr, size_t hlen, const uint8_t *nptr, size_t nlen) {
    if (nlen == 0) return 1;
    if (nlen > hlen) return 0;
    uint8_t b0 = nptr[0];
    if (nlen == 1) {
        return memchr(hptr, b0, hlen) != NULL ? 1 : 0;
    }
    uint8_t bLast = nptr[nlen - 1];
    const uint8_t *cur = hptr;
    const uint8_t *endStart = hptr + (hlen - nlen);
    while (cur <= endStart) {
        size_t rem = (size_t)(endStart - cur) + 1;
        const uint8_t *hit = (const uint8_t *)memchr(cur, b0, rem);
        if (!hit) return 0;
        if (hit[nlen - 1] == bLast) {
            if (nlen == 2 || memcmp(hit + 1, nptr + 1, nlen - 2) == 0) {
                return 1;
            }
        }
        cur = hit + 1;
    }
    return 0;
}

/* Find next ASCII CI match of lowercased needle in haystack[rel_start .. hlen).
 * Returns relative offset from hptr, or -1 if not found. */
ptrdiff_t cgrep_find_ascii_ci(
    const uint8_t *hptr,
    size_t hoff,
    size_t hlen,
    size_t rel_start,
    const uint8_t *nptr_base,
    size_t noff,
    size_t nlen
) {
    if (nlen == 0 || nlen > hlen || rel_start > hlen - nlen) return -1;
    const uint8_t *hay = hptr + hoff;
    const uint8_t *ndl = nptr_base + noff;
    size_t limit = hlen - nlen;

    if (nlen == 1) {
        uint8_t b0 = ndl[0];
        if (b0 < 'a' || b0 > 'z') {
            const uint8_t *hit = (const uint8_t *)memchr(hay + rel_start, b0, hlen - rel_start);
            return hit ? (ptrdiff_t)(hit - hay) : -1;
        }
        uint8_t b0_up = (uint8_t)(b0 - 0x20u);
        size_t i = rel_start;
#ifdef __aarch64__
        uint8x16_t v_lo = vdupq_n_u8(b0);
        uint8x16_t v_20 = vdupq_n_u8(0x20u);
        while (i + 16 <= hlen) {
            uint8x16_t chunk = vld1q_u8(hay + i);
            uint8x16_t eq = vceqq_u8(vorrq_u8(chunk, v_20), v_lo);
            if (vmaxvq_u8(eq) != 0) {
                for (size_t j = 0; j < 16; j++) {
                    uint8_t c = hay[i + j];
                    if (c == b0 || c == b0_up) return (ptrdiff_t)(i + j);
                }
            }
            i += 16;
        }
#endif
        for (; i < hlen; i++) {
            uint8_t c = hay[i];
            if (c == b0 || c == b0_up) return (ptrdiff_t)i;
        }
        return -1;
    }

    /* Check if needle has any case-invariant byte (e.g. '_', '(', digits) */
    ptrdiff_t inv_idx = -1;
    for (size_t k = 0; k < nlen; k++) {
        uint8_t b = ndl[k];
        if (b < 'a' || b > 'z') {
            inv_idx = (ptrdiff_t)k;
            break;
        }
    }

    if (inv_idx >= 0) {
        size_t k = (size_t)inv_idx;
        uint8_t inv_b = ndl[k];
        size_t pos = rel_start;
        while (pos <= limit) {
            size_t search_from = pos + k;
            size_t search_len = (limit + k) - search_from + 1;
            const uint8_t *hit = (const uint8_t *)memchr(hay + search_from, inv_b, search_len);
            if (!hit) return -1;
            size_t cand_start = (size_t)(hit - hay) - k;
            if (eq_ci_slice(hay + cand_start, ndl, nlen)) {
                return (ptrdiff_t)cand_start;
            }
            pos = cand_start + 1;
        }
        return -1;
    }

    /* Pure alphabetic needle >= 2 bytes */
    uint8_t b0 = ndl[0];
    uint8_t bLast = ndl[nlen - 1];
    size_t i = rel_start;
#ifdef __aarch64__
    uint8x16_t v_b0 = vdupq_n_u8(b0);
    uint8x16_t v_bLast = vdupq_n_u8(bLast);
    uint8x16_t v_20 = vdupq_n_u8(0x20u);
    while (i + 16 <= limit + 1) {
        uint8x16_t c_first = vld1q_u8(hay + i);
        uint8x16_t c_last = vld1q_u8(hay + i + nlen - 1);
        uint8x16_t m0 = vceqq_u8(vorrq_u8(c_first, v_20), v_b0);
        uint8x16_t mL = vceqq_u8(vorrq_u8(c_last, v_20), v_bLast);
        uint8x16_t both = vandq_u8(m0, mL);
        if (vmaxvq_u8(both) != 0) {
            for (size_t j = 0; j < 16 && (i + j) <= limit; j++) {
                if (ascii_lower_u8(hay[i + j]) == b0 &&
                    ascii_lower_u8(hay[i + j + nlen - 1]) == bLast &&
                    eq_ci_slice(hay + i + j, ndl, nlen)) {
                    return (ptrdiff_t)(i + j);
                }
            }
        }
        i += 16;
    }
#endif
    for (; i <= limit; i++) {
        if (ascii_lower_u8(hay[i]) == b0 &&
            ascii_lower_u8(hay[i + nlen - 1]) == bLast &&
            eq_ci_slice(hay + i, ndl, nlen)) {
            return (ptrdiff_t)i;
        }
    }
    return -1;
}

/* --------------------------------------------------------------------------
 * Precompiled RawPrefilter C object
 * -------------------------------------------------------------------------- */
typedef struct {
    uint8_t *bytes;
    size_t len;
} CGrepNeedle;

typedef struct {
    int mode; /* 0 = AcceptAll, 1 = RejectAll, 2 = RequireAnyCS, 3 = RequireAnyCI */
    size_t num_needles;
    CGrepNeedle *needles;
} CGrepPrefilter;

CGrepPrefilter *cgrep_pf_create(int mode, size_t num_needles, const uint8_t *packed_bytes, const size_t *lengths) {
    CGrepPrefilter *pf = (CGrepPrefilter *)calloc(1, sizeof(CGrepPrefilter));
    if (!pf) return NULL;
    pf->mode = mode;
    pf->num_needles = num_needles;
    if (num_needles > 0 && packed_bytes && lengths) {
        pf->needles = (CGrepNeedle *)calloc(num_needles, sizeof(CGrepNeedle));
        if (!pf->needles) {
            free(pf);
            return NULL;
        }
        size_t off = 0;
        for (size_t i = 0; i < num_needles; i++) {
            size_t l = lengths[i];
            pf->needles[i].len = l;
            pf->needles[i].bytes = (uint8_t *)malloc(l > 0 ? l : 1);
            if (pf->needles[i].bytes && l > 0) {
                memcpy(pf->needles[i].bytes, packed_bytes + off, l);
            }
            off += l;
        }
    }
    return pf;
}

void cgrep_pf_free(CGrepPrefilter *pf) {
    if (!pf) return;
    if (pf->needles) {
        for (size_t i = 0; i < pf->num_needles; i++) {
            free(pf->needles[i].bytes);
        }
        free(pf->needles);
    }
    free(pf);
}

int cgrep_pf_eval(const CGrepPrefilter *pf, const uint8_t *buf, size_t len) {
    if (!pf || pf->mode == 0) return 1;
    if (pf->mode == 1) return 0;
    if (pf->mode == 2) {
        for (size_t i = 0; i < pf->num_needles; i++) {
            if (cgrep_memcontains_cs(buf, len, pf->needles[i].bytes, pf->needles[i].len)) {
                return 1;
            }
        }
        return 0;
    }
    if (pf->mode == 3) {
        for (size_t i = 0; i < pf->num_needles; i++) {
            if (cgrep_find_ascii_ci(buf, 0, len, 0, pf->needles[i].bytes, 0, pf->needles[i].len) >= 0) {
                return 1;
            }
        }
        return 0;
    }
    return 1;
}

#ifndef _WIN32
/* --------------------------------------------------------------------------
 * Single-lookup POSIX file reader + prefilter
 * Returns:
 *   0 : rejected by prefilter (zero heap needed)
 *   1 : matched in scratch_buf (*out_size bytes, *out_ascii = 0/1)
 *   2 : matched in mmap (*out_mmap_ptr, *out_size bytes, *out_ascii = 0/1)
 *  -1 : non-regular file or OS error (caller falls back to Haskell IO)
 * -------------------------------------------------------------------------- */
int cgrep_read_prefilter(
    const char *path,
    const CGrepPrefilter *pf,
    uint8_t *scratch_buf,
    size_t scratch_cap,
    size_t *out_size,
    int *out_ascii,
    void **out_mmap_ptr
) {
    if (pf && pf->mode == 1) {
        return 0;
    }
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        return -1;
    }
    struct stat st;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode)) {
        close(fd);
        return -1;
    }
    size_t sz = (size_t)st.st_size;
    if (sz == 0) {
        close(fd);
        if (!cgrep_pf_eval(pf, scratch_buf, 0)) return 0;
        *out_size = 0;
        *out_ascii = 1;
        *out_mmap_ptr = NULL;
        return 1;
    }
    if (sz <= scratch_cap) {
        size_t got = 0;
        while (got < sz) {
            ssize_t r = read(fd, scratch_buf + got, sz - got);
            if (r < 0) {
                if (errno == EINTR) continue;
                close(fd);
                return -1;
            }
            if (r == 0) break;
            got += (size_t)r;
        }
        close(fd);
        if (!cgrep_pf_eval(pf, scratch_buf, got)) {
            return 0;
        }
        *out_size = got;
        *out_ascii = cgrep_is_ascii(scratch_buf, got);
        *out_mmap_ptr = NULL;
        return 1;
    } else {
        void *p = mmap(NULL, sz, PROT_READ, MAP_PRIVATE, fd, 0);
        close(fd);
        if (p == MAP_FAILED) {
            return -1;
        }
        if (!cgrep_pf_eval(pf, (const uint8_t *)p, sz)) {
            munmap(p, sz);
            return 0;
        }
        *out_size = sz;
        *out_ascii = cgrep_is_ascii((const uint8_t *)p, sz);
        *out_mmap_ptr = p;
        return 2;
    }
}

void cgrep_munmap(void *ptr, size_t len) {
    if (ptr && len > 0) {
        munmap(ptr, len);
    }
}

/* --------------------------------------------------------------------------
 * Fast POSIX directory scanner using dirent->d_type (zero stat() on REG/DIR!)
 * Entry wire format in out_buf:
 *   [uint8_t kind] [uint8_t len_lo] [uint8_t len_hi] [char name[len]]
 * kind:
 *   1 = regular file
 *   2 = real directory (DT_DIR)
 *   3 = symlinked directory (only when follow_symlinks != 0)
 * Returns:
 *   0 = ok (*out_used set)
 *  -1 = opendir error
 *  -2 = out_buf too small (caller should grow buffer or fallback)
 * -------------------------------------------------------------------------- */
int cgrep_scandir(
    const char *dir_path,
    int follow_symlinks,
    uint8_t *out_buf,
    size_t out_cap,
    size_t *out_used
) {
    DIR *d = opendir(dir_path);
    if (!d) return -1;

    size_t used = 0;
    size_t dir_len = strlen(dir_path);
    char full_tmp[4096];

    struct dirent *ent;
    while ((ent = readdir(d)) != NULL) {
        const char *name = ent->d_name;
        if (name[0] == '.' && (name[1] == '\0' || (name[1] == '.' && name[2] == '\0'))) {
            continue;
        }
        size_t nlen = strlen(name);
        if (nlen == 0 || nlen > 65535) continue;

        uint8_t kind = 0;
#ifdef DT_REG
        if (ent->d_type == DT_REG) {
            kind = 1;
        } else if (ent->d_type == DT_DIR) {
            kind = 2;
        } else if (ent->d_type == DT_LNK || ent->d_type == DT_UNKNOWN)
#endif
        {
            if (dir_len + 1 + nlen < sizeof(full_tmp)) {
                memcpy(full_tmp, dir_path, dir_len);
                size_t p = dir_len;
                if (p > 0 && full_tmp[p - 1] != '/') {
                    full_tmp[p++] = '/';
                }
                memcpy(full_tmp + p, name, nlen + 1);
                struct stat st;
                int rc = follow_symlinks ? stat(full_tmp, &st) : lstat(full_tmp, &st);
                if (rc == 0) {
                    if (S_ISREG(st.st_mode)) {
                        kind = 1;
                    } else if (S_ISDIR(st.st_mode)) {
                        kind = (ent->d_type == DT_LNK) ? 3 : 2;
                    } else if (S_ISLNK(st.st_mode)) {
                        /* With -r (follow_symlinks == 0), still allow file symlinks if target is reg file,
                         * or skip directory symlinks as documented ("don't follow symlinks") */
                        struct stat st_target;
                        if (stat(full_tmp, &st_target) == 0 && S_ISREG(st_target.st_mode)) {
                            kind = 1;
                        }
                    }
                }
            }
        }

        if (kind == 0) continue;

        if (used + 3 + nlen > out_cap) {
            closedir(d);
            return -2;
        }
        out_buf[used] = kind;
        out_buf[used + 1] = (uint8_t)(nlen & 0xFFu);
        out_buf[used + 2] = (uint8_t)((nlen >> 8) & 0xFFu);
        memcpy(out_buf + used + 3, name, nlen);
        used += 3 + nlen;
    }

    closedir(d);
    *out_used = used;
    return 0;
}

int cgrep_dir_inode(const char *dir_path, uint64_t *out_dev, uint64_t *out_ino) {
    struct stat st;
    if (stat(dir_path, &st) != 0) return -1;
    *out_dev = (uint64_t)st.st_dev;
    *out_ino = (uint64_t)st.st_ino;
    return 0;
}
#endif
