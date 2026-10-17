#!/usr/bin/env bash
# Test file for Bash semantic test filtering (BATS, shUnit2, bashunit)

# Production function
calculate_prod() {
    local cgrep_prod_a="$1"
    local cgrep_prod_b="$2"
    if test "$cgrep_prod_a" -gt 0; then
        echo $((cgrep_prod_a + cgrep_prod_b))
    else
        echo "$cgrep_prod_b"
    fi
}

# False positive guard: function starting with "test" as part of a longer word ("testify")
function testify() {
    local cgrep_prod_testify="${1:-default}"
    echo "$cgrep_prod_testify"
}

# 1. shUnit2 / bashunit lifecycle hooks
oneTimeSetUp() {
    cgrep_test_shunit_onetime_setup=1
}

setUp() {
    cgrep_test_shunit_setup=10
}

tearDown() {
    unset cgrep_test_shunit_teardown
}

# 2. shUnit2 / bashunit test functions (both `name() {` and `function name {` styles)
test_calculate_addition() {
    local cgrep_test_shunit_snake
    cgrep_test_shunit_snake=$(calculate_prod 2 3)
    assertEquals "5" "${cgrep_test_shunit_snake}"
}

function testCalculateZero() {
    local cgrep_test_shunit_camel
    cgrep_test_shunit_camel=$(calculate_prod 0 7)
    assertEquals "7" "$cgrep_test_shunit_camel"
}

function test_ksh_style_func {
    local cgrep_test_bashunit_ksh=42
    if [[ $cgrep_test_bashunit_ksh -eq 42 ]]; then
        echo "$cgrep_test_bashunit_ksh"
    fi
}

# 3. BATS (@test "..." { ... }) and BATS setup/teardown
setup() {
    cgrep_test_bats_setup="ready"
}

@test "addition using calculate_prod in BATS" {
    local cgrep_test_bats_block
    cgrep_test_bats_block=$(calculate_prod 10 20)
    [ "$cgrep_test_bats_block" -eq 30 ]
}

teardown() {
    cgrep_test_bats_teardown="done"
}

# Top-level production code at end of script
cgrep_prod_main_result=$(calculate_prod 1 2)
echo "$cgrep_prod_main_result"
