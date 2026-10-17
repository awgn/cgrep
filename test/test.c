/* © main hello world */
int compute(int a, double b)
{
    int limit = 0x10;
    if (a > 0) {
        return a + limit;
    }
    return 0;
}

int
main(int argc, char *argv[])
{
    const char * x = "main hello world";
    int ab = 10;
    double ratio = 3.14;
    int res = compute(ab, ratio);
    return res;
}
