// D test example file

int compute_cgrep(int x) {
    int cgrep_prod_1 = 1;
    return x * 2;
}

unittest {
    int cgrep_test_1 = 1;
    assert(compute_cgrep(1) == 2);
}

int runTestHarness_cgrep(int x) {
    int cgrep_prod_2 = 2;
    return x + 1;
}

unittest {
    int cgrep_test_2 = 2;
    assert(runTestHarness_cgrep(2) == 3);
}

class MyClass {
    int value;
    
    this(int v) {
        int cgrep_prod_3 = 3;
        value = v;
    }
    
    unittest {
        int cgrep_test_3 = 3;
        auto obj = new MyClass(10);
        assert(obj.value == 10);
    }
    
    int getValue() {
        int cgrep_prod_4 = 4;
        return value;
    }
}
