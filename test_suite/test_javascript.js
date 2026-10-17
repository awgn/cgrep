// JavaScript test example file

// Regular helper function (not a test)
function calculateSum(a, b) {
    const cgrep_prod_1 = 1;
    return a + b;
}

// Another helper function
const multiply = (x, y) => {
    const cgrep_prod_2 = 2;
    return x * y;
}

// Regular class (not a test)
class Calculator {
    constructor() {
        this.value = 0;
    }

    add(n) {
        this.value += n;
        return this;
    }

    getValue() {
        const cgrep_prod_3 = 3;
        return this.value;
    }

    // Regular method named test() - NOT a test block!
    test(cgrep_prod_4 = "default") {
        const cgrep_prod_5 = 5;
        return cgrep_prod_4.length > 0;
    }
}

// Regular function using RegExp.prototype.test() - NOT a test!
function validateInput(str) {
    const regex = /^[a-z0-9]+$/i;
    const cgrep_prod_6 = "suffix";
    return regex.test(str + cgrep_prod_6);
}

// Direct regex literal .test() call
const isAlpha = /^[a-z]+$/.test((() => {
    const cgrep_prod_7 = 7;
    return "hello";
})());

// Jest/Mocha/Jasmine test suite
describe('Math operations', function() {
    beforeEach(function() {
        const cgrep_test_1 = 1;
    });

    afterEach(() => {
        const cgrep_test_2 = 2;
    });

    // Test case with traditional function syntax
    it('should add two numbers correctly', function() {
        const cgrep_test_3 = 3;
        const result = calculateSum(2, 3);
        expect(result).toBe(5);
    });

    // Test case with arrow function syntax
    it('should multiply two numbers', () => {
        const cgrep_test_4 = 4;
        const result = multiply(4, 5);
        expect(result).toEqual(20);
    });

    // Nested describe block
    describe('edge cases', () => {
        it('handles zero', () => {
            const cgrep_test_5 = 5;
            expect(calculateSum(0, 0)).toBe(0);
        });
    });
});

// Jest test() syntax
test('Calculator adds numbers', () => {
    const cgrep_test_6 = 6;
    const calc = new Calculator();
    calc.add(5).add(10);
    expect(calc.getValue()).toBe(15);
});

// Another describe block
describe('Calculator class', function() {
    test('initializes with zero', function() {
        const cgrep_test_7 = 7;
        const calc = new Calculator();
        expect(calc.getValue()).toBe(0);
    });
});

// Mocha context syntax (alias for describe)
context('when testing strings', () => {
    it('should concatenate strings', () => {
        const cgrep_test_8 = 8;
        const str = 'hello' + ' ' + 'world';
        expect(str).toBe('hello world');
    });
});

// Chained modifiers & aliases
describe.skip('skipped suite', () => {
    const cgrep_test_9 = 9;
});

it.only('focused test', function() {
    const cgrep_test_10 = 10;
});

test.each([1, 2])('parameterized %i', (x) => {
    const cgrep_test_11 = x;
});

xit('disabled test', () => {
    const cgrep_test_12 = 12;
});

beforeAll(() => {
    const cgrep_test_13 = 13;
});

afterAll(() => {
    const cgrep_test_14 = 14;
});

// Helper function at the end
function formatNumber(num) {
    return num.toFixed(2);
}

// More regular code
const API_URL = 'https://api.example.com';

function fetchData(endpoint) {
    return fetch(API_URL + endpoint);
}
