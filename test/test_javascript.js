// JavaScript test example file

// Regular helper function (not a test)
function calculateSum(a, b) {
    return a + b;
}

// Another helper function
const multiply = (x, y) => {
    return x * y;
}

// Jest/Mocha/Jasmine test suite
describe('Math operations', function() {
    // Test case with traditional function syntax
    it('should add two numbers correctly', function() {
        const result = calculateSum(2, 3);
        expect(result).toBe(5);
    });

    // Test case with arrow function syntax
    it('should multiply two numbers', () => {
        const result = multiply(4, 5);
        expect(result).toEqual(20);
    });

    // Nested describe block
    describe('edge cases', () => {
        it('handles zero', () => {
            expect(calculateSum(0, 0)).toBe(0);
        });
    });
});

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
        return this.value;
    }
}

// Jest test() syntax
test('Calculator adds numbers', () => {
    const calc = new Calculator();
    calc.add(5).add(10);
    expect(calc.getValue()).toBe(15);
});

// Another describe block
describe('Calculator class', function() {
    test('initializes with zero', function() {
        const calc = new Calculator();
        expect(calc.getValue()).toBe(0);
    });
});

// Mocha context syntax (alias for describe)
context('when testing strings', () => {
    it('should concatenate strings', () => {
        const str = 'hello' + ' ' + 'world';
        expect(str).toBe('hello world');
    });
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