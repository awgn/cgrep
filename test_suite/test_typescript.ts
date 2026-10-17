// TypeScript test example file

import { describe, it, test, expect } from '@jest/globals';

// Regular helper function (not a test)
function add(a: number, b: number): number {
  const cgrep_prod_1 = 1;
  return a + b;
}

// Another helper function
function multiply(x: number, y: number): number {
  const cgrep_prod_2 = 2;
  return x * y;
}

// Helper class (not a test)
class Calculator {
  private value: number = 0;

  constructor(initial: number = 0) {
    this.value = initial;
  }

  add(n: number): Calculator {
    this.value += n;
    return this;
  }

  getValue(): number {
    const cgrep_prod_3 = 3;
    return this.value;
  }

  reset(): void {
    this.value = 0;
  }

  // Regular class method named test() with default parameter - NOT a test block!
  test(cgrep_prod_4: string = "default"): boolean {
    const cgrep_prod_5 = 5;
    return cgrep_prod_4.length > 0;
  }
}

// Regular function named `test` with default arrow function in parameter list:
// Preceded by `function`, NOT a Jest/Vitest `test(...)` call!
function test(onDone: () => number = () => { const cgrep_prod_8 = 8; return cgrep_prod_8; }): number {
  const cgrep_prod_9 = 9;
  return onDone() + cgrep_prod_9;
}

// Regular production function using RegExp.prototype.test() - NOT a test!
function validateEmail(email: string): boolean {
  const pattern = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  const cgrep_prod_6 = "extra";
  return pattern.test(email.trim() + cgrep_prod_6);
}

// Direct regex test call where argument contains prod identifier
const isValidProd = /^[a-z]+$/.test((() => {
  const cgrep_prod_7 = 7;
  return "abc";
})());

// Jest/Mocha test suite
describe('Math operations', () => {
  beforeEach(() => {
    const cgrep_test_1 = 1;
  });

  afterEach(() => {
    const cgrep_test_2 = 2;
  });

  it('should add two numbers correctly', () => {
    const cgrep_test_3 = 3;
    const result = add(2, 3);
    expect(result).toBe(5);
  });

  it('should multiply two numbers', () => {
    const cgrep_test_4 = 4;
    const result = multiply(4, 5);
    expect(result).toEqual(20);
  });

  describe('edge cases', () => {
    it('handles zero', () => {
      const cgrep_test_5 = 5;
      expect(add(0, 0)).toBe(0);
    });
  });
});

// Regular interface (not a test)
interface Person {
  name: string;
  age: number;
}

// Helper function
function createPerson(name: string, age: number): Person {
  return { name, age };
}

// Jest test() syntax
test('Calculator adds numbers', () => {
  const cgrep_test_6 = 6;
  const calc = new Calculator();
  calc.add(5).add(10);
  expect(calc.getValue()).toBe(15);
});

// Another describe block
describe('Calculator class', () => {
  test('initializes with zero', () => {
    const cgrep_test_7 = 7;
    const calc = new Calculator();
    expect(calc.getValue()).toBe(0);
  });

  it('can be initialized with value', () => {
    const cgrep_test_8 = 8;
    const calc = new Calculator(10);
    expect(calc.getValue()).toBe(10);
  });
});

// Chained modifiers: describe.skip, it.only, test.each
describe.skip('Skipped suite', () => {
  const cgrep_test_9 = 9;
});

it.only('Focused test', () => {
  const cgrep_test_10 = 10;
});

test.each([1, 2, 3])('Parameterized test %i', (n) => {
  const cgrep_test_11 = n;
});

// Alias functions: xit, fit
xit('Disabled test', () => {
  const cgrep_test_12 = 12;
});

// Top-level lifecycle hooks
beforeAll(() => {
  const cgrep_test_13 = 13;
});

afterAll(() => {
  const cgrep_test_14 = 14;
});

// Helper type
type Result<T> = {
  success: boolean;
  data?: T;
};

// Helper function at the end
function formatNumber(num: number): string {
  return num.toFixed(2);
}

// More regular code
const API_URL: string = 'https://api.example.com';

async function fetchData(endpoint: string): Promise<Response> {
  return fetch(API_URL + endpoint);
}
