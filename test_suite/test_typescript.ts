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
}

// Jest/Mocha test suite
describe('Math operations', () => {
  it('should add two numbers correctly', () => {
    const cgrep_test_1 = 1;
    const result = add(2, 3);
    expect(result).toBe(5);
  });

  it('should multiply two numbers', () => {
    const cgrep_test_2 = 2;
    const result = multiply(4, 5);
    expect(result).toEqual(20);
  });

  describe('edge cases', () => {
    it('handles zero', () => {
      const cgrep_test_3 = 3;
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
  const cgrep_test_4 = 4;
  const calc = new Calculator();
  calc.add(5).add(10);
  expect(calc.getValue()).toBe(15);
});

// Another describe block
describe('Calculator class', () => {
  test('initializes with zero', () => {
    const cgrep_test_5 = 5;
    const calc = new Calculator();
    expect(calc.getValue()).toBe(0);
  });

  it('can be initialized with value', () => {
    const cgrep_test_6 = 6;
    const calc = new Calculator(10);
    expect(calc.getValue()).toBe(10);
  });
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
