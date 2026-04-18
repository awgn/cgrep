// TypeScript test example file

import { describe, it, test, expect } from '@jest/globals';

// Regular helper function (not a test)
function add(a: number, b: number): number {
  return a + b;
}

// Another helper function
function multiply(x: number, y: number): number {
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
    return this.value;
  }

  reset(): void {
    this.value = 0;
  }
}

// Jest/Mocha test suite
describe('Math operations', () => {
  it('should add two numbers correctly', () => {
    const result = add(2, 3);
    expect(result).toBe(5);
  });

  it('should multiply two numbers', () => {
    const result = multiply(4, 5);
    expect(result).toEqual(20);
  });

  describe('edge cases', () => {
    it('handles zero', () => {
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
  const calc = new Calculator();
  calc.add(5).add(10);
  expect(calc.getValue()).toBe(15);
});

// Another describe block
describe('Calculator class', () => {
  test('initializes with zero', () => {
    const calc = new Calculator();
    expect(calc.getValue()).toBe(0);
  });

  it('can be initialized with value', () => {
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