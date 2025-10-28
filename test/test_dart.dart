// Dart test example file

import 'package:test/test.dart';
import 'package:flutter_test/flutter_test.dart';

// Regular helper function (not a test)
int add(int a, int b) {
  return a + b;
}

// Another helper function
int multiply(int x, int y) {
  return x * y;
}

// Helper class (not a test)
class Calculator {
  int _value = 0;

  int get value => _value;

  void add(int n) {
    _value += n;
  }

  void reset() {
    _value = 0;
  }
}

// Test cases using test()
void main() {
  test('addition works correctly', () {
    final result = add(2, 3);
    expect(result, equals(5));
  });

  test('multiplication works', () {
    final result = multiply(4, 5);
    expect(result, equals(20));
  });

  // Helper function inside main (not a test)
  String formatNumber(int n) {
    return n.toString();
  }

  // Test group
  group('Calculator tests', () {
    test('starts with zero', () {
      final calc = Calculator();
      expect(calc.value, equals(0));
    });

    test('adds numbers correctly', () {
      final calc = Calculator();
      calc.add(5);
      calc.add(10);
      expect(calc.value, equals(15));
    });

    test('reset works', () {
      final calc = Calculator();
      calc.add(100);
      calc.reset();
      expect(calc.value, equals(0));
    });
  });

  // Nested group
  group('Math operations', () {
    group('edge cases', () {
      test('handles zero', () {
        expect(add(0, 0), equals(0));
      });

      test('handles negatives', () {
        expect(add(-5, 5), equals(0));
      });
    });
  });
}

// Regular function outside tests
List<int> filterPositive(List<int> numbers) {
  return numbers.where((n) => n > 0).toList();
}

// Helper class
class Person {
  final String name;
  final int age;

  Person(this.name, this.age);
}

// Widget tests (Flutter)
void widgetTests() {
  testWidgets('Counter increments smoke test', (WidgetTester tester) async {
    // Widget test code here
    expect(true, isTrue);
  });

  testWidgets('Widget displays correct text', (WidgetTester tester) async {
    // More widget test code
    expect(1 + 1, equals(2));
  });
}

// More helper code
String reverseString(String s) {
  return s.split('').reversed.join('');
}