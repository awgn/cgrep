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
// =========================================================================
// --- CGREP SEMANTIC TESTS (appended) ---
// =========================================================================

int compute_cgrep(int x) {
  var CGREP_IDENTIFIER = 1;
  return x * 2;
}

// Name contains "test" but is a regular function
int runTestHarness_cgrep(int x) {
  var CGREP_IDENTIFIER = 2;
  return x + 1;
}

void dartTests_cgrep() {
  setUp(() {
    var CGREP_IDENTIFIER_TEST = 1;
  });

  tearDown(() {
    var CGREP_IDENTIFIER_TEST = 2;
  });

  setUpAll(() {
    var CGREP_IDENTIFIER_TEST = 3;
  });

  tearDownAll(() {
    var CGREP_IDENTIFIER_TEST = 4;
  });

  test('simple test cgrep', () {
    var CGREP_IDENTIFIER_TEST = 5;
    expect(1, 1);
  });

  testWidgets('widget test cgrep', (WidgetTester tester) async {
    var CGREP_IDENTIFIER_TEST = 6;
    expect(2, 2);
  });

  group('nested group cgrep', () {
    var CGREP_IDENTIFIER_TEST = 7;
    test('inside group', () {
      var CGREP_IDENTIFIER_TEST = 8;
      expect(3, 3);
    });
  });
}

// Production code after tests
int tripled_cgrep(int x) {
  var CGREP_IDENTIFIER = 3;
  return x * 3;
}
