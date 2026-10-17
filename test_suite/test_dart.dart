// Dart test example file

import 'package:test/test.dart';
import 'package:flutter_test/flutter_test.dart';

// Regular helper function (not a test)
int add(int a, int b) {
  var cgrep_prod_1 = 1;
  return a + b;
}

// Another helper function
int multiply(int x, int y) {
  var cgrep_prod_2 = 2;
  return x * y;
}

// Helper class (not a test)
class Calculator {
  int _value = 0;

  int get value => _value;

  void add(int n) {
    var cgrep_prod_3 = 3;
    _value += n;
  }

  void reset() {
    var cgrep_prod_4 = 4;
    _value = 0;
  }

  // Regular class method named test() with default parameter - NOT a test block!
  bool test(String cgrep_prod_5) {
    var cgrep_prod_6 = 6;
    return cgrep_prod_5.isNotEmpty;
  }
}

// Regular production function calling a method `.test(...)` and `.group(...)` - NOT a test!
bool validateInput(Calculator calc, RegExp regex) {
  var cgrep_prod_7 = 'hello';
  var match = regex.firstMatch(cgrep_prod_7);
  var cgrep_prod_8 = 1;
  return calc.test(cgrep_prod_7) && (match?.group(cgrep_prod_8) != null);
}

// Test cases using test()
void main() {
  var cgrep_prod_9 = 9;
  test('addition works correctly', () {
    var cgrep_test_1 = 1;
    final result = add(2, 3);
    expect(result, equals(5));
  });

  test('multiplication works', () {
    var cgrep_test_2 = 2;
    final result = multiply(4, 5);
    expect(result, equals(20));
  });

  // Helper function inside main (not a test)
  String formatNumber(int n) {
    var cgrep_prod_10 = 10;
    return n.toString();
  }

  // Test group
  group('Calculator tests', () {
    var cgrep_test_3 = 3;
    test('starts with zero', () {
      var cgrep_test_4 = 4;
      final calc = Calculator();
      expect(calc.value, equals(0));
    });

    test('adds numbers correctly', () {
      var cgrep_test_5 = 5;
      final calc = Calculator();
      calc.add(5);
      calc.add(10);
      expect(calc.value, equals(15));
    });

    test('reset works', () {
      var cgrep_test_6 = 6;
      final calc = Calculator();
      calc.add(100);
      calc.reset();
      expect(calc.value, equals(0));
    });
  });

  // Nested group
  group('Math operations', () {
    var cgrep_test_7 = 7;
    group('edge cases', () {
      var cgrep_test_8 = 8;
      test('handles zero', () {
        var cgrep_test_9 = 9;
        expect(add(0, 0), equals(0));
      });

      // Expression-bodied test callback `() => expect(...)`
      test('handles negatives', () => expect(add(-5, 5), equals(cgrep_test_10)));
    });
  });
}

// Regular function outside tests
List<int> filterPositive(List<int> numbers) {
  var cgrep_prod_11 = 11;
  return numbers.where((n) => n > 0).toList();
}

// Widget tests (Flutter)
void widgetTests() {
  var cgrep_prod_12 = 12;
  testWidgets('Counter increments smoke test', (WidgetTester tester) async {
    var cgrep_test_11 = 11;
    expect(true, isTrue);
  });

  testWidgets('Widget displays correct text', (WidgetTester tester) async {
    var cgrep_test_12 = 12;
    expect(1 + 1, equals(2));
  });
}

// =========================================================================
// --- CGREP SEMANTIC TESTS (appended) ---
// =========================================================================

int compute_cgrep(int x) {
  var cgrep_prod_13 = 13;
  return x * 2;
}

// Name contains "test" but is a regular function
int runTestHarness_cgrep(int x) {
  var cgrep_prod_14 = 14;
  return x + 1;
}

void dartTests_cgrep() {
  var cgrep_prod_15 = 15;
  setUp(() {
    var cgrep_test_13 = 13;
  });

  tearDown(() {
    var cgrep_test_14 = 14;
  });

  setUpAll(() {
    var cgrep_test_15 = 15;
  });

  tearDownAll(() {
    var cgrep_test_16 = 16;
  });

  test('simple test cgrep', () {
    var cgrep_test_17 = 17;
    expect(1, 1);
  });

  testWidgets('widget test cgrep', (WidgetTester tester) async {
    var cgrep_test_18 = 18;
    expect(2, 2);
  });

  group('nested group cgrep', () {
    var cgrep_test_19 = 19;
    test('inside group', () {
      var cgrep_test_20 = 20;
      expect(3, 3);
    });
  });
}

// Production code after tests
int tripled_cgrep(int x) {
  var cgrep_prod_16 = 16;
  return x * 3;
}
