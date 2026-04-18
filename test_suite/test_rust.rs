// 1. Normal code (Should NOT be filtered out as a test)
pub fn normal_function() {
    let CGREP_IDENTIFIER = 1;
}

// 2. Normal function with 'test' in the name but no test attribute
pub fn do_some_test_prep() {
    let CGREP_IDENTIFIER = 2;
}

// 3. Normal struct with attributes
#[derive(Debug, Clone)]
pub struct NormalStruct {
    pub CGREP_IDENTIFIER: i32,
}

// 4. Standard test
#[test]
fn basic_test() {
    let CGREP_IDENTIFIER_TEST = 4;
    assert_eq!(1, 1);
}

// 5. Standard test module
#[cfg(test)]
mod tests {
    fn helper() {
        let CGREP_IDENTIFIER_TEST = 5;
    }

    #[test]
    fn inside_mod_test() {
        let CGREP_IDENTIFIER_TEST = 6;
    }
}

// 6. Complex cfg test module (with nested brackets)
#[cfg(all(test, feature = "mock"))]
mod mock_tests {
    #[test]
    fn complex_test() {
        let CGREP_IDENTIFIER_TEST = 7;
    }
}

// 7. Tokio async test
#[tokio::test]
async fn async_tokio_test() {
    let CGREP_IDENTIFIER_TEST = 8;
}

// 8. Multiple attributes stacked
#[inline]
#[allow(dead_code)]
#[test]
#[should_panic]
fn multi_attr_test() {
    let CGREP_IDENTIFIER_TEST = 9;
    panic!("oops");
}

// 9. Rstest (third party parameterized tests)
#[rstest]
fn parameterized_test() {
    let CGREP_IDENTIFIER_TEST = 10;
}

// 10. Another testing macro with arguments
#[test_case::test_case("foo")]
fn advanced_macro_test() {
    let CGREP_IDENTIFIER_TEST = 11;
}