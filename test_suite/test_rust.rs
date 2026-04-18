// 1. Normal code (Should NOT be filtered out as a test)
pub fn normal_function() {
    let cgrep_prod_1 = 1;
}

// 2. Normal function with 'test' in the name but no test attribute
pub fn do_some_test_prep() {
    let cgrep_prod_2 = 2;
}

// 3. Normal struct with attributes
#[derive(Debug, Clone)]
pub struct NormalStruct {
    pub cgrep_prod_3: i32,
}

// 4. Standard test
#[test]
fn basic_test() {
    let cgrep_test_1 = 1;
    assert_eq!(1, 1);
}

// 5. Standard test module
#[cfg(test)]
mod tests {
    fn helper() {
        let cgrep_test_2 = 2;
    }

    #[test]
    fn inside_mod_test() {
        let cgrep_test_3 = 3;
    }
}

// 6. Complex cfg test module (with nested brackets)
#[cfg(all(test, feature = "mock"))]
mod mock_tests {
    #[test]
    fn complex_test() {
        let cgrep_test_4 = 4;
    }
}

// 7. Tokio async test
#[tokio::test]
async fn async_tokio_test() {
    let cgrep_test_5 = 5;
}

// 8. Multiple attributes stacked
#[inline]
#[allow(dead_code)]
#[test]
#[should_panic]
fn multi_attr_test() {
    let cgrep_test_6 = 6;
    panic!("oops");
}

// 9. Rstest (third party parameterized tests)
#[rstest]
fn parameterized_test() {
    let cgrep_test_7 = 7;
}

// 10. Another testing macro with arguments
#[test_case::test_case("foo")]
fn advanced_macro_test() {
    let cgrep_test_8 = 8;
}