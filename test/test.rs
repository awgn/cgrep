pub struct Gateway<S> {
    config: Config,
    drain: drain::Watch,
    stack: Stack<S>,
}

pub fn compute(x: i32, label: &str) -> i32 {
    let base: i32 = 42;
    let tag: &str = "default";
    if x > 0 {
        return x + base;
    }
    0
}

fn main() {
    let count: i32 = 10;
    let res: i32 = compute(count, "prod");
    test.method();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
