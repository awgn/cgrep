
pub struct Gateway<S> {
    config: Config,
    drain: drain::Watch,
    stack: Stack<S>,
}

fn main() {
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
