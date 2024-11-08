
pub struct Gateway<S> {
    config: Config,
    drain: drain::Watch,
    stack: Stack<S>,
}
