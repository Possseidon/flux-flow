use std::sync::Arc;

struct Stack;

impl Stack {
    fn get<T>(&self) -> Arc<T> {
        todo!()
    }

    fn push<T>(&mut self, value: Arc<T>) {
        todo!()
    }
}

trait Value {}

trait Arg {
    fn ret(self: Arc<Self>, value: Arc<dyn Value>) -> Arc<dyn Ret>;
}

trait Ret {}

fn sort(stack: Arc<dyn Arg>) -> Arc<dyn Ret> {
    stack.ret()
}

fn main() {}
