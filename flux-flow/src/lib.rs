pub mod runtime_value;
// pub mod stack;
pub mod static_type;

use static_type::StaticType;

struct Stack;

trait FluxValue {
    fn static_type() -> StaticType;

    fn store(self, stack: &mut Stack);
    fn load(stack: &mut Stack) -> Self;
}

impl FluxValue for i32 {
    fn static_type() -> StaticType {
        todo!("i32");
    }

    fn store(self, stack: &mut Stack) {
        todo!("store");
    }
    fn load(stack: &mut Stack) -> Self {
        todo!("load");
    }
}

trait FluxFunction {
    fn arg_type() -> StaticType;
    fn ret_type() -> StaticType;

    fn call(stack: &mut Stack);
}

fn neg(value: i32) -> i32 {
    -value
}

// generated from attributes

struct Neg;

impl FluxFunction for Neg {
    fn arg_type() -> StaticType {
        todo!("i32")
    }

    fn ret_type() -> StaticType {
        todo!("i32");
    }

    fn call(stack: &mut Stack) {
        let (a, b) = FluxValue::load(stack);
        add(a, b).store(stack);
    }
}
