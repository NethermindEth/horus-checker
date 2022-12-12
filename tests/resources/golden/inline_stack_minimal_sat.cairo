%lang starknet

struct Stack {
    value: felt,
    next: Stack*,
}

namespace _Stack {
    func empty() -> (stack: Stack*) {
        return (cast(0, Stack*),);
    }

    func add(stack: Stack*) -> (stack: Stack*) {
        let x = stack.value;
        let y = stack.next.value;
        return (new Stack(value=x + y, next=stack.next.next),);
    }

    func lit(stack: Stack*, i: felt) -> (stack: Stack*) {
        return (new Stack(value=i, next=stack),);
    }

    func top(stack: Stack*) -> (res: felt) {
        return (stack.value,);
    }
}

// @post [ap - 1] == 11
func main_() -> (res: felt) {
    let (stack) = _Stack.empty();
    let (stack) = _Stack.lit(stack, 5);
    let (stack) = _Stack.lit(stack, 7);
    let (stack) = _Stack.add(stack);
    let (top) = _Stack.top(stack);
    return (top,);
}
