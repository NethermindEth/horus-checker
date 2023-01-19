%lang starknet

struct Stack {
    value: felt,
    next: Stack*,
}

namespace _Stack {
    func empty() -> (stack: Stack*) {
        return (cast(0, Stack*),);
    }

    // @post $Return.stack.value == stack.value + stack.next.value
    // @post $Return.stack.next == stack.next.next
    func add(stack: Stack*) -> (stack: Stack*) {
        let x = stack.value;
        let y = stack.next.value;
        return (new Stack(value=x + y, next=stack.next.next),);
    }

    // @post $Return.stack.value == i
    // @post $Return.stack.next == stack
    func lit(stack: Stack*, i: felt) -> (stack: Stack*) {
        return (new Stack(value=i, next=stack),);
    }

    // @post $Return.res == stack.value
    func top(stack: Stack*) -> (res: felt) {
        return (stack.value,);
    }
}

// @post [ap - 1] == 11
func main_() -> (res: felt) {
    let (stack1) = _Stack.empty();
    let (stack2) = _Stack.lit(stack1, 5);
    // @assert stack2.value == 5 and stack2.next == stack1
    let (stack3) = _Stack.lit(stack2, 6);
    // @assert stack3.value == 6 and stack3.next == stack2 and stack2.value == 5
    let (stack4) = _Stack.add(stack3);
    // @assert stack4.value == 11
    let (top) = _Stack.top(stack4);
    return (top,);
}
