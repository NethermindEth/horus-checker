%lang starknet

struct Stack:
    member value : felt
    member next : Stack*
end

namespace _Stack:
    func empty() -> (stack : Stack*):
        return (cast(0, Stack*))
    end

    # @post stack_.value == stack.value + stack.next.value
    # @post stack_.next == stack.next.next
    func add(stack : Stack*) -> (stack_: Stack*):
        let x = stack.value
        let y = stack.next.value
        return (new Stack(value=x + y, next=stack.next.next))
    end

    # @post stack_.value == i
    # @post stack_.next == stack
    func lit(stack : Stack*, i : felt) -> (stack_: Stack*):
        return (new Stack(value=i, next=stack))
    end

    # @post res == stack.value
    func top(stack : Stack*) -> (res : felt):
        return (stack.value)
    end
end

# @post [ap - 1] == 11
func main_() -> (res : felt):
    let (stack) = _Stack.empty()
    let (stack) = _Stack.lit(stack, 5)
    let (stack) = _Stack.lit(stack, 6)
    let (stack) = _Stack.add(stack)
    let (top) = _Stack.top(stack)
    return (top)
end
