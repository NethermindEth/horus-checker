# @declare $x : felt
# @pre x == $x
# @post $Return.res == $x
func inc0(x : felt) -> (res : felt):
     alloc_locals
     local x = x + 1
     return (res=x)
end

# @declare $x : felt
# @pre x == $x
# @post $Return.res == $x + 1
func inc1(x : felt) -> (res : felt):
     alloc_locals
     local x = x + 1
     return (res=x)
end
