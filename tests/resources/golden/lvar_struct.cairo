struct MyStruct:
       member x : felt
       member y : felt
end

# @declare $s : MyStruct
# @pre s == $s
# @post $Return.res == $s.x + $s.y
func elemSum0(s : MyStruct) -> (res):
     return (res=s.x+s.y)
end

# @declare $s : MyStruct
# @pre s == $s
# @post $Return.res == $s.x + $s.y + 1
func elemSum1(s : MyStruct) -> (res):
     return (res=s.x+s.y)
end
