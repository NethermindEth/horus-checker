# @pre True
# @post res == x + 1
func succ(x) -> (res):
    return (res=x + 1)
end

# @pre True
# @post res == x - 1
func dec(x) -> (res):
    return (res=x - 1)
end

# @pre True
# @post sum == x + y
func peano_sum(x,y) -> (sum):
    if y == 0:
     	return (sum=x)
    else:
	    let (x1)  = succ(x)
	    let (y1)  = dec(y)
	    let (res) = peano_sum(x1,y1)
        return (sum=res)
     end
end
