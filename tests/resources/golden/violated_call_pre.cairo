func main():
    let balanceA = 5
    let balanceB = 0
    let (balanceA, balanceB) = transfer(6, balanceA, balanceB)
    return ()
end

# @pre balanceA >= howMuch
# @post [ap - 2] == balanceA - howMuch
# @post [ap - 1] == balanceB + howMuch
func transfer(howMuch, balanceA, balanceB) -> (balanceA_, balanceB_):
    return (balanceA - howMuch, balanceB + howMuch)
end