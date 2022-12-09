func main() {
    let balanceA = 5;
    let balanceB = 0;
    let (balanceA, balanceB) = transfer(6, balanceA, balanceB);
    return ();
}

# @pre balanceA >= howMuch
# @post $Return.balanceA == balanceA - howMuch
# @post $Return.balanceB == balanceB + howMuch
func transfer(howMuch, balanceA, balanceB) -> (balanceA, balanceB):
    return (balanceA - howMuch, balanceB + howMuch)
end
