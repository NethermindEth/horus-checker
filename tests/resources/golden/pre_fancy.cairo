// @pre token == 0 or token == 1
// @post (token == 0 and $Return.t == 1) or (token == 1 and $Return.t == 0)
func get_opposite_token(token: felt) -> (t: felt) {
  if (token == 0) {
      return (t=1);
  } else {
      return (t=0);
  }
}

// @pre token == 0 or token == 1
// @post $Return.res == token
func bar(token) -> (res: felt) {
let (a) = get_opposite_token(token);
let (b) = get_opposite_token(a);
return (res=b);
}