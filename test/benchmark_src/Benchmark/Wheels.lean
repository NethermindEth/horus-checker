namespace Benchmark

inductive Vector (α : Type u) : Nat → Type u where
  | Nil : Vector α 0
  | Cons (head : α) (tail : Vector α n) : Vector α (n + 1)

open Vector

def Vector.get : Vector α n → Fin n → α 
  | Cons hd tl, ⟨0, _⟩ => hd
  | Cons hd tl, ⟨k + 1, h⟩ => get tl ⟨k, Nat.lt_of_succ_lt_succ h⟩

def Vector.map (f : α → β) : Vector α n → Vector β n
  | Nil => Nil
  | Cons hd tl => Cons (f hd) <| map f tl

syntax "$[" term,* "]" : term

open Lean in
macro_rules
  | `($[ $elems,* ]) => do
    let rec expandListLit (i : Nat) (skip : Bool) (result : Syntax) : MacroM Syntax := do
      match i, skip with
      | 0,     _     => pure result
      | i + 1, true  => expandListLit i false result
      | i + 1, false => expandListLit i true  (← ``(Vector.Cons $(elems.elemsAndSeps[i]) $result))
    expandListLit elems.elemsAndSeps.size false (← ``(Vector.Nil))

def ofExcept! [Inhabited α] (x? : Except IO.Error α) : α :=
  match x? with
    | .ok x => x
    | .error err => panic! s!"{err} - IMPORTANT: If you are seeing this error, check your environment!"

def fileName! (name : String) : String :=
  if let some name := System.FilePath.fileName ⟨name⟩ then name else panic! "Invalid filename."

def uncurry (f : α → β → γ) : (α × β) → γ := λ (a, b) => f a b

namespace Float

-- TODO: Replace with a proper version if this ever becomes an issue.
-- ε should be derived from the width of the Float representation
-- and ε closeness to zero should be derived from therefrom further.
-- I do not suppose we need this sort of precision though, so for now,
-- this will have to do.

def ε : Float := 0.000001

def abs (x : Float) : Float := if x.toString.startsWith "-" then -x else x

def withinε (x₁ x₂ : Float) : Bool := abs (x₂ - x₁) ≤ ε

end Float

end Benchmark

def Float.isZeroish (x : Float) : Bool := Benchmark.Float.withinε x 0.0

-- Needless to say, this is a toy version.
def String.isSubstringOf (s₁ : String) : String → Bool
  | ⟨[]⟩ => s₁ = ""
  | s@⟨hd :: tl⟩ => if s₁.isPrefixOf s then true else isSubstringOf s₁ ⟨tl⟩

def Option.maybe (x? : Option α) (default : α) : α :=
  match x? with
    | some x => x
    | none => default