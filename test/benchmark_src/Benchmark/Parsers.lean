import Lean.Data.Parsec
import Lean.Data.Json.Parser

namespace Benchmark
namespace Parser

open Lean
open Lean.Parsec

def Base10 : Nat := 10

def nat : Parsec Nat := do
  let digits ← many1 <| (·.toNat - '0'.toNat) <$> digit
  pure $ Prod.fst $ digits.foldr
    (λ digit ⟨sum, magnitude⟩ => ⟨sum + digit * magnitude, magnitude * Base10⟩)
    ⟨0, 1⟩

def whole : Parsec Int :=
  (Int.ofNat <$> nat) <|> do skipChar '-'; ((λ x : Nat => -x) <$> nat)

open Lean in
def float : Parsec Float := do
  let jsonNum ← Json.Parser.num
  if jsonNum.exponent = 0 then fail "No decimal part."
  return Float.ofInt jsonNum.mantissa * 10 ^ - Float.ofNat jsonNum.exponent

def sepBy1 (sep : Parsec Unit) (p : Parsec α) : Parsec (Array α) :=
  (⟨· :: ·.toList⟩) <$> p <*> many do sep; p

def sepBy (sep : Parsec Unit) (p : Parsec α) : Parsec (Array α) := 
  sepBy1 sep p <|> return #[]

inductive SMTResult where | Sat | Unsat | Unknown deriving Inhabited, Repr, DecidableEq

def SMTResultOfBool : Bool → SMTResult
  | true => SMTResult.Sat
  | false => SMTResult.Unsat

def smtResult : Parsec SMTResult :=
  do { skipString "sat"; pure SMTResult.Sat } <|>
  do { skipString "unsat"; pure SMTResult.Unsat } <|>
  do { skipString "unknown"; pure SMTResult.Unknown }

namespace Z3

def propName : Parsec String :=
  many1Chars do asciiLetter <|> pchar '-' <|> pchar '_'

inductive EntryVal :=
  | IntVal : Int → EntryVal
  | FloatVal : Float → EntryVal
deriving Repr

open EntryVal

def entry : Parsec (String × EntryVal) := do
  skipChar ':'
  let name ← propName
  ws
  let val ← attempt (FloatVal <$> float) <|> IntVal <$> whole
  return (name, val)

def Z3 : Parsec (SMTResult × Array (String × EntryVal)) := do
  let smtResult ← smtResult
  ws
  skipChar '('
  let entries ← many (entry <* (ws <|> skipChar ')'))
  return (smtResult, entries)

end Z3

namespace Cvc

inductive EntryVal :=
  | IntVal : Int → EntryVal
  | NoClue : String → EntryVal -- Only ever seen -nan, but are these really floats?
  | IntWithUnit : Int → EntryVal
  | Set : Array (String × Int) → EntryVal
  | FileName : String → EntryVal
deriving Repr

open EntryVal

def propName : Parsec String := do
  skipChar '"'
  let name ← many1Chars do asciiLetter <|> digit <|> pchar ':' <|> pchar '_' <|> pchar '-' <|> pchar ' '
  skipChar '"'
  pure name

def file : Parsec String := do
  let fileName ← many1Chars (asciiLetter <|> digit <|> pchar '_' <|> pchar '/')
  skipChar '.'
  let fileExt ← many1Chars (asciiLetter <|> digit)
  return s!"{fileName}.{fileExt}"

def setValue : Parsec (String × Int) := do
  let name ← many1Chars (asciiLetter <|> digit <|> pchar '_' <|> pchar ' ')
  skipChar ':'
  ws
  let value ← whole
  return (name, value)

def set : Parsec (Array (String × Int)) := do
  skipChar '{'
  ws
  let set ← sepBy (skipChar ',' <* ws) setValue
  ws
  skipChar '}'
  return set

def value : Parsec EntryVal :=
  attempt (NoClue      <$> pstring "-nan")           <|> -- Only ever seen -nan, but are these really floats?
  attempt (IntWithUnit <$> whole <* skipString "ms") <|>
  attempt (IntVal      <$> whole                   ) <|>
  attempt (Set         <$> set                     ) <|>
           FileName    <$> file

def entry : Parsec (String × EntryVal) := do
  skipChar '('
  let name ← propName
  ws
  let value ← value
  skipChar ')'
  return (name, value)

def CVC : Parsec (SMTResult × Array (String × EntryVal)) := do
  let smtResult ← smtResult
  ws
  skipString "(:all-statistics"
  ws
  skipChar '('
  let entries ← many (entry <* (ws <|> skipChar ')'))
  skipChar ')'
  return (smtResult, entries)

end Cvc

namespace Yices

def propName : Parsec String :=
  many1Chars do asciiLetter <|> pchar ':' <|> pchar '_' <|> pchar '.' <|> pchar '-'

inductive EntryVal :=
  | IntVal : Int → EntryVal
  | FloatVal : Float → EntryVal
deriving Repr

open EntryVal

def entry : Parsec (String × EntryVal) := do
  skipChar ':'
  let name ← propName
  ws
  let val ← attempt (FloatVal <$> float) <|> IntVal <$> whole
  return (name, val)

def yices : Parsec (SMTResult × Array (String × EntryVal)) := do
  let smtResult ← smtResult
  ws
  skipChar '('
  ws
  let entries ← many (entry <* (ws <|> skipChar ')'))
  return (smtResult, entries)

end Yices

namespace MathS

def propName : Parsec String :=
  many1Chars do asciiLetter <|> pchar '-' <|> pchar '_' <|> pchar '+'

inductive EntryVal :=
  | IntVal : Int → EntryVal
  | FloatVal : Float → EntryVal
deriving Repr

open EntryVal

def entry : Parsec (String × EntryVal) := do
  skipChar ':'
  let name ← propName
  ws
  let val ← attempt (FloatVal <$> float) <|> IntVal <$> whole
  return (name, val)

def MathS : Parsec (SMTResult × Array (String × EntryVal)) := do
  let smtResult ← smtResult
  ws
  skipChar '('
  ws
  let entries ← many (entry <* ws)
  skipChar ')'
  return (smtResult, entries)

end MathS

def ofSuccess [Inhabited α] (res : ParseResult α) : α :=
  match res with
    | .error it err => panic! s!"Failed to parse. Error: {err} at: {it.i}."
    | .success _ x  => x

end Parser
end Benchmark