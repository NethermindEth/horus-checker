import Benchmark.Parsers

namespace Benchmark

def Parser.SMTResult.toString : Parser.SMTResult → String
  | Sat => "sat"
  | Unsat => "unsat"
  | Unknown => "unknown"

instance : ToString Parser.SMTResult := ⟨Parser.SMTResult.toString⟩

def Parser.Z3.EntryVal.toString : Parser.Z3.EntryVal → String
  | IntVal x | FloatVal x => s!"{x}"

instance : ToString Parser.Z3.EntryVal := ⟨Parser.Z3.EntryVal.toString⟩

def Parser.Cvc.EntryVal.toString : Parser.Cvc.EntryVal → String
  | IntVal a      => s!"{a}"
  | NoClue s      => s
  | IntWithUnit a => s!"{Float.ofInt a / 1000.0}" -- These are always ms.
  | Set attribs   => attribs.foldl (init := "") λ acc ⟨attrName, attrVal⟩ => s!"{acc} | {attrName} {attrVal}"
  | FileName s    => s

instance : ToString Parser.Cvc.EntryVal := ⟨Parser.Cvc.EntryVal.toString⟩

def Parser.Yices.EntryVal.toString : Parser.Yices.EntryVal → String
  | IntVal x | FloatVal x => s!"{x}"

instance : ToString Parser.Yices.EntryVal := ⟨Parser.Yices.EntryVal.toString⟩

def Parser.MathS.EntryVal.toString : Parser.MathS.EntryVal → String
  | IntVal x | FloatVal x => s!"{x}"

instance : ToString Parser.MathS.EntryVal := ⟨Parser.MathS.EntryVal.toString⟩

end Benchmark