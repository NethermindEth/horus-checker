import Benchmark.Parsers
import Benchmark.PrettyPrinters
import Benchmark.Wheels

namespace Benchmark

inductive SMT where | Cvc | Yices | Z3 | MathS deriving Inhabited

open SMT

def SMT.toString : SMT → String
  | Cvc   => "cvc"
  | Yices => "yices"
  | Z3    => "Z3"
  | MathS => "MathS"

def SMTs := #[Cvc, Yices, Z3, MathS]

def NumSMTs : Nat := SMTs.size

def SMT.toNat : SMT → Nat
  | Cvc   => 0
  | Yices => 1
  | Z3    => 2
  | MathS => 3

def SMT.ofNat : Nat → SMT
  | 0 => Cvc
  | 1 => Yices
  | 2 => Z3
  | _ => MathS

instance : ToString SMT := ⟨SMT.toString⟩
instance : Coe SMT String := ⟨SMT.toString⟩
instance : Coe SMT Nat := ⟨SMT.toNat⟩
open Parser

inductive StatEntry :=
  | CvcStat   : String → Cvc.EntryVal   → StatEntry
  | YicesStat : String → Yices.EntryVal → StatEntry
  | Z3Stat    : String → Z3.EntryVal    → StatEntry
  | MathSStat : String → MathS.EntryVal → StatEntry

open StatEntry

instance : Inhabited StatEntry := ⟨CvcStat "" <| Cvc.EntryVal.IntVal 0⟩

def StatEntry.toString : StatEntry → String
  | CvcStat str val | YicesStat str val | Z3Stat str val | MathSStat str val => s!"{str}: {val}"

instance : ToString StatEntry := ⟨StatEntry.toString⟩

def statEntryOfCvcEntry : String × Parser.Cvc.EntryVal → StatEntry
  | ⟨name, val⟩ => CvcStat name val

def statEntryOfYicesEntry : String × Parser.Yices.EntryVal → StatEntry
  | ⟨name, val⟩ => YicesStat name val

def statEntryOfZ3Entry : String × Parser.Z3.EntryVal → StatEntry
  | ⟨name, val⟩ => Z3Stat name val

def statEntryOfMathSEntry : String × Parser.MathS.EntryVal → StatEntry
  | ⟨name, val⟩ => MathSStat name val

open Lean Parser SMTResult SMT in
def parse (smt : SMT) (data : String) : Option (SMTResult × Array StatEntry × SMT) :=
  if data.isEmpty then none else
  let liftParser {α} (f : Parsec (_ × Array (_ × α)))
                     (g : String × α → _)
                        : Parsec (SMTResult × Array StatEntry) :=
    f >>= λ (x, y) => pure (x, y.map g)
  let parser : Parsec (SMTResult × Array StatEntry) := 
    match smt with
      | Cvc   => liftParser Cvc.CVC     statEntryOfCvcEntry
      | Yices => liftParser Yices.yices statEntryOfYicesEntry
      | Z3    => liftParser Z3.Z3       statEntryOfZ3Entry
      | MathS => liftParser MathS.MathS statEntryOfMathSEntry
  match parser data.iter with
    | .error it err => panic! s!"Failed to parse. Error: {err} at: {it.i}. SMT[{smt}] response: {data}"
    | .success _ (smtResult, stats) => some (smtResult, stats, smt)

def isZero : StatEntry → Bool
  | CvcStat   _ (Cvc.EntryVal.IntVal      a) => a = 0
  | CvcStat   _ (Cvc.EntryVal.NoClue      x) => x = "-nan"
  | CvcStat   _ (Cvc.EntryVal.IntWithUnit a) => a = 0
  | CvcStat   _ (Cvc.EntryVal.Set       arr) => arr.size = 0
  | CvcStat   _ (Cvc.EntryVal.FileName    s) => s = "" -- TODO: This is a guess. Only seen valid files.
  | YicesStat _ (Yices.EntryVal.IntVal    a)
  | Z3Stat    _ (Z3.EntryVal.IntVal       a)
  | MathSStat _ (MathS.EntryVal.IntVal    a) => a = 0
  | YicesStat _ (Yices.EntryVal.FloatVal  x)  
  | Z3Stat    _ (Z3.EntryVal.FloatVal     x)
  | MathSStat _ (MathS.EntryVal.FloatVal  x) => x.isZeroish

def CvcTotalRuntimeName   := "global::totalTime"
def YicesTotalRuntimeName := "total-run-time"
def Z3TotalRuntimeName    := "time"
def MathSRuntimeName      := "time-seconds"

def isTotalTime : StatEntry → Bool
  | CvcStat   name (Cvc.EntryVal.IntWithUnit _) => name = CvcTotalRuntimeName
  | YicesStat name (Yices.EntryVal.FloatVal  _) => name = YicesTotalRuntimeName
  | Z3Stat    name (Z3.EntryVal.FloatVal     _) => name = Z3TotalRuntimeName
  | MathSStat name (MathS.EntryVal.FloatVal  _) => name = MathSRuntimeName
  | _                                           => false

def applyFilters (statFs : Array (StatEntry → Bool)) (stats : Array StatEntry) : Array (Array StatEntry) :=
  statFs.foldl (·.push <| stats.filter ·) #[]

end Benchmark