import Cli
import Benchmark.Analysis
import Benchmark.Wheels
import Benchmark.PrettyPrinters

namespace Benchmark

open Cli

def runCmdOptions (p : Parsed) : IO UInt32 := do
  return 0

def cmdOptions : Cmd := `[Cli|
  cmdOptions VIA runCmdOptions; ["0.0.1"]
  "Runner/benchmarker for CVC5 / Yices2 / Z3."

  FLAGS:
    file : String; "Path to *.txt file containing SMT output."
    smt  : String; "Which SMT."
]

open SMT in
instance : ParseableType SMT where
  name := "smt"
  parse? smt := 
    match smt with
      | "cvc" => some Cvc
      | "yices" => Yices
      | "Z3" => Z3
      | "MathS" => MathS
      | _ => none

end Benchmark

open Benchmark Option SMT Parser

def main (args : List String) : IO Unit := do
  if (← cmdOptions.validate args) ≠ 0 then panic! "Invalid arguments. Terminating."
  if let .ok ⟨_, data⟩ := cmdOptions.parse args then

  let smt := Cli.Parsed.Flag.as! (data.flag! "smt") SMT 
  let analysisFile := Cli.Parsed.Flag.as! (data.flag! "file") String 
  let smtResultsRaw : String ← IO.FS.readFile ⟨analysisFile⟩

  if let some (smtRes, stats, smt) := parse smt smtResultsRaw then
    let currentFilters := applyFilters #[not ∘ isZero, isTotalTime]
    let (smtRes, stats, smt) := (smtRes, currentFilters stats, smt)
    -- This is a minor hack to report timings for the time being.
    -- Z3 occasionally doesn't give time, make that 'too fast', aka. 0.0s.
    let time := stats.get! 1 |>.getD 0 <| StatEntry.Z3Stat Z3TotalRuntimeName <| Z3.EntryVal.FloatVal 0.0
    IO.FS.writeFile s!"{analysisFile}" s!"{smtRes}\n{time}\n{stats}"
  else
    IO.FS.writeFile s!"{analysisFile}" ""

  IO.Process.exit 0