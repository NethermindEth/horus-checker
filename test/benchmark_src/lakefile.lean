import Lake
open Lake DSL

package benchmark {
  dependencies := #[
    {
      name := `Cli
      src := Source.git "https://github.com/mhuisi/lean4-cli.git" "main"
    }
  ]
}
