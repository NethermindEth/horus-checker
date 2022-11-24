<div align="center">
<br />
    <img src="./static/nethermind.png" alt="Ethereum" width="80" >

<br />
  <h2 align="center">Horus</h2>
  <p align="center">
    Cairo compiler plugin extending language with annotations
    <br />
    ·
    <a href="https://github.com/NethermindEth/horus-compile/issues">Report Bug</a>
    ·
    <a href="https://github.com/NethermindEth/horus-compile/issues">Request Feature</a>
  </p>
</div>

<br>

# Introduction

Horus is a formal verification tool based on SMT checkers which allows users to annotate their StarkNet contracts with assertions and therafter verify that they hold by discharging them to an SMT solver (or multiple ones).

<br>

# Pre-requisites

In order to utilise Horus you will require several software dependencies such as:

- Python 3.7x
- Stack (Haskell build tool)
- Poetry (Python package and depedency manager)

In addition to the above, you will need to ensure that you have the following SMT solvers installed on your machine:

- z3 (version 4.10.2)
- mathsat (version 5.6.8)

> Note: If your machine has x86 or x64 architecture you can install these solvers by running the install scripts located in `horus-check/scripts/ci/*` else if you are using an M1/M2 Macbook running on arm64, follow [these instructions](#m1-m2).

<br>

# Setting up `horus-compile`

Firstly, clone the `horus-compile` and `horus-checker` repositories to your machine.

<br>

The next step involved is setting up `horus-compile` to be used across all the Horus repositories. This requires setting up a Python virtual environment and installing the compiler dependencies, thereafter you should be able to access the `horus-compile` commandline utility from anywhere on your machine (specifically we will want to make use of this once we want to start running compiled code through the SMT checkers for formal verification assertions).

<br>

To get started with `horus-compile` setup, create a virtual environment and activate it:

```bash
# create virtual environment <venv-name>
python -m venv ~/path/to/env-dir/<venv-name>
# use virtual environment
source ~/path/to/env-dir/<venv-name>/bin/activate
```

<br>

While being at the root directory of the `horus-compile` , make sure you are in the virtual environment and install the Python dependencies using `poetry`:

```bash
poetry install
```

You can use `poetry install` to install the required dependencies into your virtual environment.

<br>

At this point, the `horus-compile` command-line utility should be ready for use, to compile annotated Cairo code into files that the `horus-checker` will be able to use later.

You can utilise `horus-compile` to compile your specified Cairo code which may include the additional annotation standard (specify `--output` flag followed by JSON destination to specify where to save the generated ABI):

```bash
horus-compile <path_to_cairo_file> --output  <path_to_json_file_to_create>
```

<br>

# Setting up `horus-checker`

Go to the `horus-checker` directory and make sure you are in the virtual environment with which you installed the dependencies for `horus-compile`. You can quickly check if horus-compile can be called from this directory by calling the same command from above pointing at an specific Cairo file you wish to comiple:

```bash
horus-compile <path_to_cairo_file> --output  <path_to_json_file_to_create>
```

<br>

If the above worked, you can proceed now with setting up `horus-checker` and installing required Haskell dependencies (you should have `stack` installed for Haskell use). Install the dependencies with the following command:

```bash
stack build
```

<br>

If the above command was executed without error, then you are finished with the initial setup and are now ready to work with Horus

<br>

# Using `horus-check`

In the `horus-checker` directory, you should now be able to use the Horus checker after installing the Haskell dependencies using `stack`.

In order to use the Horus checker you would need to have used `horus-compile` to generate a JSON file including the compiled code and other attributes required by the checker.

<br>

Thereafter you can point to the specific JSON file that you would like to run the Horus checker over, you will also need to use the `-s` flag to specify which SMT solvers you would like to use for the testing:

<br>

```
stack exec horus-check -- ./<path-to-file>/example.json -s z3
```

<br>

> You can also call the Horus checker with multiple SMT solvers, below you can see the same example but with all the solver options added after the `-s` flag:

```bash
stack exec horus-check -- ./<path-to-file>/example.json -s z3 mathsat cvc5
```

<br>

### Horus Checker options

The following flags are able to added with `stack exec horus-check`:

- `-v` (verbose) = If the flag is set all the intermediate steps are printed out.
- `-output-queries` = Stores the (unoptimized) SMT queries for each module in .smt2 files inside DIR.
- `output-optimized-queries` = Stores the (optimized) SMT queries for each module in .smt2 files inside DIR.
- `print-models` = Print models for SAT results.
- `-t` (timeout) = Time limit (ms) for the smt solver.

<br>

---

<h2 id="m1-m2">M1/M2 Macbook Setup</h2>

These setup instructions assume that you have [Homebrew](https://brew.sh/) installed on your machine.

### 1. Mathsat Setup

Follow this [link](https://mathsat.fbk.eu/download.html) to get to the downloads page of the MathSAT SMT solver. Take the binary file in the bin folder (should be named `mathsat`) and copy it to `/usr/local/bin` directory.
The MathSAT solver should now be ready for use.

### 2. z3 Setup

This setup is going to require that you have [Homebrew](https://brew.sh/) installed.

What we need to do is install a specific version of z3 compatible with the Horus checker (specifically version `4.10.2`). Homebrew unfortunately makes this process a bit more difficult but if you follow the commands below, the setup of the z3 solver will be successful.

```bash
# created random tap
brew tap-new horus/z3-horus

# extracted version 4.10.2 of z3 and applied to my tap
brew extract --version 4.10.2 z3 horus/z3-horus

# install this z3 version from my tap
brew install horus/z3-horus/z3@4.10.2
```

> You could technically make the name of your tap anything you wish, an example was just added for demonstration purposes and will work if you wish to just copy the instructions into the command line.

### Writing specs

<br/>

# Annotations

Horus works using an annotation system similar to Cairo itself, however Horus annotations are written in comments. For example:

```
# @post $Result.res == 3
func example() -> (res):
	return (3)
end
```
The following annotations are supported.

<br/>

### `@post`
Specifies conditions that must be true if the function returns.

Example:
```cairo
# @post $Return.res < 100 && $Return.res >= 50
```
No claim is made about whether the function completes or reverts, but that if it completes then the postcondition holds.

<br/>

### `@pre`
Restricts the initial state, value of logical variables, or set of possible inputs for which the postcondition must hold.

Example:
```cairo
# @pre flag * (flag - 1) == 0
```

<br/>

### `@declare`
Allows the introduction of logical variables.

Example:
```cairo
# @declare $x : felt
```
Logical variable names must begin with a `$`. Note that if a logical variable is not mentioned in the precondition, then the spec must hold for all possible values of that variable.

<br/>

### `@storage_update`
Allows claims to be made about the state of a storage variable before and after the function.

Example:
```cairo
# @storage_update x() := x() + 1
```
A storage update must be included for all storage variables modified by a function otherwise it will not meet the spec.

Only the top-level storage variable reference on the left hand side refers to the state after the function. As such, if `x` took one input and we specified the update as such `x(y()) := x(y()) + 1`, both instances of `y()` refer to the state before the function was called. If you would like to make claims about the relationship between multiple storage variables after the function is complete, this can be achieved via the use of logical variables. To do so, equate your 'before' logical variable to the storage variable in the precondition. Then, also in the precondition, relate the 'after' and 'before' logical variables.  Finally assign the 'after' logical variable to the storage variable in a storage update annotation.

<br/>

### `@invariant`
Introduces a constraint attached to a label, typically used for loop invariants.

Example:
```cairo
# @invariant i <= 10
```
The invariant annotation is only required in the case of low level loops implemented with jump instructions, however it can also be used to make claims that must hold at any specific point in a function by adding an appropriately named label and attaching the annotation to it.  Note that this effectively splits the function in two, and that anything from before the invariant that is not mentioned within it cannot be used after.

# Spec syntax

The following are allowed within logical formula:
* `a`, `$a` cairo references and logical variables can be used by name
* `$Return.a` the special logical variable `$Return` is defined to contain the values returned from the function
* `a+b`, `a==b`, erc arithmetic operations and comparisons are supported for felts as in cairo
* `a==b or c==d`, `a==b and c==d`, `! a==b`, `a==b -> c==d` propositional logic operators are written as such
* `True`, `False` are defined as keywords
