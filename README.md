<div align="center">
<br />
    <img src="./nethermind.png" alt="Ethereum" width="80" >

<br />
  <h2 align="center">Horus</h2>
  <p align="center">
    Formal verification of <a href="https://www.cairo-lang.org/">Cairo</a> programs with language annotations
    <br />
    <a href="https://github.com/NethermindEth/horus-checker/issues">Report bug</a>
    ·
    <a href="https://github.com/NethermindEth/horus-checker/issues">Request feature</a>
  </p>
</div>

<br>

## Introduction

Horus is a command-line formal verification tool for annotating
[StarkNet](https://starkware.co/starknet/) contracts with assertions and
verifying that they hold using one or more SMT (satisfiability modulo theory)
solvers.

> **Note.** Horus is currently in alpha, and thus should not be fully trusted yet!

### Our documentation

* [**Installation**](#installation) - Get the `horus-compile` and
  `horus-check` executables setup for your development environment.
* [Tutorial: Your first verified Cairo program](#tutorial-your-first-verified-cairo-program) - Try this if you're
  new to Horus and want to get a taste of verification in Cairo.  This will
  walk you through an example step-by-step.
* [FAQ](#faq) - What is Cairo? What is Horus? When should I use Horus? Why should
  I use Horus? All these answered and more!
* [Usage](#usage) - Exhastive reference information on the CLI options, among
  other things.
* [Internals](#internals) - In which we explain why things are implemented the
  way they are, and discuss details relevant to the development of Horus. This
  is prose to aid contributors and onboard new team members.

## Installation

Horus is supported on Linux and MacOS (including AArch64 Macs)!

We also have instructions for [installing with
Docker](#quick-installation-using-docker) if you want to run Horus in a
container.

### Prerequisites

- [Python 3.7](https://www.python.org/downloads/release/python-379/)
- [Stack](https://docs.haskellstack.org/en/stable/) (Haskell build tool)
- [Poetry](https://python-poetry.org/) (Python package and dependency manager)
- [Z3](https://github.com/Z3Prover/z3) (version 4.10.2)
- [MathSAT](https://mathsat.fbk.eu/) (version 5.6.8)

### Installing Python 3.7

Check your python version:
```console
python3 --version
```
<sub>Expected output:</sub>
```
Python 3.7.15
```

If you see `3.7` as in above (any variant of 3.7 should be okay), **you can
skip head to [installing stack](#installing-stack).**

Otherwise, you may have a different version, or you may not have python
installed at all. Follow the instructions below to install the needed version.


1.  Download and install
  [`miniconda`](https://docs.conda.io/en/latest/miniconda.html) on your
  system:

  * [Linux 64-bit](https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh)
  * [macOS Intel x86 64-bit](https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh)
  * [macOS Apple M1 64-bit](https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh)
  * [Windows](https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe)

  In each case, run the downloaded script/executable, and follow the instructions.

2.  Create a conda environment with python 3.7:

  ```console
  conda create -n horus-py37 python=3.7
  ```
  In the above, `horus-py37` is just a name we've chosen for this environment.

3.  Activate the created environmment:

  ```console
  conda activate horus-py37
  ```

4.  Verify that you're running 3.7:
  ```console
  python3 --version
  ```
  <sub>Expected output:</sub>
  ```
  Python 3.7.15
  ```

### Installing the Haskell tool stack

###### On Linux:
```console
curl -sSL https://get.haskellstack.org/ | sh
```

###### On macOS:
```console
brew install stack
```

> These setup instructions assume that you have [Homebrew](https://brew.sh/) installed on your Mac.

<br>


Check that the install was successful:
```console
stack --version
```

<sub>Expected output (something like this):</sub>
```
Version 2.7.5, Git revision ba147e6f59b2da75b1beb98b1888cce97f7032b1 x86_64 hpack-0.34.4
```

### Install poetry

```console
pip3 install poetry
```

### Clone repositories

Clone the `horus-compile` and `horus-checker` repositories to your machine.

```console
git clone git@github.com:NethermindEth/horus-compile.git
git clone git@github.com:NethermindEth/horus-checker.git
```

### Install SMT solvers

Navigate to the `horus-checker/` repository root.

```console
cd horus-checker/
```

###### On Linux:
```console
# Inside the `horus-checker/` repository root.
sh ./scripts/ci/install-z3-linux-amd64.sh
sh ./scripts/ci/install-mathsat-linux.sh
sh ./scripts/ci/install-cvc5-linux.sh
```

###### On macOS:
```console
# Inside the `horus-checker/` repository root.
sh ./scripts/ci/install-z3-macos.sh
sh ./scripts/ci/install-mathsat-macos.sh
sh ./scripts/ci/install-cvc5-macos.sh
```

###### On macOS (`aarch64`):
```console
# Inside the `horus-checker/` repository root.
sh ./scripts/ci/install-z3-macos.sh
sh ./scripts/ci/install-mathsat-macos.sh
sh ./scripts/ci/install-cvc5-macos-aarch64.sh
```

### Create a python virtual environment

If you're using `conda`, you can skip to [install `horus-compile`](#install-horus-compile).

Otherwise, navigate to the `horus-compile/` repository root and run the following commands:

```console
python -m venv .venv/horus
source .venv/horus/bin/activate
```
<sup>In the above, `horus` is just the name we chose for our virtual environment.</sup>

### Install `horus-compile`

Make sure you're inside the `horus-compile` repository root.

On Linux:
```console
pip install .
```
On macOS:
```console
CFLAGS=-I`brew --prefix gmp`/include LDFLAGS=-L`brew --prefix gmp`/lib pip install .
```

<br>

### Install `horus-checker`

Navigate to the `horus-checker` repository root and run:
```console
stack install
```

> **Note.** Stack installs executables to `~/.local/bin` by default. Make sure
> this directory is on your `PATH` or pass a different install directory with
> `stack install --local-bin-path <dir>`.

You can check that the install was successful with:
```console
horus-check --help
```

If the above command executed without error, you are ready to use Horus!

### Quick installation using Docker

Follow these instructions if you prefer to install Horus in a docker container.
Refer to the [Docker documentation](https://docs.docker.com/) for more
information on working with containers.

#### 1. Clone repositories

Clone the `horus-compile` and `horus-checker` repositories to your machine.

```console
git clone git@github.com:NethermindEth/horus-checker.git
cd horus-checker
git clone git@github.com:NethermindEth/horus-compile.git
```

#### 2. Build docker image

```console
sudo docker build . -t horus
```

This process might take several minutes.

> **Note.** If your local platform is an AArch64 Mac, this process will take even longer
> because some dependencies will be compiled from source. For us, it took 38min in a MacBook Pro M1.

At the end you should see a message like this:

```console
Successfully built e48336cb10ae
Successfully tagged horus:latest
```

#### 3. Prepare a project directory

To access files on the host (your machine) from inside a docker container, you
must mount some extant directory. This makes the directory readable and
writable from both the host and the container.

Here, we'll create an example project directory named `my-cairo-project` for
this purpose.
```console
mkdir my-cairo-project/
```

See the docker documentation section on [bind
mounts](https://docs.docker.com/storage/bind-mounts/) for more information.

#### 4. Start docker container

The following command will:
1. Run the container
2. Mount the directory `my-cairo-project/` on your filesystem to the location
   `/home/` on the container's filesystem
3. Drop you into a root shell inside the container
```console
sudo docker run -v $(pwd)/my-cairo-project/:/home/ -it horus:latest /bin/bash
```

#### 5. Check `horus-compile` and `horus-check` installation.

Inside the docker container, run the following commands:

```console
horus-compile --help
```

```console
horus-check --help
```

If these commands execute without error, you're ready to use Horus!


<br>

## Tutorial: Your first verified Cairo program

Let's verify a Cairo program! First, we'll write a simple program that
implements a stack data structure in Cairo. If you're unfamiliar with Cairo, or
you need a refresher, check out the
[documentation](https://www.cairo-lang.org/docs/).

To follow along, the full example program without annotations is provided in
[`example.cairo`](https://github.com/NethermindEth/horus-checker/blob/langfield/documentation/example.cairo).

#### Define a struct called `Stack`

Let's define a [`struct`](https://www.cairo-lang.org/docs/reference/syntax.html#structs)
called `Stack` with two members:
* one called `value` which has type [`felt`](https://www.cairo-lang.org/docs/hello_cairo/intro.html#field-element),
* one called `next` of type `Stack*`, which means it's a pointer to an instance
of the struct `Stack` that we're currently defining.

```cairo
struct Stack {
  value: felt,
  next: Stack*,
}
```

Intuitively, we are representing our stack as a linked list. The `value` is the
head of the list, i.e. the top of the stack, and the `next` member is a pointer
to the next node in the list.

Now we've got a bare data structure. Let's define some functions that operate on it.

#### Define a function that constructs an empty `Stack`

First, we'll define a function called `empty()` that takes no arguments and
returns a pointer to a new, empty `Stack`.
```cairo
struct Stack {
  value: felt,
  next: Stack*,
}

func empty() -> (stack: Stack*) {
  return (cast(0, Stack*),);
}
```
The use of `cast()` above is a
[typed reference](https://www.cairo-lang.org/docs/how_cairo_works/consts.html?highlight=cast#typed-references).

#### Define a function that adds the top two elements on the stack

We'll also define a function called `add()`. It will take one argument, which
will be a pointer to a stack, and it will have one return value, also a pointer
to a stack.

We can use
[member accessor notation](https://www.cairo-lang.org/docs/how_cairo_works/consts.html?highlight=cast#typed-references)
to access the appropriate data from our parameter `stack`.

```cairo
struct Stack {
  value: felt,
  next: Stack*,
}

func empty() -> (stack: Stack*) {
  return (cast(0, Stack*),);
}

func add(stack: Stack*) -> (stack: Stack*) {
  let x = stack.value;
  let y = stack.next.value;
  return (new Stack(value=x + y, next=stack.next.next),);
}
```

We use the
[`new` operator](https://www.cairo-lang.org/docs/how_cairo_works/object_allocation.html?highlight=new#the-new-operator)
which creates a specified object, in this case a `Stack`, in the
[execution segment](https://www.cairo-lang.org/docs/how_cairo_works/segments.html#the-program-and-execution-segments)
of our program. It then returns a pointer to the newly created object. In this
case, it returns a pointer to a `Stack`, and that makes sense, since our return
value is `stack: Stack*`!

#### Define a function that pushes values onto the stack

Next, we'll define a function called `lit()` for pushing values onto the stack.

By convention, `lit` stands for "literal", since we're pushing "literal"
values. This is a naming tradition that originates from implementations of
stack machines and stack-based languages:
> *LIT is the primitive word for pushing a "literal" number onto the data stack. -[Wikipedia page for Forth](https://en.wikipedia.org/wiki/Forth_(programming_language))*

Our function will take two arguments, a pointer to a stack, and a literal value
`i`, which has type
[`felt`](https://www.cairo-lang.org/docs/hello_cairo/intro.html#field-element).
It will return a pointer to a stack to which the literal `i` has been pushed,
and is now the top element. Our function will leave the rest of the stack
unmodified.

```cairo
struct Stack {
  value: felt,
  next: Stack*,
}

func empty() -> (stack: Stack*) {
  return (cast(0, Stack*),);
}

func add(stack: Stack*) -> (stack: Stack*) {
  let x = stack.value;
  let y = stack.next.value;
  return (new Stack(value=x + y, next=stack.next.next),);
}

func lit(stack: Stack*, i: felt) -> (stack: Stack*) {
  return (new Stack(value=i, next=stack),);
}
```

#### Define a function to peek the top value of the stack

And finally, we'll define a function `top()` which simply returns the top value
on the stack without modifying the stack.
```cairo
struct Stack {
  value: felt,
  next: Stack*,
}

func empty() -> (stack: Stack*) {
  return (cast(0, Stack*),);
}

func add(stack: Stack*) -> (stack: Stack*) {
  let x = stack.value;
  let y = stack.next.value;
  return (new Stack(value=x + y, next=stack.next.next),);
}

func lit(stack: Stack*, i: felt) -> (stack: Stack*) {
  return (new Stack(value=i, next=stack),);
}

func top(stack: Stack*) -> (res: felt) {
  return (stack.value,);
}
```

#### Add a namespace for our `Stack`-related functions

We can wrap all these functions up in a namespace called `_Stack` to clarify
usage:

```cairo
struct Stack {
  value: felt,
  next: Stack*,
}

namespace _Stack {
  func empty() -> (stack: Stack*) {
      return (cast(0, Stack*),);
  }

  func add(stack: Stack*) -> (stack: Stack*) {
      let x = stack.value;
      let y = stack.next.value;
      return (new Stack(value=x + y, next=stack.next.next),);
  }

  func lit(stack: Stack*, i: felt) -> (stack: Stack*) {
      return (new Stack(value=i, next=stack),);
  }

  func top(stack: Stack*) -> (res: felt) {
      return (stack.value,);
  }
}
```

This means when we call them we must write, for example, `_Stack.add` instead
of just `add`, which makes it slightly clearer what sort of objects we're
operating on, and where to find the implementation of that operation.

#### Add the necessary imports and a `main()` function

Great! Now we'll just add a short `main()` function to test that our stack
functions as we expect.

```cairo
// Import a function to perform output.
from starkware.cairo.common.serialize import serialize_word

struct Stack {
  value: felt,
  next: Stack*,
}

namespace _Stack {
  func empty() -> (stack: Stack*) {
      return (cast(0, Stack*),);
  }

  func add(stack: Stack*) -> (stack: Stack*) {
      let x = stack.value;
      let y = stack.next.value;
      return (new Stack(value=x + y, next=stack.next.next),);
  }

  func lit(stack: Stack*, i: felt) -> (stack: Stack*) {
      return (new Stack(value=i, next=stack),);
  }

  func top(stack: Stack*) -> (res: felt) {
      return (stack.value,);
  }
}

// Perform some example operations on a stack to sum two integers, and then
// print the result.
func main{output_ptr : felt*}() -> () {
  let (stack) = _Stack.empty();
  let (stack) = _Stack.lit(stack, 5);
  let (stack) = _Stack.lit(stack, 6);
  let (stack) = _Stack.add(stack);
  let (top) = _Stack.top(stack);
  serialize_word(top);
  return ();
}
```

Note that we've added a compiler directive [`%builtins output`](https://www.cairo-lang.org/docs/hello_cairo/intro.html#writing-a-main-function). The Cairo documentation explains this:

> The directive `%builtins output` instructs the Cairo compiler that our program will use the “output” builtin.

For the purposes of this example, this just allows us to print stuff.

We are then allowed to import the `serialize_word()` library function:
```cairo
from starkware.cairo.common.serialize import serialize_word
```
You can think of it like `print()`.

The main function above takes no arguments and returns no values. There is an
implicit argument `output_ptr` that we need in order to perform output. Our
example `main()` function pushes two literals to an empty stack, adds them, and
then prints the result.

#### Compile and run our example program

Let's try it out! If you've installed everything correctly, you should have the
`cairo-compile` and `cairo-run` executables on your `PATH`.

###### Check that `cairo-compile` is installed
```console
cairo-compile --version
```
<sub>Expected output:</sub>
```
cairo-compile 0.10.1
```

<br>

###### Check that `cairo-run` is installed
```console
cairo-run --version
```
<sub>Expected output:</sub>
```
cairo-run 0.10.1
```

Now let's put the source code of the program we just wrote in a file and compile it. You can
download the source code from
[here](https://raw.githubusercontent.com/NethermindEth/horus-checker/example.cairo).

Make sure your file has the name `example.cairo`, and then run the following command:
```console
cairo-compile example.cairo --output compiled.json
```

The compiler outputs a JSON file, which we name `compiled.json`. If everything
goes according to plan, you should see this file in your current working
directory.

We can run this compiled program using `cairo-run`:
```console
cairo-run --program compiled.json --layout all --print_output
```

<sub>Expected output:</sub>
```
Program output:
11

```

And we see that it correctly added the two literal values we pushed, `5` and
`6`. Fantastic!


#### Formally verify our example program

Now, let's add some annotations that describe how we expect our `_Stack`
functions to behave, and then we'll prove that the implementations we wrote
always do what the annotations say.


Here's our program with the annotations:
```cairo
%builtins output
from starkware.cairo.common.serialize import serialize_word

struct Stack {
  value: felt,
  next: Stack*,
}

namespace _Stack {
  func empty() -> (stack: Stack*) {
      return (cast(0, Stack*),);
  }

  // @post $Return.stack.value == stack.value + stack.next.value
  // @post $Return.stack.next == stack.next.next
  func add(stack: Stack*) -> (stack: Stack*) {
      let x = stack.value;
      let y = stack.next.value;
      return (new Stack(value=x + y, next=stack.next.next),);
  }

  // @post $Return.stack.value == i
  // @post $Return.stack.next == stack
  func lit(stack: Stack*, i: felt) -> (stack: Stack*) {
      return (new Stack(value=i, next=stack),);
  }

  // @post $Return.res == stack.value
  func top(stack: Stack*) -> (res: felt) {
      return (stack.value,);
  }
}

// Perform some example operations on a stack.
func main{output_ptr : felt*}() -> () {
  let (stack) = _Stack.empty();
  let (stack) = _Stack.lit(stack, 5);
  let (stack) = _Stack.lit(stack, 6);
  let (stack) = _Stack.add(stack);
  let (top) = _Stack.top(stack);

  // @assert top == 11
  serialize_word(top);
  return ();
}
```
The annotations are the comments directly above each of the functions `add()`,
`lit()`, and `top()`, along with the assert comment in `main()`. They all begin
with `// @`. The `@post` keyword indicates that an annotation is specifying a
condition that must hold **at the end of the function call**, when the function
returns. It is called `@post` because it is a "postcondition".

The `@assert` keyword is the syntax for a condition that can be checked
anywhere inside the body of a function.

Briefly, with the annotations we've added, we are checking that:
* The `add()` function returns a pointer to a stack with the sum of the first
  two elements on top, and the remainder of the original stack (the third
  element and so on) after that.
* The `lit` function puts `i` on the top of the stack, and preserves the old
  stack underneath it.
* The `top` function actually returns the top of the stack we pass as an
  argument.
* The `top` value on the stack in `main()` is actually `11` (the sum of pushed
  values `5` and `6`).

As an example, let's examine the annotations for the `_Stack.add()` function:

```cairo
// @post $Return.stack.value == stack.value + stack.next.value
// @post $Return.stack.next == stack.next.next
```

Here's what's going on:
* The `//` is just the syntax for comments in Cairo.
* The `@post` declares a Horus postcondition annotation. This condition must
  hold at the end of each function call.
* The `$Return` syntax is a way of referring to the return values of the
  function. So `$Return.stack` is the return value named `stack` of the `add()`
  function. In general, the `$` syntax is how we reference
  [logical variables](#spec-syntax).
* Both annotations are boolean expressions asserting equality of a pair of
  values, and we can use arithmetic operators like `+` in annotations for
  supported datatypes. See the section on [spec syntax](#spec-syntax) for more
  info on what operators and symbols are supported.
* It is notable that we **cannot reference locals** within preconditions or
  postconditions. So if we tried to say `@pre x == 0` for the `add` function,
  the compiler would print an error message telling us it cannot find the
  identifier `x`.

Let's compile this annotated program with Horus and then check these properties:
```console
horus-compile annotated.cairo --output compiled.json
```

This should create a file called `compiled.json`. Now let's verify the compiled binary:
```console
horus-check --solver z3 compiled.json
```

> The `--solver z3` flag tells Horus which SMT solver to use (Z3, in this case). See also the [available solvers](#usage).

<sub>Expected output:</sub>
```
_Stack.add
Verified

_Stack.lit
Verified

_Stack.top
Verified

_Stack.empty
Verified

main
Verified

```

> Note: `_Stack.empty` appears here since Horus implicitly gives all
> unannotated functions a trivial (always true) specification.

The four functions `_Stack.add`, `_Stack.lit`, `_Stack.top`, and `main` that we
annotated all say `Verified`, which means our implementations are correct with
respect to the specifications we wrote in our annotations.

Congrats! You've just formally verified your first Cairo program!


## FAQ

#### What is Horus?

Horus is a command-line tool for the [Cairo ecosystem](https://www.cairo-lang.org/).
It helps you [formally verify](https://en.wikipedia.org/wiki/Formal_verification)
Cairo programs and [StarkNet smart contracts](https://starkware.co/starknet/).

The way it works is like this:

1. You write a Cairo program.
2. You add annotations that describe how the program should operate.
3. You run Horus on your program, and Horus tells you one of the following:
    * The program obeys the annotations.
    * The program does not obey the annotations (found a counterexample).
    * Ran out of time (`Unknown`).

> “Program testing can be used to show the presence of bugs, but never to show their absence!”
> ― Edsger W. Dijkstra

Horus can be used to show the **absence** of bugs.

> **Note.** Horus is currently in **alpha**, and thus should not be fully
> trusted yet. Both Horus and SMT solvers may have bugs themselves, and so a
> given judgement is never a "certainty".

Horus consists of two command-line tools, called `horus-compile` and
`horus-check`. The first, `horus-compile`, is a modified version of the Cairo
compiler that you can use to compile a program with [Horus annotations](#annotations).
You can then run `horus-check` on the compiled program to formally verify the
program's behavior.



#### What is Cairo/StarkNet?

[**StarkNet**](https://starkware.co/starknet/) is a Layer 2 network over Ethereum. Specifically, it is a [ZK-Rollup (zero-knowledge rollup)](https://docs.ethhub.io/ethereum-roadmap/layer-2-scaling/zk-rollups/), which is basically a way of scaling up the number of transactions that a blockchain can process by bundling (rolling-up) many transactions into one.

[**Cairo**](https://www.cairo-lang.org/) is a [Turing-complete](https://en.wikipedia.org/wiki/Turing_completeness) language for writing [dApps](https://ethereum.org/en/dapps/#what-are-dapps) using [STARKs](https://docs.ethhub.io/ethereum-roadmap/layer-2-scaling/zk-starks/). STARK stands for Scalable Transparent Argument of Knowledge.

Basically, it's a programming language for
[verifiable computing](https://en.wikipedia.org/wiki/Verifiable_computing)
that runs on StarkNet. It lets you write programs where one party can prove to
another that a certain computation was executed correctly. The syntax is a
bit like [Rust](https://www.rust-lang.org/).

You can write StarkNet smart contracts in the Cairo language.



#### When should I use Horus?

Use Horus when you need to be absolutely sure that a Cairo program or StarkNet
contract executes correctly according to some specification. Horus is good for
when you know what your program should do, but you aren't sure that the
implementation actually does that thing, in all cases, no matter what. Horus
will not help you if you don't know exactly what your program should do.

Horus, and formal verification in general, proves that the implementation of a
program **matches the expected behavior**, as expressed in some formal
specification.

You get the most mileage out of this when the expected behavior is simple, but
the implementation is very complex.



#### Why should I use Horus?

Because you love formal verification and care about writing provably correct programs!

> [Really stupid “smart contract” bug let hackers steal $31 million in digital coin](https://arstechnica.com/information-technology/2021/12/hackers-drain-31-million-from-cryptocurrency-service-monox-finance/)

Alternatively, because you don't want your firm to be in the news.



#### What does Horus do?

It uses a modified version of the Cairo compiler to translate your
[function specification annotations](#annotations) into
[SMT solver](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories)
queries. These are mathematical assertions that the desired properties of the
function in question are true for all inputs. Then these queries are run, and
the SMT solver magically tells us whether or not it was able to prove that the
program is sound!



#### What things can I assert/check about a program?

You can assert things about function parameters, function return values,
arbitrary labels/variables in a function body, and storage variables. Here are
examples of things you can check about these values:
  * Two values are equal (`==`)
  * One value is less/greater than (or equal) to another (e.g. `<`, `<=`)
  * A storage variable is updated with a particular value
  * An invariant holds, e.g. in a loop

Note that arithmetic operations are, in general, defined for finite field
elements (felts) with respect to the obvious embedding of felts into the
naturals ($[x] \mapsto x \in \mathbb{N}$).

You can check these conditions hold at the start and end of a function call
with `@pre` and `@post`, respectively, and also in the middle of a function
body with `@invariant` and `@assert`.


#### What SMT solvers are available?

The `-s`/`--solver` flag is used to tell Horus which SMT solver to use.

> **Example**
> ```console
> horus-check -s cvc5 program.json
> ```
> In the above example, we use the solver named `cvc5`.

Horus supports the following solvers:
* `cvc5`
* `mathsat`
* `z3`


#### How can I refer to return values in an annotation?

You can use the `$Return` syntax to refer to elements of the return tuple by
name. This can only be done in a postcondition, i.e. a `@post` annotation. The
names must be declared in the type signature of the function, after the `->`
notation. Example:

```cairo
// @post $Return.a == 1
// @post $Return.b == 2
func f() -> (a: felt, b: felt) {
    return (a=1, b=2);
}
```

#### How can I refer to the address of the caller in an annotation?

You can use `get_caller_address()`. Here's an example:

```cairo
%lang starknet
from starkware.starknet.common.syscalls import get_caller_address

// @post $Return.res == get_caller_address()
func f{syscall_ptr: felt*}() -> (res: felt) {
    let (res) = get_caller_address();
    return (res=res);
}
```

#### How can I refer to the address of the contract itself in an annotation?

You can use `get_contract_address()`. Here's an example:

```cairo
%lang starknet
from starkware.starknet.common.syscalls import get_contract_address

// @post $Return.res == get_contract_address()
func f{syscall_ptr: felt*}() -> (res: felt) {
    let (res) = get_contract_address();
    return (res=res);
}
```

#### How can I refer to the current block timestamp in an annotation?

You can use `get_block_timestamp()`. Here's an example:

```cairo
%lang starknet
from starkware.starknet.common.syscalls import get_block_timestamp

// @post $Return.res == get_block_timestamp()
func f{syscall_ptr: felt*}() -> (res: felt) {
    let (res) = get_block_timestamp();
    return (res=res);
}
```



#### Why do extra functions/things appear in the output even though I didn't give them annotations?

Horus prints a judgement for every function it uses to verify your annotations.
If an annotated function `f` calls an unannotated function `g` (perhaps a
library function), the judgement for `g` may also be printed.

Horus also adds a trivial annotation (equivalent to `@pre True` and `@post
True`) which is **always** verifiable to all unannotated functions, and so
judgements for these will also be printed.

An `empty: (...)` module and judgement is sometimes added to the output of a
run. This indicates an empty segment.


#### Why am I getting `Verified` when I expect `False`?

One common reason this happens is contradictions in the `@pre` condition. If
you have constraints that cannot ever be true, i.e. are impossible, and you ask
Horus, _"if you assume my `@pre` conditions, are my `@post` conditions always
true?"_, what you are asking is really, _"if you assume an impossible situation,
are my `@post` conditions always true?"_

In first-order logic, anything can be proved, i.e. anything is possible, if you
start from an impossible state.

As an example:
```cairo
// @pre 9 == 10
// @post $Return.a == 1
func f() -> (a: felt) {
  return (a=0);
}
```
The above program has a postcondition which is obviously `False`, since we
return a literal `a=0`, and we assert that `$Return.a == 1`. *However*, since
the `@pre` condition assumes `9 == 10`, which is impossible, we will get
`Verified`, because we are asking Horus to prove that _"if `9 == 10`, does `f`
return `a=1`?"_, and this is indeed true in a vacuous sort of way, because it
will never be the case that `9 == 10`.


More generally, you may also expect `False` when you use a `@pre` condition
which is obviously not always satisfied. For example:
```cairo
// @pre x == 2
// @post $Return.a == 1
func f(x: felt) -> (a: felt) {
  return (a=1);
}
```
We may expect to get `False` for the above, since, obviously, `x` is not always
going to be `2`, in general. However, Horus will tell us that `f` has judgement
`Verified`. This is because what Horus checks is that **if** `x == 2`, **then**
the function returns `a=1`. It never checks **whether** `x == 2`. It only
**assumes** `x == 2` to then check other things.

Instead, we only get a failure when we **call** `f` from some other place where
Horus is unable to prove that `x` is always `2`. For example:
```cairo
// @pre x == 2
// @post $Return.a == 1
func f(x: felt) -> (a: felt) {
  return (a=1);
}

// @pre x > 0
func g(x: felt) -> (b: felt) {
  let (b,) = f(x);
  return (b=b);
}
```
When we run Horus on the above program, we get:
```console
user@computer:~/pkgs/horus-checker$ horus-check -s cvc5 a.json
f
Verified

g
False

```
So we see that the `False` is given for the caller `g`, and not `f`, since in
order for the call to `f` to satisfy `f`'s precondition, we must have that `x
== 2`, but all we know is that `x > 0`, from the precondition of `g`. So it is
easy for Horus to find a counterexample that breaks our specification. In
particular, we could pick `x == 1`.

#### Why am I seeing `Cannot obtain identifier "y". Expected a reference but got "future"`?

You may see this error message when you try to reference a variable that is
out-of-scope in an annotation.

Consider the following program:
```cairo
// @pre y == 3
// @post $Return.a == 1
func f(x: felt) -> (a: felt) {
    let y = 4;
    return (a=x + 2);
}
```

When we compile this, we see:

```console
(horus37) user@computer:~/pkgs/horus-checker$ horus-compile contra.cairo > a.json
contra.cairo:1:6: Cannot obtain identifier "y". Expected a reference but got "future"
@pre y == 3
     ^
```

This is becuase `y` is a local variable. It is not present in the function's
type signature, it is defined _within_ the function body. We see this error
because **local variables cannot be referenced in preconditions or
postconditions**.

#### Why am I getting `Unknown`?

There are many reasons why an SMT solver may timeout on a query. Unfortunately,
satisfiability is a very difficult problem
([undecidable](https://en.wikipedia.org/wiki/Undecidable_problem) in general),
and it is nigh impossible to accurately predict how long an arbitrary query
will take to solve.

However, besides increasing the timeout passed to the solver with the `-t`
flag, it may also be helpful to try using a different solver backend to resolve
`Unknown` results. It is notable that **`cvc5` (the default solver) does not
perform well with nonlinear arithmetic**, and thus it is better to use `z3` or
`mathsat` for these cases.

It is also sometimes helpful to rewrite your annotations in a different, but
logically equivalent form, as this sometimes has the effect of making the query
easier for solver.


## Usage

Horus consists of two command-line tools, `horus-compile` and `horus-check`.

### `horus-compile`
```console
horus-compile [-h] [--abi ABI] [--disable_hint_validation]
              [--account_contract] [--prime PRIME]
              [--cairo_path CAIRO_PATH] [--preprocess]
              [--output OUTPUT] [--no_debug_info]
              [--cairo_dependencies CAIRO_DEPENDENCIES]
              [--no_opt_unused_functions] [-v]
              file [file ...]
```
A tool to compile checked StarkNet contracts.

Emits a compiled Cairo program in the form of JSON, printed to `stdout` by default.

#### Example

```console
horus-compile a.cairo > b.json
```
Compiles the annotated Cairo program `a.cairo`, and dumps the output into `b.json`.

#### Positional arguments

`file`

One or more Cairo programs to compile.

#### Flags
`-h, --help`

Show a help message and exit

`--abi ABI`

Dump the contract's ABI (application binary interface)
to a file. This is a JSON list containing metadata
(like type signatures and members) on functions,
structs, and other things within the program.

`--disable-hint-validation`

Disable the hint validation, which ordinarily checks
program hints against a whitelist.

`--account-contract`

Compile as account contract, which means the ABI will
be checked for expected builtin entry points.

`--prime PRIME`

The positive integer size of the finite field. This is
a (usually large) prime power over which basic
arithmetic within the program is carried out.

`--cairo_path CAIRO_PATH`

A list of directories, separated by ":" to resolve
import paths. The full list will consist of
directories defined by this argument, followed by the
environment variable `CAIRO_PATH`, the working directory
and the standard library path.

`--preprocess`

Stop after the preprocessor step and output the
preprocessed program, which consists only of low-level
Cairo (e.g. frame pointer and allocation pointer
manipulations) along with annotations indicating
relevant source code locations.

`--output OUTPUT`

The output file name (default: stdout).

`--no_debug_info`

Don't include debug information in the compiled file.
Removes the 'debug_info' field from the JSON output,
which by default contains an 'instruction_locations'
map with information on flow tracking data, hints,
accessible scopes, and source code location.

`--cairo_dependencies CAIRO_DEPENDENCIES`

Path to dump a list of the Cairo source files used
during the compilation as a CMake file.

`--no_opt_unused_functions`

Disable unused function optimization, which ordinarily
only compiles functions reachable from the main scope
in the dependency graph, i.e. functions that are
actually called.

`-v, --version`

Show program's version number and exit

### `horus-check`
```console
horus-check COMPILED_FILE [-v|--verbose] [--output-queries DIR]
            [--output-optimized-queries DIR] (-s|--solver SOLVER)
            [-t|--timeout TIMEOUT]
```

#### Example

```console
horus-check b.json
```
Attempts to verify the compiled Cairo program `b.json` with the default SMT
solver `cvc5`, and prints the output to `stdout`.


#### Positional arguments

`COMPILED_FILE`

A JSON contract compiled with 'horus-compile'. This can be generated from a
'.cairo' file as follows (for an example contract called `program.cairo`):
```console
horus-compile --output program.json program.cairo
```

#### Flags

`-v,--verbose`

Print all intermediate steps (control flow graph, SMT2 queries, metadata for
each module).

`--output-queries DIR`

Stores the (unoptimized) SMT queries for each module in .smt2 files inside DIR.

`--output-optimized-queries DIR`

Stores the (optimized) SMT queries for each module in .smt2 files inside DIR.

`-s,--solver SOLVER`

Solver to check the resulting SMT queries (options: `z3`, `cvc5`, `mathsat`).

> **Note:** If verifying a function `f()` that calls a function `g()` whose
> Horus annotations contain logical variables, the `mathsat` and `cvc5` solvers
> will fail, and thus `z3` must be used.

You can also pass multiple solvers, which will be tried in the order they are
passed as flags. In the example below, we run `z3` followed by `mathsat`:

```console
horus-check example.json -s z3 mathsat cvc5
```

The timeout will apply to each solver individually, meaning that running two
solvers doubles the maximum time the `horus-check` command will run before
terminating.

`-t,--timeout TIMEOUT`

Time limit (ms) per-module, per-SMT solver.

`-h,--help`

Show this help text

## Annotations

To formally verify a program, we must prove that it behaves as expected. In
order to do this, we must tell Horus what the expected behavior of the program
is. The way that we do this is with the **annotation language**. Annotations
are comments that contain special syntax that Horus can understand. The set of
all of a function's annotations is sometimes referred to as its _specification_
or _spec_. Here's an example:

```cairo
/ @post $Return.res == 3
func example() -> (res: felt) {
	return (3,);
}
```

The annotation in the example above is the line:
```cairo
// @post $Return.res == 3
```

It asserts that the `res` return value must always be `3`.

### Annotation syntax

In order to describe the expected behavior of a function, we need to be able to
talk about the function's inputs, outputs, and effects. This means, we need to
be able to reference the function's parameters and return values in
annotations. Below, we describe the syntax for references, [logical
variables](#declare), boolean expressions, and implications:
* `a`, `$a` cairo references and logical variables can be used by name
* `$Return.a` the special logical variable `$Return` is defined to contain the values returned from the function
* `a+b`, `a==b`, arithmetic operations and comparisons are supported for felts as in Cairo
* `a==b or c==d`, `a==b and c==d`, `not a==b`, `a==b -> c==d` (disjunctions, conjunctions, negations, and implications)
* `True`, `False` are defined as keywords

### Annotation types

### `@post`
Specifies conditions that must be true when the function returns. The name
`post` is short for "postcondition".

No claim is made about whether the function completes or reverts. We only
assert that _if it completes_, then the postcondition holds.


> **Example**
> ```cairo
> // @post $Return.res < 100 && $Return.res >= 50
> ```
> The annotation above asserts that the return value with name `res` of the
> function (not pictured here) is less than 100 and greater than or equal to
> 50.

Local variables cannot be referenced in postconditions.

### `@pre`
Specifies conditions that must be true immediately before a function is called.
The name `pre` is short for "precondition". This annotation type allows us to:
* Place constraints on the values of function parameters
* Assign values to [logical variables](#declare)


> **Example**
> ```cairo
> // @pre flag * (flag - 1) == 0
> ```
> The annotation above asserts that the function parameter with name `flag`
> satisfies the equation $x(x - 1) = 0$, which implies that $x = 0$ or $x = 1$.

Local variables cannot be referenced in preconditions.

### `@declare`
Allows the introduction of logical variables.

A **logical variable** is a variable defined and used within a function spec
(i.e.  a set of annotations for a function, i.e. a set of lines starting with
`// @`) for conveniently referring to subexpressions. They play the same role
that ordinary variables do in any programming language, but they can only be
used within `horus` annotations.

Logical variable names must begin with a `$`. Note that if a logical variable
is not mentioned in the precondition, then the spec must hold for all possible
values of that variable.

> **Example**
> ```cairo
> // @declare $x : felt
> // @pre $x == 5
> ```
> In the above example, `$x` is the logical variable being declared, and we
> assign it a value using a precondition.

### `@storage_update`
Allows claims to be made about the state of a
[storage variable](https://docs.starknet.io/documentation/architecture_and_concepts/Contracts/contract-storage/#storage_variables)
before and after the function. A storage update **must be included for all
storage variables modified by a function**, or verification will fail.

> The first new primitive that we see in the code is `@storage_var`. Unlike a
> Cairo program, which is stateless, StarkNet contracts have a state, called
> “the contract’s storage”. Transactions invoked on such contracts may modify
> this state, in a way defined by the contract.
>
> The `@storage_var` decorator declares a variable which will be kept as part
> of this storage. In our case, this variable consists of a single felt, called
> balance. To use this variable, we will use the `balance.read()` and
> `balance.write()` functions which are automatically created by the
> `@storage_var` decorator. When a contract is deployed, all its storage cells
> are initialized to zero. In particular, all storage variables are initially
> zero.
>
> *From the Cairo documentation on [writing Starknet contracts](https://www.cairo-lang.org/docs/hello_starknet/intro.html?highlight=storage%20variable)*

> **Example**
> ```cairo
> // @storage_update x() := x() + 1
> ```
> In the above example, only the top-level storage variable reference on the left
hand side refers to the state after the function. As such, if `x` took one
input and we specified the update as such `x(y()) := x(y()) + 1`, both
instances of `y()` refer to the state before the function was called.

**Note:** If you would like to make claims about the relationship between
multiple storage variables after the function is complete, this can be achieved
via the use of logical variables. To do so, equate your 'before' logical
variable to the storage variable in the precondition. Then, also in the
precondition, relate the 'after' and 'before' logical variables. Finally assign
the 'after' logical variable to the storage variable in a storage update
annotation.

### `@invariant`
Introduces a constraint attached to a label, typically used for loop invariants.

The invariant annotation is only required in the case of low level loops
implemented with jump instructions, however it can also be used to make claims
that must hold at any specific point in a function by adding an appropriately
named label and attaching the annotation to it. Note that this effectively
splits the function in two, and that anything from before the invariant that is
not mentioned within it cannot be used after.

> **Example**
> ```cairo
> // @invariant i <= 10
> ```
> In the above example, we assert that a local variable `i` (perhaps a loop
> variable) is always less than or equal to 10.

### `@assert`
Introduces a boolean constraint at an arbitrary point in a function body.

You write a boolean expression after `@assert`, and Horus will try to prove
that the expression will always evaluate to `True`.

> **Example**
> ```cairo
> // @assert j >= 10
> ```
> In the above example, we assert that a local variable `j` is at least 10.

### Storage variable rules

In a function that updates a storage variable `x`, it is ambiguous what the
name `x` refers to in an annotation. It could be the **initial** value, before
the update, or the **final** value, after the update.

Here are the rules for figuring out which value is being referenced:
* If a storage variable is referenced in a precondition (`@pre`), it is the **initial** value.
* If a storage variable is referenced in a postcondition (`@post`), it is the **final** value.
* Storage variables cannot be referenced in `@assert` annotations within function bodies.

## Internals

The purpose of this section is to give a brief high-level overview of the
architecture of Horus. Hopefully this can serve as a minimal guide to
understanding the source code and contributing to the project.

The entrypoint source file is `app/Main.hs`. When we run `horus-check` on a
compiled JSON file:
```console
horus-check -s z3 a.json
```
the first thing that happens is that we deserialize the JSON into a value of
type `ContractDefinition`, which is defined in `ContractDefinition.hs`. This
contract is then preprocessed into a richer value of type called
`ContractInfo`, which contains information on the instructions, storage
variables, identifiers, etc.

A mutable configuration record and the immutable `ContractInfo` data are
carried around in an `Env` throughout the program, accessible from within a
`ReaderT` stack.

The implementation makes use of several eDSLs (embedded domain-specific
languages), the most important of which is `GlobalL`, defined in `Global.hs`.
Each DSL is separated into two source files: one containing functions written
_in_ the DSL, like `Global.hs`, and one containing the implementation of the
interpreter for the DSL, as well as a runner, like `Global/Runner.hs`. Each DSL
contains a record type defining the 'instructions' that constitute the
language, which is named something like `GlobalL`, where the `L` suffix stands
for "Language".

There is an `Impl` type defined in the `Runner.hs` file for each DSL, which is
the monad stack in which the interpreter is run. For example, the `Impl` type
for `GlobalL` looks like this:
```haskell
type Impl = ReaderT Env (ExceptT Text IO)
```

The DSLs are written in a continuation-passing style. For example, the
constructor `GetConfig` within the `GlobalL` DSL looks like this:
```haskell
data GlobalF a
  ...
  | GetConfig (Config -> a)
  ...
```
This constructor can be thought of as an "instruction" within the DSL, which,
when run by the interpreter, returns a value of type `Config`. The reason why
we see `(Config -> a)` is because `a` is the continuation of the "program"
within the DSL, i.e. more instructions for the interpreter to process.

The `Global` DSL, and in particular the `solveContract` routine within
`Global.hs`, serves as the entrypoint to the rest of the program, and its
runner is called from within `Main.hs`.

Apart from `GlobalL`, there are several other sub-DSLs, which include:
* `CFGBuildL` -- builds the control flow graph.
* `ModuleL` -- constructs a list of `Module`s from the control flow graph.
* `CairoSemanticsL` -- constructs the set of memory variables, assertions, etc.
  for a given module.
* `PreprocessorL` -- preprocesses and runs SMT queries.

### Glossary

* **contract** - a Starknet smart contract.
* **control flow** - refers to things like `if-else` blocks, loops, function
  calls, etc.
* **control flow graph** - consists of a list of labels, which are our
  vertices, along with the code between these labels, which is represented as
  edges between those vertices.
* **label** - a construct within the Cairo language implementation that
  identifies instructions which may be jumped-to.
* **module** - a set of contiguous Cairo instructions between control flow
  primitives. A function may contain multiple modules. For example, a function
  with `if` branching will include a module for when the branching condition is
  `True`, and another module for when it is `False`.
* **SMT query** - a symbolic proposition which may be passed to an SMT solver,
  which will attempt to prove it or give a counterexample.
