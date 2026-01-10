<a id="readme-top"></a>

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/Petar-Yordanov/picolang">
    <img src="assets/image.jpeg" alt="Logo" width="200" height="200">
  </a>

  <h3 align="center">Picolang</h3>

  <p align="center">
    Picolang is a minimal compiled programming language that has a custom frontend and LLVM backend with a custom runtime and Garbage Collection.
  </p>
</div>

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#license">License</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
## About The Project

Picolang is a minimal programming language with syntax mix of Rust and Golang, with garbage collection, a small runtime and a usable foundation (ifs, loops, data types, functions etc.).

PicoLang consists of a compiler and a runtime:
- `picolang-compiler`: lexer/parser + symbols + module loader + diagnostics + LLVM codegen
- `picolang-runtime`: a small runtime library linked into compiled programs

<div align="center">

[![CI](https://github.com/Petar-Yordanov/picolang/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/Petar-Yordanov/picolang/actions/workflows/build.yml)

</div>

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Built With

* [![Rust][rust-badge]][rust-url]
* [![LLVM][llvm-badge]][llvm-url]
* [![Clang][clang-badge]][clang-url]
* [![GitHub Actions][gha-badge]][gha-url]
* [![Linux][linux-badge]][linux-url]

<!-- Badges -->
[rust-badge]: https://img.shields.io/badge/Rust-000000?style=for-the-badge&logo=rust&logoColor=white
[rust-url]: https://www.rust-lang.org/

[llvm-badge]: https://img.shields.io/badge/LLVM-262D3A?style=for-the-badge&logo=llvm&logoColor=white
[llvm-url]: https://llvm.org/

[clang-badge]: https://img.shields.io/badge/Clang-262D3A?style=for-the-badge&logo=llvm&logoColor=white
[clang-url]: https://clang.llvm.org/

[gha-badge]: https://img.shields.io/badge/GitHub%20Actions-2088FF?style=for-the-badge&logo=githubactions&logoColor=white
[gha-url]: https://github.com/features/actions

[linux-badge]: https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black
[linux-url]: https://www.kernel.org/

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

PicoLang builds a native runtime library and a compiler that uses LLVM for code generation. The compiler produces `.o` files which are linked with the runtime to produce executables.

### Prerequisites

- Linux host (Ubuntu 24.04 / Fedora 41 tested)
- Rust stable toolchain
- LLVM 14 development packages
- Clang + LLD
- `make`
- `libffi`, `zlib`, and `ncurses` development packages (for LLVM tooling)

* Essentials setup on Ubuntu/Debian
  ```sh
    sudo apt-get update
    sudo apt-get install -y \
        clang lld \
        llvm-14-dev clang-14 lld-14 \
        libpolly-14-dev \
        make \
        ca-certificates curl \
        libffi-dev \
        zlib1g-dev \
        libtinfo-dev
  ```

* Install Rust
  ```sh
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    source "$HOME/.cargo/env"
    rustup toolchain install stable
    rustup default stable
  ```

### Installation

1. Clone the repo
   ```sh
   git clone git@github.com:Petar-Yordanov/picolang.git
   ```
2. Set LLVM prefix (needed by llvm-sys)
   ```sh
   export LLVM_SYS_140_PREFIX=/usr/lib/llvm-14
   ```
3. Build
   ```sh
   # Debug
   cargo build -p mylang-runtime
   cargo build -p mylang-compiler

   # Release
   cargo build -p mylang-runtime --release
   cargo build -p mylang-compiler --release
   ```

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
## Usage

1. Compile module folder
   ```sh
   export MYLANG_RUNTIME_LIB_DIR="$PWD/target/release"

   cargo run -p mylang-compiler --release -- examples/modules
   ```
2. Compile single file
   ``` sh
   export MYLANG_RUNTIME_LIB_DIR="$PWD/target/release"

   cargo run -p mylang-compiler --release -- examples/llvm_tests/some_test.my
   ```
3. Link and run
   ```sh
   CLANG=clang
   command -v clang-14 >/dev/null 2>&1 && CLANG=clang-14

   $CLANG -no-pie examples/llvm_tests/some_test.o \
      -L target/release -lmylang_runtime \
      -o examples/llvm_tests/some_test

   ./examples/llvm_tests/some_test
   ```

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Roadmap

- [x] **Language**
  - [x] Primitive types (`int`, `bool`, `void`, `char`, `byte`, `string`)
  - [x] Arrays + indexing (`[T]{...}`, `x[i]`, `len(x)`)
  - [x] Functions (params, return types)
  - [x] Variables (`let`, assignment)
  - [x] Control flow (`if/else`, `while`, `for`, `switch/case/default`, `break/continue`, `return`)
  - [x] Imports (`import a.b.c;`)
  - [x] User types: `class`, `enum` (syntax + parsing)
  - [ ] Type parameters / generics end-to-end (beyond parsing)
  - [ ] Real type checking for function calls (arg/param matching)
  - [ ] `switch` support
  - [ ] Classes/enums semantics support

- [x] **Compiler pipeline**
  - [x] Lexer + token stream
  - [x] Parser to AST
  - [x] Diagnostics with spans (parse/type/symbol errors)
  - [x] Symbol table + scope checking (redefs, unknown types, etc.)
  - [x] Basic type checking for expressions and returns
  - [x] AST to IR lowering (basic blocks + terminators)
  - [x] LLVM codegen (IR to object file)

- [x] **Runtime / stdlib**
  - [x] Runtime library linked into executables
  - [x] Garbage collector (mark/sweep with roots)
  - [x] Console I/O (`printInt`, `printChar`, `printString`, logging)
  - [x] File I/O (`runtime_read_file`)
  - [ ] More stdlib (collections, string utilities, formatting)
  - [ ] Expanded file APIs (write, list, path helpers)
  - [ ] GC tuning + safer root management

- [x] **Examples + CI**
  - [x] Folder-mode module builds (`examples/modules`)
  - [x] LLVM tests that must succeed (`examples/llvm_tests`)
  - [x] Negative tests that must fail (`examples/error_modules`)
  - [ ] Debug info support + easier debugging of generated binaries

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.md` for details.

<p align="right">(<a href="#readme-top">back to top</a>)</p>
