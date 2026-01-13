# Dockerized COBOL & Oracle Compiler

A turnkey environment for compiling and running GnuCOBOL applications with full Oracle Pro\*COBOL support. This utility provides a consistent toolchain wrapped in Docker, eliminating local setup headaches for `cobc` and `procob`.

## 🚀 Quick Start

### 1. Prerequisites

- Docker
- Docker Compose
- `make` (Optional, for simplified commands)

### 2. Setup

The repository comes pre-packaged with necessary Oracle Instant Client binaries. Simply build the image:

```bash
make build
# Or without make: docker-compose build
```

## 📂 Workflow Directory

Work primarily takes place in the `workspace/` directory, which is mounted into the container.

- **`workspace/src/`** (Input): Place your source code here.
  - Use `.pco` for COBOL with embedded SQL (`EXEC SQL`).
  - Use `.cbl` for standard COBOL.
- **`workspace/bin/`** (Output): Compiled executable modules (`.so`) are generated here.
- **`workspace/compile.sh`**: The build script (runs automatically inside the container).
- **`workspace/execute.sh`**: The runner script.

## 🛠️ Usage

### Compiling Code

To compile **all** source files located in `workspace/src/`:

```bash
make compile
# Or: docker-compose run --rm cobol-compiler ./compile.sh
```

**What happens?**

1. The script finds all `.pco` files and precompiles them using Oracle's `procob`.
2. It gathers all `.cbl` files (including those generated from `.pco`).
3. It compiles everything into shared object modules (`.so`) using `cobc`.
4. Artifacts are placed in `workspace/bin/`.

### Running Modules

To execute a compiled module (e.g., `helloworld.so`):

```bash
make run mod=helloworld
# Or: docker-compose run --rm cobol-compiler ./execute.sh helloworld
```

_Note: Do not add the extension `.so` in the command, just the module name._

### Interactive Shell

For manual debugging, running specific commands, or checking environment variables:

```bash
make shell
# Or: docker-compose run --rm cobol-compiler bash
```

## 📏 Conventions

- **File Extensions:**
  - `.pco`: Oracle Pro\*COBOL source.
  - `.cbl`: Pure GnuCOBOL source.
- **Output:**
  - All compiled binaries are native shared objects (`.so`) compatible with `cobcrun`.
