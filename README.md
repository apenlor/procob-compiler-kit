[![Codacy Badge](https://app.codacy.com/project/badge/Grade/4cceb8c0d38f487aaf230cdda4b3d787)](https://app.codacy.com/gh/apenlor/procob-compiler-kit/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)

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

- **`workspace/src/`**: Place your source code here (`.cbl`, `.pco`).
- **`workspace/input/`**: Place your runtime data files here (e.g., [`bccuota-input`](workspace/input/bcuota-input)).
- **`workspace/output/`**: Generated reports, logs, and output files appear here after a run.
- **`workspace/bin/`**: Compiled executable modules (`.so`) are generated here. This folder also acts as a temporary sandbox during execution.

## 🛠️ Usage

### Compiling Code

To compile **all** source files located in `workspace/src/`:

```bash
make compile
# Or: docker-compose run --rm compiler ./compile.sh
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
# Or: docker-compose run --rm runner ./execute.sh helloworld
```

_Note: Do not add the extension `.so` in the command, just the module name._

### Testing Modules

This project uses a "Golden Master" testing approach. Test cases are defined in the `tests/` directory, where each subdirectory contains `input` and `expected` output files.

To run the test case for a specific module:

```bash
make test mod=bcuota
```

**What happens?**
1. The Go test runner compiles the latest code.
2. It copies the test's input files into the workspace.
3. It runs the module inside the Docker container.
4. It compares the generated output against the "expected" golden master files.
5. The command will exit with a success or failure code.

### Cleaning Up

To remove all compiled binaries and generated output files:

```bash
make clean
```

### Resetting the Database

If the database fails to start or gets corrupted, you can completely reset it. **This will delete all data.**

```bash
make db-clean
```

## 📏 Conventions

- **File Extensions:**
  - `.pco`: Oracle Pro\*COBOL source.
  - `.cbl`: Pure GnuCOBOL source.
- **Output:**
  - All compiled binaries are native shared objects (`.so`) compatible with `cobcrun`.
