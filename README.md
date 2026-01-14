[![Codacy Badge](https://app.codacy.com/project/badge/Grade/4cceb8c0d38f487aaf230cdda4b3d787)](https://app.codacy.com/gh/apenlor/procob-compiler-kit/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)

# Dockerized COBOL & Oracle Compiler Kit

A turnkey environment for compiling and running GnuCOBOL applications with full Oracle Pro\*COBOL support, designed for a consistent and hassle-free developer workflow.

## Table of Contents

- [Core Concepts](#core-concepts)
- [Project Structure](#project-structure)
- [Prerequisites](#prerequisites)
- [Setup](#setup)
- [Commands](#commands)
  - [Development Workflow](#development-workflow)
  - [Testing Workflow](#testing-workflow)
  - [Maintenance](#maintenance)
- [Tutorial: Adding a New Program](#tutorial-adding-a-new-program)

## Core Concepts

This project provides a "black box" compiler and runtime environment using Docker. It isolates the complexities of the Oracle and COBOL toolchains, allowing you to focus solely on writing and testing your code.

The system is composed of three services managed by Docker Compose:

```
+----------+       +---------+       +------------+
| compiler | ----> | runner  | ----> | oracle-db  |
+----------+       +---------+       +------------+
(No DB Link)    (Executes Code)     (Database)
```

- **`compiler`**: A lightweight container that compiles your source code without needing a database connection.
- **`runner`**: An identical container that executes your compiled code and has a direct link to the database.
- **`oracle-db`**: The Oracle database instance for runtime operations.

## Project Structure

The repository is organized to separate the toolchain's infrastructure from your application code.

```
.
├── resources/
│   └── ...           # Third-party dependencies for the Docker build.
├── tools/
│   └── test-runner/    # Go-based CLI for automated testing.
├── tests/
│   └── bcuota/         # Example test case.
│       ├── input/      # Input files for this test.
│       └── expected/   # "Golden Master" expected output.
└── workspace/
    ├── src/            # Your COBOL source code (.cbl, .pco).
    ├── input/          # Input files for manual runs.
    ├── output/         # Output files from manual runs.
    └── bin/            # Compiled binaries (.so).
```

## Prerequisites

- Docker
- Docker Compose
- `make` (Optional, for simplified commands)

## Setup

The repository comes pre-packaged with the necessary Oracle Instant Client binaries. Simply build the Docker image:

```bash
make build
```

## Commands

All common tasks are managed via the `Makefile`.

### Development Workflow

The typical development cycle involves editing your source code, compiling, and running the program to check its output.

- **Compile All Sources**

  Use this command after you've made changes to your `.cbl` or `.pco` files. It acts as a "linter" by checking for syntax errors.

  ```bash
  make compile
  ```

  This command finds all source files in `workspace/src/`, pre-compiles any Pro\*COBOL files, and compiles everything into binary modules (`.so`) located in `workspace/bin/`.

- **Run a Single Module Manually**

  To execute a specific program, use the `run` command. This is for manual testing and debugging.

  ```bash
  make run mod=helloworld
  ```

  This command uses the `runner` service (which has database access), copies any files from `workspace/input/` into the execution sandbox, runs your program, and places any generated files into `workspace/output/`.

### Testing Workflow

This project uses an automated "Golden Master" testing system, which is the preferred way to verify program correctness. A "golden master" is a known-good, trusted set of output files.

- **Run an Automated Test**

  This command runs a specific test case defined in the `tests/` directory.

  ```bash
  make test mod=bcuota
  ```

  The runner automates the entire process: it compiles the latest code, copies the correct input files from `tests/bcuota/input`, runs the program, and performs a deep comparison of the generated output against the golden master files stored in `tests/bcuota/expected`. If there is any difference, the test will fail.

- **Create or Update a Golden Master**

  When you create a new program or change an existing one, you need to "bless" its output as the new correct version.

  ```bash
  make golden-master mod=bcuota
  ```

  This command runs the program just like a test, but instead of comparing the output, it deletes the old `expected` files and saves the new output as the golden master for future tests.

### Maintenance

These commands help you manage the project's state, especially when troubleshooting.

- **Clean Artifacts**

  Use this to get a clean slate without affecting the database. It removes all compiled binaries from `workspace/bin/` and all files from `workspace/output/`.

  ```bash
  make clean
  ```

- **Reset the Database**

  Use this command if the Oracle database is behaving unexpectedly or you see errors like `ORA-01034: ORACLE not available`.

  ```bash
  make db-clean
  ```

  **Warning:** This is a destructive operation. It will stop the database container and permanently delete its data volume, wiping out all tables and data. You will need to run `make build` or `make run` to recreate it.

- **Interactive Shell**

  For advanced debugging, you can open a `bash` shell inside the `runner` container. This gives you direct access to the environment where your code runs.

  ```bash
  make shell
  ```

## Tutorial: Adding a New Program

This tutorial walks through the full Test-Driven Development (TDD) cycle for adding a new program named `mycalc`.

- **Step 1: Write Your Code**

Create your COBOL source file at `workspace/src/mycalc.cbl`. You can start with a simple "Hello World" program.

- **Step 2: Create the Test Structure**

Every program needs a test case directory. Create the standard `input` and `expected` folders for your new program.

```bash
mkdir -p tests/mycalc/input tests/mycalc/expected
```

- **Step 3: Define Test Input**

Place any input files your program needs (e.g., `mycalc-input.dat`) inside `tests/mycalc/input/`. If your program doesn't need input files, create an empty `.gitkeep` file so Git tracks the directory:

```bash
touch tests/mycalc/input/.gitkeep
```

- **Step 4: Generate the Golden Master**

Run the automated generator command. This will compile your code, run it with the test inputs, and save the result as the official "correct" output.

```bash
make golden-master mod=mycalc
```

At this point, you should inspect the files generated in `tests/mycalc/expected/` to ensure they are correct. If your program produces no output files, this directory should contain only an empty `.gitkeep` file.

- **Step 5: Run the Test**

With the golden master in place, your test is now live. Run it to confirm everything passes.

```bash
make test mod=mycalc
```

- **Step 6: Develop and Iterate**

Now, you can continue to modify your code in `workspace/src/mycalc.cbl`. After every change, run `make test mod=mycalc`.

- If the test fails, you've introduced a regression.
- If you intentionally change the output, the test will fail. Simply run `make golden-master mod=mycalc` again to "bless" the new output as the correct version.
