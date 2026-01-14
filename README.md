[![Codacy Badge](https://app.codacy.com/project/badge/Grade/4cceb8c0d38f487aaf230cdda4b3d787)](https://app.codacy.com/gh/apenlor/procob-compiler-kit/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)

# Dockerized COBOL & Oracle Compiler Kit

A turnkey environment for compiling and running GnuCOBOL applications with full Oracle Pro*COBOL support, designed for a consistent and hassle-free developer workflow.

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

These commands are for your day-to-day compile-and-run loop.

- **Compile All Sources**
  ```bash
  make compile
  ```
- **Run a Single Module Manually**
  ```bash
  make run mod=helloworld
  ```

### Testing Workflow

This project uses an automated "Golden Master" testing system.

- **Run a Single Test**
  ```bash
  make test mod=bcuota
  ```
  This command compiles the code, runs the program with the correct test data, and compares the output against the known-good "expected" version.

### Maintenance

- **Clean Artifacts** (removes binaries and output files)
  ```bash
  make clean
  ```
- **Reset the Database** (**Warning:** This deletes all data)
  ```bash
  make db-clean
  ```

## Tutorial: Adding a New Program

Here’s how to add a new program named `mycalc` and create a test for it.

**1. Write Your Code**
Create your COBOL source file in `workspace/src/mycalc.cbl`.

**2. Create the Test Case Directory**
```bash
mkdir -p tests/mycalc/input
mkdir -p tests/mycalc/expected
```

**3. Add Test Input**
Place any input files your program needs (e.g., `mycalc-input.dat`) inside `tests/mycalc/input/`.

**4. Compile Your Program**
```bash
make compile
```
Fix any syntax errors that appear.

**5. Generate the "Golden Master" Output**
Run your program once manually to generate the correct, trusted output.
- First, copy your test input to the manual run directory: `cp tests/mycalc/input/* workspace/input/`
- Then, run the program: `make run mod=mycalc`

**6. Save the Golden Master**
Copy the generated output files to your test's `expected` directory.
```bash
cp workspace/output/* tests/mycalc/expected/
```

**7. Run the Automated Test**
Your test is now set up. You can verify it at any time.
```bash
make test mod=mycalc
```
It should pass. If you change your COBOL code, you can simply run this command to ensure you haven't broken anything.
