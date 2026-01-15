[![Codacy Badge](https://app.codacy.com/project/badge/Grade/4cceb8c0d38f487aaf230cdda4b3d787)](https://app.codacy.com/gh/apenlor/procob-compiler-kit/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)

# Dockerized COBOL & Oracle Compiler Kit

A turnkey environment for compiling and running GnuCOBOL applications with full Oracle Pro\*COBOL support, designed for a consistent and hassle-free developer workflow.

## Table of Contents

- [Quick Reference](#quick-reference)
- [Core Concepts](#core-concepts)
- [Project Structure](#project-structure)
- [Prerequisites](#prerequisites)
- [Setup](#setup)
- [Commands](#commands)
  - [Development Workflow](#development-workflow)
  - [Testing Workflow](#testing-workflow)
  - [Maintenance](#maintenance)
- [Tutorial: Adding a New Program](#tutorial-adding-a-new-program)
- [Database Connection Details](#database-connection-details)

## Quick Reference

### Key Commands

| Command | Description |
| :--- | :--- |
| `make build` | Build the Docker infrastructure. |
| `make compile` | Compile all COBOL sources. |
| `make run mod=NAME` | Run a compiled module manually. |
| `make test mod=NAME` | Run an automated regression test. |
| `make golden-master mod=NAME` | Regenerate a test's golden master files. |
| `make shell` | Start an interactive shell in the runner. |
| `make db-clean` | Destroy the database and its data volume. |

### Database Connection

| Parameter | Value |
| :--- | :--- |
| **Host** | `localhost` |
| **Port** | `1521` |
| **SID** | `FREEPDB1` |
| **Username** | `procob` |
| **Password** | `procob` |

*For detailed connection instructions for GUI vs. CLI clients, see the [Database Connection Details](#database-connection-details) section.*

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

### COBOL Copybooks

The build system automatically detects and includes COBOL Copybooks (`.cpy` files).

- **Location**: Place your `.cpy` files in any subdirectory within `workspace/src/`. The compiler will automatically find them.
- **Usage**: Use the standard `COPY` statement in your COBOL programs (e.g., `COPY "my-copybook.cpy".`).

## Project Structure

The repository is organized to separate the toolchain's infrastructure from your application code.

```
.
├── resources/
│   └── ...             # Third-party dependencies for the Docker build.
├── tools/
│   └── test-runner/    # Go-based CLI for automated testing.
├── tests/
│   ├── bcuota/               # Test case for bcuota module.
│   ├── bcuota-with-copybook/ # Test case with copybook dependency.
│   └── helloworld/           # Simple test case.
└── workspace/
    ├── src/
    │   ├── bcuota/               # Source code for bcuota module.
    │   ├── bcuota-with-copybook/ # Source code using copybooks.
    │   ├── helloworld/           # Source code for helloworld module.
    │   └── ...
    ├── input/          # Input files for manual runs.
    ├── output/         # Output files from manual runs.
    └── bin/            # Compiled binaries (.so).
```

While the compiler does not enforce a strict structure within `workspace/src/`, the convention is to organize each program and its related files into its own subdirectory (e.g., `workspace/src/mycalc/`).

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

  If the test suite does not yet exist, this command will automatically create it for you.

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

This tutorial walks through the modern, streamlined workflow for adding a new program named `mycalc`.

### Step 1: Write and Verify Your Program

The first step is to get your program working manually.

1.  **Write Your Code**: Create your source files (e.g., `mycalc.cbl`) inside a new directory at `workspace/src/mycalc/`.
2.  **Add Input Files (If Needed)**: Place any necessary input data directly into the `workspace/input/` directory.
3.  **Compile and Run**: Use the manual `compile` and `run` commands until you are satisfied with the output generated in `workspace/output/`.

```bash
# First, compile everything
make compile

# Now, run your new module repeatedly until it works
make run mod=mycalc
```

### Step 2: Create the Test Suite

Once your program is working correctly, use the `golden-master` command to create the test suite and capture its state.

```bash
make golden-master mod=mycalc
```

This single command does the following:

- **Validates**: It checks that `mycalc.cbl` or `mycalc.pco` exists.
- **Creates Test Suite**: It creates the `tests/mycalc/`, `tests/mycalc/input/`, and `tests/mycalc/expected/` directories.
- **Snapshots Input**: It copies all files from `workspace/input/` into `tests/mycalc/input/` to be used as the test's input data.
- **Generates Golden Master**: It runs the program and saves the resulting files from `workspace/output/` into `tests/mycalc/expected/` as the official "correct" output.

### Step 3: Run the Test and Iterate

Your new test is now live.

- **Run the Test**:

```bash
make test mod=mycalc
```

- **Iterate**: Continue modifying your code. If you intentionally change the output, just run `make golden-master mod=mycalc` again to bless the new version.

## Database Connection Details

The Oracle database is configured with a dedicated user for the application.

- **Host (from local machine):** `localhost`
- **Port:** `1521`
- **Pluggable Database (SID):** `FREEPDB1`
- **Username:** `procob`
- **Password:** `procob`

### Connecting with a GUI Client (DBeaver, etc.)

For GUI-based tools running on your local machine, use the connection details above.

When running queries, execute only the standard SQL statements. Client-specific commands used by command-line tools (like `SET`, `COLUMN`, `EXIT`) will cause errors and should be omitted.

**Example Query:**

```sql
SELECT * FROM EURIBOR ORDER BY YEAR, MONTH;
```

### Connecting with SQL\*Plus (CLI inside container)

For advanced debugging, you can connect using Oracle's command-line client from within the `runner` container.

1.  **Open a shell in the container:**

    ```bash
    make shell
    ```

2.  **Connect using the service name:**
    The service name `oracle-db` is used for internal container-to-container communication.

    ```bash
    sqlplus procob/procob@//oracle-db:1521/FREEPDB1
    ```

3.  **Run a Script:**
    The following example script includes SQL*Plus specific commands for formatting (`SET`, `COLUMN`) and to terminate the session (`EXIT`). These are ignored by GUI clients but are necessary for scripting with SQL*Plus.

    ```sql
    -- Filename: query.sql
    SET LINESIZE 100
    COLUMN YEAR FORMAT 9999
    COLUMN MONTH FORMAT 99
    COLUMN INT_RATE FORMAT 9.999

    SELECT * FROM EURIBOR ORDER BY YEAR, MONTH;

    EXIT;
    ```
