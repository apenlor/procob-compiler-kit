package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"procob-compiler-kit/tools/test-runner/pkg/assert"
	"procob-compiler-kit/tools/test-runner/pkg/executor"
	"procob-compiler-kit/tools/test-runner/pkg/fsops"
)

const (
	WorkspaceRoot = "../../workspace"
	TestRoot      = "../../tests"
)

func main() {
	// Step 1: Parse Flags
	mod := flag.String("mod", "", "The name of the test module to run (required)")
	flag.Parse()

	// 1. Validate mod arg
	if *mod == "" {
		fmt.Println("Error: --mod flag is required")
		os.Exit(1)
	}

	// Define paths
	workspaceInput := filepath.Join(WorkspaceRoot, "input")
	workspaceOutput := filepath.Join(WorkspaceRoot, "output")
	testInput := filepath.Join(TestRoot, *mod, "input")
	testExpected := filepath.Join(TestRoot, *mod, "expected")

	// 2. Clean workspace/input and workspace/output
	fmt.Println("--> Cleaning workspace...")
	if err := fsops.CleanDir(workspaceInput); err != nil {
		fmt.Printf("Error cleaning directory %s: %v\n", workspaceInput, err)
		os.Exit(1)
	}
	if err := fsops.CleanDir(workspaceOutput); err != nil {
		fmt.Printf("Error cleaning directory %s: %v\n", workspaceOutput, err)
		os.Exit(1)
	}

	// 3. Inject: Copy tests/<mod>/input/* to workspace/input/
	fmt.Printf("--> Injecting test data for module: %s\n", *mod)
	if err := fsops.CopyDir(testInput, workspaceInput); err != nil {
		// It's okay if a test has no input files
		if !os.IsNotExist(err) {
			fmt.Printf("Error copying test data: %v\n", err)
			os.Exit(1)
		}
		fmt.Println("No input directory for module, skipping copy.")
	}

	// Define project root relative to the main.go file
	projectRoot := "../.."

	// 4. Compile: Run make compile
	fmt.Println("--> Compiling sources...")
	if out, err := executor.RunCommand(projectRoot, "make", "compile"); err != nil {
		fmt.Printf("Error during compilation:\n%s\n", out)
		os.Exit(1)
	}

	// 5. Run: Run make run mod=<mod>
	fmt.Printf("--> Running test for module: %s\n", *mod)
	runArg := fmt.Sprintf("mod=%s", *mod)
	if out, err := executor.RunCommand(projectRoot, "make", "run", runArg); err != nil {
		fmt.Printf("Error during test execution:\n%s\n", out)
		os.Exit(1)
	}

	// 6. Verify: Call assert.CompareDirs(workspace/output, tests/<mod>/expected)
	fmt.Println("--> Verifying output...")
	if err := assert.CompareDirs(workspaceOutput, testExpected); err != nil {
		fmt.Printf("Verification failed: %v\n", err)
		os.Exit(1)
	}

	// 7. Report: Exit 0 on success
	fmt.Println("--> Test run successful!")
	os.Exit(0)
}
