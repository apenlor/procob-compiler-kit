package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"procob-compiler-kit/tools/test-runner/pkg/assert"
	"procob-compiler-kit/tools/test-runner/pkg/executor"
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
	cleanDirectory(workspaceInput)
	cleanDirectory(workspaceOutput)

	// 3. Inject: Copy tests/<mod>/input/* to workspace/input/
	fmt.Printf("--> Injecting test data for module: %s\n", *mod)
	copyDirectoryContents(testInput, workspaceInput)

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

func cleanDirectory(dir string) {
	err := os.RemoveAll(dir)
	if err != nil {
		fmt.Printf("Error cleaning directory %s: %v\n", dir, err)
		os.Exit(1)
	}
	err = os.MkdirAll(dir, 0755)
	if err != nil {
		fmt.Printf("Error recreating directory %s: %v\n", dir, err)
		os.Exit(1)
	}
}

func copyDirectoryContents(src, dest string) {
	entries, err := os.ReadDir(src)
	if err != nil {
		// It's okay if a test has no input files
		if os.IsNotExist(err) {
			fmt.Printf("No input directory for module, skipping copy.\n")
			return
		}
		fmt.Printf("Error reading source directory %s: %v\n", src, err)
		os.Exit(1)
	}

	for _, entry := range entries {
		srcPath := filepath.Join(src, entry.Name())
		destPath := filepath.Join(dest, entry.Name())

		fileData, err := os.ReadFile(srcPath)
		if err != nil {
			fmt.Printf("Error reading source file %s: %v\n", srcPath, err)
			os.Exit(1)
		}

		err = os.WriteFile(destPath, fileData, 0644)
		if err != nil {
			fmt.Printf("Error writing destination file %s: %v\n", destPath, err)
			os.Exit(1)
		}
	}
}
