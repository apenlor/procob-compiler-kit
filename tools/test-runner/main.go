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

// validateSource ensures that exactly one source file (.cbl or .pco) exists for the given module.
func validateSource(moduleName string) error {
	srcRoot := filepath.Join(WorkspaceRoot, "src")
	cblFile := moduleName + ".cbl"
	pcoFile := moduleName + ".pco"

	cblMatches, err := fsops.FindFile(srcRoot, cblFile)
	if err != nil {
		return fmt.Errorf("error searching for .cbl file: %w", err)
	}

	pcoMatches, err := fsops.FindFile(srcRoot, pcoFile)
	if err != nil {
		return fmt.Errorf("error searching for .pco file: %w", err)
	}

	totalMatches := len(cblMatches) + len(pcoMatches)

	if totalMatches == 0 {
		return fmt.Errorf("source file '%s' or '%s' not found in %s", cblFile, pcoFile, srcRoot)
	}
	if totalMatches > 1 {
		return fmt.Errorf("ambiguous module name; found multiple source files for '%s'", moduleName)
	}
	return nil
}

func main() {
	// Step 1: Parse Flags
	mod := flag.String("mod", "", "The name of the test module to run (required)")
	generate := flag.Bool("generate", false, "Generate golden master files instead of testing")
	flag.Parse()

	if *mod == "" {
		fmt.Println("Error: --mod flag is required")
		os.Exit(1)
	}

	// Step 2: Validate that the source code for the module exists
	if err := validateSource(*mod); err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}

	// Define paths
	workspaceInput := filepath.Join(WorkspaceRoot, "input")
	workspaceOutput := filepath.Join(WorkspaceRoot, "output")
	testDir := filepath.Join(TestRoot, *mod)
	testInput := filepath.Join(testDir, "input")
	testExpected := filepath.Join(testDir, "expected")

	// Step 3: Handle Golden Master generation for new tests
	isNewTest := false
	if *generate {
		if _, err := os.Stat(testDir); os.IsNotExist(err) {
			isNewTest = true
			fmt.Printf("--> New test detected for module '%s'. Creating test suite...\n", *mod)
			os.MkdirAll(testInput, 0755)
			os.MkdirAll(testExpected, 0755)

			fmt.Printf("--> Snapshotting workspace/input to %s\n", testInput)
			if err := fsops.CopyDir(workspaceInput, testInput); err != nil {
				fmt.Printf("Error snapshotting input data: %v\n", err)
				os.Exit(1)
			}
		}
	}

	// Defer cleanup to ensure it runs at the end
	defer func() {
		if !isNewTest { // Don't clean if we just snapshotted, user might want to inspect
			fmt.Println("--> Cleaning up workspace...")
			fsops.CleanDir(workspaceInput)
			fsops.CleanDir(workspaceOutput)
		}
	}()

	// Step 4: Prepare workspace by cleaning and injecting test data
	fmt.Println("--> Cleaning workspace...")
	fsops.CleanDir(workspaceInput)
	fsops.CleanDir(workspaceOutput)

	fmt.Printf("--> Injecting test data for module: %s\n", *mod)
	if err := fsops.CopyDir(testInput, workspaceInput); err != nil {
		if !os.IsNotExist(err) {
			fmt.Printf("Error copying test data: %v\n", err)
			os.Exit(1)
		}
		fmt.Println("No input directory for module, skipping copy.")
	}

	// Step 5: Compile & Run
	projectRoot := "../.."
	fmt.Println("--> Compiling sources...")
	if out, err := executor.RunCommand(projectRoot, "make", "compile"); err != nil {
		fmt.Printf("Error during compilation:\n%s\n", out)
		os.Exit(1)
	}

	fmt.Printf("--> Running test for module: %s\n", *mod)
	runArg := fmt.Sprintf("mod=%s", *mod)
	if out, err := executor.RunCommand(projectRoot, "make", "run", runArg); err != nil {
		fmt.Printf("Error during test execution:\n%s\n", out)
		os.Exit(1)
	}

	// Step 6: Verify or Generate Golden Master
	if *generate {
		fmt.Println("--> Generating golden master files...")
		fsops.CleanDir(testExpected)
		if err := fsops.CopyDir(workspaceOutput, testExpected); err != nil {
			fmt.Printf("Error copying golden master files: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("--> Golden master for module '%s' generated successfully!\n", *mod)
	} else {
		fmt.Println("--> Verifying output...")
		if err := assert.CompareDirs(workspaceOutput, testExpected); err != nil {
			fmt.Printf("Verification failed: %v\n", err)
			os.Exit(1)
		}
		fmt.Println("--> Test run successful!")
	}
}
