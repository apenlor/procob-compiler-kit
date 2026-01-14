package executor

import (
	"os/exec"
)

// RunCommand executes a command in a directory and returns output or error
func RunCommand(dir string, name string, args ...string) (string, error) {
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	output, err := cmd.CombinedOutput()
	return string(output), err
}
