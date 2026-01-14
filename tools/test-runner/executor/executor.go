package executor

import "fmt"

func Run(command string, args ...string) error {
	fmt.Printf("Running command: %s %v\n", command, args)
	return nil
}
