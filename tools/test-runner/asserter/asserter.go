package asserter

import "fmt"

func CompareDirs(dir1, dir2 string) error {
	fmt.Printf("Comparing directories %s and %s\n", dir1, dir2)
	return nil
}
