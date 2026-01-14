package assert

import (
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
)

// CompareDirs compares two directories. Returns nil if identical, error otherwise.
func CompareDirs(expectedDir, actualDir string) error {
	expFiles, err := listFiles(expectedDir)
	if err != nil {
		return fmt.Errorf("failed to list expected dir: %w", err)
	}
	actFiles, err := listFiles(actualDir)
	if err != nil {
		return fmt.Errorf("failed to list actual dir: %w", err)
	}

	// Compare file lists
	var expPaths, actPaths []string
	for p := range expFiles {
		expPaths = append(expPaths, p)
	}
	for p := range actFiles {
		actPaths = append(actPaths, p)
	}
	sort.Strings(expPaths)
	sort.Strings(actPaths)

	// Check for mismatches in file existence
	if len(expPaths) != len(actPaths) || !slicesEqual(expPaths, actPaths) {
		// Provide detailed diff of missing/extra
		return fmt.Errorf("file list mismatch:\nExpected: %v\nActual:   %v", expPaths, actPaths)
	}

	// Compare content
	for _, relPath := range expPaths {
		expContent, err := os.ReadFile(expFiles[relPath])
		if err != nil {
			return err
		}
		actContent, err := os.ReadFile(actFiles[relPath])
		if err != nil {
			return err
		}
		if !bytes.Equal(expContent, actContent) {
			return fmt.Errorf("content mismatch in %s", relPath)
		}
	}

	return nil
}

func listFiles(root string) (map[string]string, error) {
	files := make(map[string]string)
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || d.Name() == ".gitkeep" {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		// Normalize separators for cross-platform safety, though Rel usually handles it
		rel = filepath.ToSlash(rel)
		files[rel] = path
		return nil
	})
	return files, err
}

func slicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
