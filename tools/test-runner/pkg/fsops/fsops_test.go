package fsops

import (
	"os"
	"path/filepath"
	"testing"
)

func TestCleanDir(t *testing.T) {
	// Setup: Create a temporary directory
	tempDir, err := os.MkdirTemp("", "fsops_clean_test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create a dummy file in it
	dummyFile := filepath.Join(tempDir, "dummy.txt")
	if err := os.WriteFile(dummyFile, []byte("content"), 0644); err != nil {
		t.Fatalf("Failed to create dummy file: %v", err)
	}

	// Create a subdirectory
	subDir := filepath.Join(tempDir, "subdir")
	if err := os.Mkdir(subDir, 0755); err != nil {
		t.Fatalf("Failed to create subdir: %v", err)
	}

	// Action: Call CleanDir
	// Note: CleanDir is not implemented yet, so this will fail to compile initially if I were running it,
	// but I am writing the test first.
	if err := CleanDir(tempDir); err != nil {
		t.Fatalf("CleanDir failed: %v", err)
	}

	// Verify: Directory should exist
	if _, err := os.Stat(tempDir); os.IsNotExist(err) {
		t.Fatalf("CleanDir removed the root directory itself")
	}

	// Verify: Directory should be empty
	entries, err := os.ReadDir(tempDir)
	if err != nil {
		t.Fatalf("Failed to read dir: %v", err)
	}

	if len(entries) != 0 {
		t.Errorf("Expected 0 entries, got %d: %v", len(entries), entries)
	}
}

func TestCopyDir(t *testing.T) {
	// Setup: Create source directory with a file
	srcDir, err := os.MkdirTemp("", "fsops_copy_src")
	if err != nil {
		t.Fatalf("Failed to create src dir: %v", err)
	}
	defer os.RemoveAll(srcDir)

	srcFile := filepath.Join(srcDir, "data.txt")
	content := []byte("hello world")
	if err := os.WriteFile(srcFile, content, 0644); err != nil {
		t.Fatalf("Failed to write src file: %v", err)
	}

	// Create a nested file
	subDir := filepath.Join(srcDir, "nested")
	if err := os.Mkdir(subDir, 0755); err != nil {
		t.Fatalf("Failed to create src subdir: %v", err)
	}
	nestedFile := filepath.Join(subDir, "nested.txt")
	nestedContent := []byte("nested content")
	if err := os.WriteFile(nestedFile, nestedContent, 0644); err != nil {
		t.Fatalf("Failed to write nested file: %v", err)
	}

	// Setup: Create dest directory
	dstDir, err := os.MkdirTemp("", "fsops_copy_dst")
	if err != nil {
		t.Fatalf("Failed to create dst dir: %v", err)
	}
	defer os.RemoveAll(dstDir)

	// Action: Call CopyDir
	// CopyDir should copy contents of srcDir into dstDir
	// Note: We assume CopyDir copies contents, not the directory itself as a subdir.
	if err := CopyDir(srcDir, dstDir); err != nil {
		t.Fatalf("CopyDir failed: %v", err)
	}

	// Verify: File exists in dest
	dstFile := filepath.Join(dstDir, "data.txt")
	readContent, err := os.ReadFile(dstFile)
	if err != nil {
		t.Fatalf("Failed to read dst file: %v", err)
	}

	if string(readContent) != string(content) {
		t.Errorf("Content mismatch: expected %q, got %q", content, readContent)
	}

	// Verify: Nested file exists
	dstNested := filepath.Join(dstDir, "nested", "nested.txt")
	readNested, err := os.ReadFile(dstNested)
	if err != nil {
		t.Fatalf("Failed to read dst nested file: %v", err)
	}
	if string(readNested) != string(nestedContent) {
		t.Errorf("Nested content mismatch: expected %q, got %q", nestedContent, readNested)
	}
}

func TestCleanDir_Safety(t *testing.T) {
	// Test safety checks for dangerous paths

	// Case 1: Empty string
	if err := CleanDir(""); err == nil {
		t.Error("CleanDir(\"\") should return error, got nil")
	}

	// Case 2: Dot (.)
	// We run this in a safe context (temp dir) just in case,
	// but we expect it to fail validation regardless of context.
	func() {
		// Create safe temp dir
		tempDir, err := os.MkdirTemp("", "fsops_safety")
		if err != nil {
			t.Fatalf("Failed to create temp dir: %v", err)
		}
		defer os.RemoveAll(tempDir)

		// Save current dir
		wd, err := os.Getwd()
		if err != nil {
			t.Fatalf("Failed to get wd: %v", err)
		}
		// Restore wd at end of block
		defer func() {
			if err := os.Chdir(wd); err != nil {
				t.Errorf("Failed to restore wd: %v", err)
			}
		}()

		// Chdir to temp dir
		if err := os.Chdir(tempDir); err != nil {
			t.Fatalf("Failed to chdir: %v", err)
		}

		// Run CleanDir(".")
		// Current implementation: Should succeed (return nil) and clean dir (harmless)
		// Desired implementation: Should return error
		if err := CleanDir("."); err == nil {
			t.Error("CleanDir(\".\") should return error, got nil")
		}
	}()

	// Case 3: Root (/)
	// We DO NOT execute this because it is too dangerous to test "live"
	// against an implementation that might lack the check.
	// We rely on the implementation of "." and "" checks to cover the logic pattern.
}
