package assert

import (
	"os"
	"path/filepath"
	"testing"
)

func TestCompareDirs(t *testing.T) {
	// Helper to create temp dirs
	createTempDir := func(t *testing.T, files map[string]string) string {
		t.Helper()
		dir, err := os.MkdirTemp("", "assert-test-*")
		if err != nil {
			t.Fatalf("failed to create temp dir: %v", err)
		}
		for name, content := range files {
			path := filepath.Join(dir, name)
			if err := os.WriteFile(path, []byte(content), 0644); err != nil {
				t.Fatalf("failed to write file %s: %v", name, err)
			}
		}
		return dir
	}

	t.Run("IdenticalDirs", func(t *testing.T) {
		files := map[string]string{
			"file1.txt": "content1",
			"file2.txt": "content2",
		}
		expected := createTempDir(t, files)
		defer os.RemoveAll(expected)
		actual := createTempDir(t, files)
		defer os.RemoveAll(actual)

		err := CompareDirs(expected, actual)
		if err != nil {
			t.Errorf("expected no error for identical dirs, got: %v", err)
		}
	})

	t.Run("ContentMismatch", func(t *testing.T) {
		expected := createTempDir(t, map[string]string{"file.txt": "foo"})
		defer os.RemoveAll(expected)
		actual := createTempDir(t, map[string]string{"file.txt": "bar"})
		defer os.RemoveAll(actual)

		err := CompareDirs(expected, actual)
		if err == nil {
			t.Error("expected error for content mismatch, got nil")
		}
	})

	t.Run("MissingFile", func(t *testing.T) {
		expected := createTempDir(t, map[string]string{"f1.txt": "a", "f2.txt": "b"})
		defer os.RemoveAll(expected)
		actual := createTempDir(t, map[string]string{"f1.txt": "a"})
		defer os.RemoveAll(actual)

		err := CompareDirs(expected, actual)
		if err == nil {
			t.Error("expected error for missing file, got nil")
		}
	})

	t.Run("ExtraFile", func(t *testing.T) {
		expected := createTempDir(t, map[string]string{"f1.txt": "a"})
		defer os.RemoveAll(expected)
		actual := createTempDir(t, map[string]string{"f1.txt": "a", "extra.txt": "b"})
		defer os.RemoveAll(actual)

		err := CompareDirs(expected, actual)
		if err == nil {
			t.Error("expected error for extra file, got nil")
		}
	})

	t.Run("NestedDirs", func(t *testing.T) {
		// Custom setup for this test because helper might not support nested mkdir
		setup := func() string {
			d, _ := os.MkdirTemp("", "assert-nested-*")
			os.WriteFile(filepath.Join(d, "root.txt"), []byte("a"), 0644)
			os.Mkdir(filepath.Join(d, "sub"), 0755)
			os.WriteFile(filepath.Join(d, "sub", "file.txt"), []byte("b"), 0644)
			return d
		}

		expected := setup()
		defer os.RemoveAll(expected)
		actual := setup()
		defer os.RemoveAll(actual)

		err := CompareDirs(expected, actual)
		if err != nil {
			t.Errorf("expected no error for identical nested dirs, got: %v", err)
		}
	})
}
