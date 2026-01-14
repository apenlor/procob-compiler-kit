package executor

import (
	"strings"
	"testing"
)

func TestRunCommand_Echo(t *testing.T) {
	// "echo" should be available on most systems (linux/mac)
	out, err := RunCommand(".", "echo", "hello")
	if err != nil {
		t.Fatalf("expected no error, got %v", err)
	}
	// Output might contain newline
	if !strings.Contains(out, "hello") {
		t.Errorf("expected output to contain 'hello', got %q", out)
	}
}
