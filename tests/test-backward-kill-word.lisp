(in-package :tle)

(defun test-backward-kill-word-same-line ()
  "Test backward-kill-word when deleting words within the same line"
  (format t "Running backward-kill-word same line tests...~%")
  
  ;; Test 1: Kill word from end of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) " world test") ()
            "Test 1 failed: expected ' world test', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Kill word from end~%"))
  
  ;; Test 2: Kill word from middle of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 3)  ; At 'l' in "hello"
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) "lo world test") ()
            "Test 2 failed: expected 'lo world test', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Kill word from middle~%"))
  
  ;; Test 3: Kill word from middle, should kill from beginning of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello   world"))
    (buffer-set-point buf 0 10)  ; At 'r' in "world"
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) "hello   rld") ()
            "Test 3 failed: expected 'hello   rld', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 8)) () "Test 3 point failed: expected (0 8), got ~A" point))
    (format t "✓ Test 3 passed: Kill from beginning of word~%"))
  
  ;; Test 4: Kill from beginning of line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)  ; At beginning of line
    (let ((original-line (buffer-line buf 0)))
      (backward-kill-word buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 4 failed: line should not change, got '~A'" (buffer-line buf 0))
      (format t "✓ Test 4 passed: Kill at beginning of line does nothing~%")))
  
  ;; Test 5: Kill word with underscores (part of word)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("my_variable other"))
    (buffer-set-point buf 0 11)  ; At end of "my_variable"
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) " other") ()
            "Test 5 failed: expected ' other', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 5 passed: Kill word with underscores~%"))
  
  (format t "All same-line backward-kill-word tests passed!~%~%"))

(defun test-backward-kill-word-across-lines ()
  "Test backward-kill-word when at beginning of lines (should do nothing in single-line mode)"
  (format t "Running backward-kill-word beginning-of-line tests...~%")
  
  ;; Test 1: Kill at beginning of line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world test"))
    (buffer-set-point buf 1 0)  ; At beginning of "world"
    (let ((original-line-count (buffer-line-count buf))
          (original-line-0 (buffer-line buf 0))
          (original-line-1 (buffer-line buf 1)))
      (backward-kill-word buf)
      (assert (= (buffer-line-count buf) original-line-count) ()
              "Test 1 line count changed: expected ~A, got ~A" original-line-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) original-line-0) ()
              "Test 1 line 0 changed: expected '~A', got '~A'" original-line-0 (buffer-line buf 0))
      (assert (string= (buffer-line buf 1) original-line-1) ()
              "Test 1 line 1 changed: expected '~A', got '~A'" original-line-1 (buffer-line buf 1))
      (format t "✓ Test 1 passed: Kill at beginning of line does nothing~%")))
  
  ;; Test 2: Kill from beginning of word after spaces
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("   hello world"))
    (buffer-set-point buf 0 3)  ; At 'h' in "hello"
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) "hello world") ()
            "Test 2 failed: expected 'hello world', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Kill leading spaces~%"))
  
  ;; Test 3: Kill word that starts at beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) " world") ()
            "Test 3 failed: expected ' world', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Kill word from beginning of line~%"))
  
  (format t "All beginning-of-line backward-kill-word tests passed!~%~%"))

(defun test-backward-kill-word-edge-cases ()
  "Test backward-kill-word edge cases and boundary conditions"
  (format t "Running backward-kill-word edge case tests...~%")
  
  ;; Test 1: Single character word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("a b c"))
    (buffer-set-point buf 0 1)  ; After 'a'
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) " b c") ()
            "Test 1 failed: expected ' b c', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Kill single character word~%"))
  
  ;; Test 2: Only spaces and symbols (no word characters)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("  !@#  $%^"))
    (buffer-set-point buf 0 10)  ; At end of line
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) "") ()
            "Test 2 failed: expected empty string, got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Kill non-word characters~%"))
  
  ;; Test 3: Empty line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("word" ""))
    (buffer-set-point buf 1 0)  ; At empty line
    (let ((original-line-count (buffer-line-count buf)))
      (backward-kill-word buf)
      (assert (= (buffer-line-count buf) original-line-count) ()
              "Test 3 line count changed: expected ~A, got ~A" original-line-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) "word") ()
              "Test 3 line 0 changed: expected 'word', got '~A'" (buffer-line buf 0))
      (assert (string= (buffer-line buf 1) "") ()
              "Test 3 line 1 changed: expected empty string, got '~A'" (buffer-line buf 1))
      (format t "✓ Test 3 passed: Kill from empty line does nothing~%")))
  
  ;; Test 4: Only one line with one word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (backward-kill-word buf)
    (assert (string= (buffer-line buf 0) "") ()
            "Test 4 failed: expected empty string, got '~A'" (buffer-line buf 0))
    (format t "✓ Test 4 passed: Kill only word in buffer~%"))
  
  ;; Test 5: Multiple consecutive kills
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("one two three four"))
    (buffer-set-point buf 0 18)  ; At end of line
    (backward-kill-word buf)  ; Kill "four"
    (assert (string= (buffer-line buf 0) "one two three ") ())
    (backward-kill-word buf)  ; Kill " three"
    (assert (string= (buffer-line buf 0) "one two ") ())
    (backward-kill-word buf)  ; Kill " two"
    (assert (string= (buffer-line buf 0) "one ") ())
    (format t "✓ Test 5 passed: Multiple consecutive kills~%"))
  
  (format t "All edge case backward-kill-word tests passed!~%~%"))

(defun test-backward-kill-word-mark-clearing ()
  "Test that backward-kill-word properly clears the mark"
  (format t "Running backward-kill-word mark clearing tests...~%")
  
  ;; Test 1: Mark should be cleared after backward-kill-word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)  ; At end of line
    (buffer-set-mark buf 0 3)
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (backward-kill-word buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared")
    (format t "✓ Test 1 passed: Mark cleared after backward-kill-word~%"))
  
  ;; Test 2: Mark cleared even when kill does nothing
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello"))
    (buffer-set-point buf 0 0)  ; At beginning of line
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 2 setup failed: mark should be set")
    (backward-kill-word buf)  ; Should do nothing at beginning of line
    (assert (null (buffer-get-mark buf)) () "Test 2 failed: mark should be cleared even when nothing killed")
    (format t "✓ Test 2 passed: Mark cleared even when kill does nothing~%"))
  
  (format t "All mark clearing backward-kill-word tests passed!~%~%"))

(defun test-backward-kill-word-undo-redo ()
  "Test backward-kill-word undo and redo functionality"
  (format t "Running backward-kill-word undo/redo tests...~%")
  
  ;; Test 1: Basic undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)  ; At end of line
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      (backward-kill-word buf)
      (assert (string= (buffer-line buf 0) "hello ") ()
              "Test 1 kill failed: expected 'hello ', got '~A'" (buffer-line buf 0))
      ;; Test undo
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1 undo failed: expected '~A', got '~A'" original-line (buffer-line buf 0))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1 undo point failed: expected ~A, got ~A" original-point (buffer-get-point buf))
      ;; Test redo
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) "hello ") ()
              "Test 1 redo failed: expected 'hello ', got '~A'" (buffer-line buf 0))
      (format t "✓ Test 1 passed: Basic undo/redo~%")))
  
  ;; Test 2: Undo/redo at beginning of line (no change expected)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world test"))
    (buffer-set-point buf 1 0)  ; At beginning of "world"
    (let ((original-lines (loop for i from 0 below (buffer-line-count buf)
                                collect (buffer-line buf i)))
          (original-point (copy-list (buffer-get-point buf))))
      (backward-kill-word buf)  ; Should do nothing at beginning of line
      ;; Verify nothing changed
      (assert (= (buffer-line-count buf) 2) ())
      (assert (string= (buffer-line buf 0) (first original-lines)) ())
      (assert (string= (buffer-line buf 1) (second original-lines)) ())
      ;; Test undo (should also do nothing since no change was made)
      (buffer-undo buf)
      (assert (= (buffer-line-count buf) 2) ()
              "Test 2 undo line count failed: expected 2, got ~A" (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) (first original-lines)) ()
              "Test 2 undo line 0 failed: expected '~A', got '~A'" (first original-lines) (buffer-line buf 0))
      (assert (string= (buffer-line buf 1) (second original-lines)) ()
              "Test 2 undo line 1 failed: expected '~A', got '~A'" (second original-lines) (buffer-line buf 1))
      (format t "✓ Test 2 passed: Undo/redo at beginning of line~%")))
  
  (format t "All undo/redo backward-kill-word tests passed!~%~%"))

(defun test-backward-kill-word-double-undo-redo ()
  "Test backward-kill-word with double undo/redo operations"
  (format t "Running backward-kill-word double undo/redo tests...~%")
  
  ;; Test: Double undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("one two three"))
    (buffer-set-point buf 0 13)  ; At end of line
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      ;; First kill
      (backward-kill-word buf)
      (assert (string= (buffer-line buf 0) "one two ") ())
      ;; Second kill  
      (backward-kill-word buf)
      (assert (string= (buffer-line buf 0) "one ") ())
      ;; First undo
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) "one two ") ())
      ;; Second undo
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) original-line) ())
      (assert (equal (buffer-get-point buf) original-point) ())
      ;; First redo
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) "one two ") ())
      ;; Second redo
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) "one ") ())
      (format t "✓ Double undo/redo test passed~%")))
  
  (format t "All double undo/redo backward-kill-word tests passed!~%~%"))

(defun run-all-backward-kill-word-tests ()
  "Run all backward-kill-word tests"
  (format t "~%======================================~%")
  (format t "Running Backward-Kill-Word Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-backward-kill-word-same-line)
        (test-backward-kill-word-across-lines)
        (test-backward-kill-word-edge-cases)
        (test-backward-kill-word-mark-clearing)
        (test-backward-kill-word-undo-redo)
        (test-backward-kill-word-double-undo-redo)
        (format t "~%======================================~%")
        (format t "All backward-kill-word tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)