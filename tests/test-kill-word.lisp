(in-package :tle)

(defun test-kill-word-same-line ()
  "Test kill-word when deleting words within the same line"
  (format t "Running kill-word same line tests...~%")
  
  ;; Test 1: Kill word from beginning of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 0)  ; At beginning of "hello"
    (kill-word buf)
    (assert (string= (buffer-line buf 0) " world test") ()
            "Test 1 failed: expected ' world test', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Kill word from beginning~%"))
  
  ;; Test 2: Kill word from middle of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 2)  ; At 'l' in "hello"
    (kill-word buf)
    (assert (string= (buffer-line buf 0) "he world test") ()
            "Test 2 failed: expected 'he world test', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 2 point failed: expected (0 2), got ~A" point))
    (format t "✓ Test 2 passed: Kill word from middle~%"))
  
  ;; Test 3: Kill word with spaces before next word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello   world"))
    (buffer-set-point buf 0 5)  ; At first space after "hello"
    (kill-word buf)
    (assert (string= (buffer-line buf 0) "hello") ()
            "Test 3 failed: expected 'hello', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 3 point failed: expected (0 5), got ~A" point))
    (format t "✓ Test 3 passed: Kill spaces and next word~%"))
  
  ;; Test 4: Kill from end of line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)  ; At end of line
    (let ((original-line (buffer-line buf 0)))
      (kill-word buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 4 failed: line should not change, got '~A'" (buffer-line buf 0))
      (format t "✓ Test 4 passed: Kill at end of line does nothing~%")))
  
  ;; Test 5: Kill word with underscores (part of word)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("my_variable other"))
    (buffer-set-point buf 0 0)  ; At beginning of "my_variable"
    (kill-word buf)
    (assert (string= (buffer-line buf 0) " other") ()
            "Test 5 failed: expected ' other', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 5 passed: Kill word with underscores~%"))
  
  (format t "All same-line kill-word tests passed!~%~%"))

(defun test-kill-word-across-lines ()
  "Test kill-word when at end of lines (should do nothing in single-line mode)"
  (format t "Running kill-word end-of-line tests...~%")
  
  ;; Test 1: Kill at end of line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world test"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (let ((original-line-count (buffer-line-count buf))
          (original-line-0 (buffer-line buf 0))
          (original-line-1 (buffer-line buf 1)))
      (kill-word buf)
      (assert (= (buffer-line-count buf) original-line-count) ()
              "Test 1 line count changed: expected ~A, got ~A" original-line-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) original-line-0) ()
              "Test 1 line 0 changed: expected '~A', got '~A'" original-line-0 (buffer-line buf 0))
      (assert (string= (buffer-line buf 1) original-line-1) ()
              "Test 1 line 1 changed: expected '~A', got '~A'" original-line-1 (buffer-line buf 1))
      (format t "✓ Test 1 passed: Kill at end of line does nothing~%")))
  
  ;; Test 2: Kill when only spaces remain on line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello   " "world"))
    (buffer-set-point buf 0 5)  ; At first space after "hello"
    (kill-word buf)
    (assert (string= (buffer-line buf 0) "hello") ()
            "Test 2 failed: expected 'hello', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Kill trailing spaces~%"))
  
  ;; Test 3: Kill word split across lines should only kill part on current line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello wo" "rld test"))
    (buffer-set-point buf 0 6)  ; At 'w' in "wo"
    (kill-word buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "hello ") ()
            "Test 3 failed: expected 'hello ', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "rld test") ()
            "Test 3 line 1 failed: expected 'rld test', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 3 passed: Kill partial word on current line only~%"))
  
  (format t "All end-of-line kill-word tests passed!~%~%"))

(defun test-kill-word-edge-cases ()
  "Test kill-word edge cases and boundary conditions"
  (format t "Running kill-word edge case tests...~%")
  
  ;; Test 1: Single character word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("a b c"))
    (buffer-set-point buf 0 0)  ; At 'a'
    (kill-word buf)
    (assert (string= (buffer-line buf 0) " b c") ()
            "Test 1 failed: expected ' b c', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Kill single character word~%"))
  
  ;; Test 2: Only spaces and symbols (no word characters)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("  !@#  $%^"))
    (buffer-set-point buf 0 0)  ; At first space
    (kill-word buf)
    (assert (string= (buffer-line buf 0) "") ()
            "Test 2 failed: expected empty string, got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Kill non-word characters~%"))
  
  ;; Test 3: Empty line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("" "word"))
    (buffer-set-point buf 0 0)  ; At empty line
    (let ((original-line-count (buffer-line-count buf)))
      (kill-word buf)
      (assert (= (buffer-line-count buf) original-line-count) ()
              "Test 3 line count changed: expected ~A, got ~A" original-line-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) "") ()
              "Test 3 line 0 changed: expected empty string, got '~A'" (buffer-line buf 0))
      (assert (string= (buffer-line buf 1) "word") ()
              "Test 3 line 1 changed: expected 'word', got '~A'" (buffer-line buf 1))
      (format t "✓ Test 3 passed: Kill from empty line does nothing~%")))
  
  ;; Test 4: Only one line with one word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello"))
    (buffer-set-point buf 0 0)
    (kill-word buf)
    (assert (string= (buffer-line buf 0) "") ()
            "Test 4 failed: expected empty string, got '~A'" (buffer-line buf 0))
    (format t "✓ Test 4 passed: Kill only word in buffer~%"))
  
  ;; Test 5: Multiple consecutive kills
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("one two three four"))
    (buffer-set-point buf 0 0)
    (kill-word buf)  ; Kill "one"
    (assert (string= (buffer-line buf 0) " two three four") ())
    (kill-word buf)  ; Kill " two"
    (assert (string= (buffer-line buf 0) " three four") ())
    (kill-word buf)  ; Kill " three"
    (assert (string= (buffer-line buf 0) " four") ())
    (format t "✓ Test 5 passed: Multiple consecutive kills~%"))
  
  (format t "All edge case kill-word tests passed!~%~%"))

(defun test-kill-word-mark-clearing ()
  "Test that kill-word properly clears the mark"
  (format t "Running kill-word mark clearing tests...~%")
  
  ;; Test 1: Mark should be cleared after kill-word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (buffer-set-mark buf 0 3)
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (kill-word buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared")
    (format t "✓ Test 1 passed: Mark cleared after kill-word~%"))
  
  ;; Test 2: Mark cleared even when kill does nothing
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello"))
    (buffer-set-point buf 0 5)  ; At end of line
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 2 setup failed: mark should be set")
    (kill-word buf)  ; Should do nothing at end of last line
    (assert (null (buffer-get-mark buf)) () "Test 2 failed: mark should be cleared even when nothing killed")
    (format t "✓ Test 2 passed: Mark cleared even when kill does nothing~%"))
  
  (format t "All mark clearing kill-word tests passed!~%~%"))

(defun test-kill-word-undo-redo ()
  "Test kill-word undo and redo functionality"
  (format t "Running kill-word undo/redo tests...~%")
  
  ;; Test 1: Basic undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      (kill-word buf)
      (assert (string= (buffer-line buf 0) " world") ()
              "Test 1 kill failed: expected ' world', got '~A'" (buffer-line buf 0))
      ;; Test undo
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1 undo failed: expected '~A', got '~A'" original-line (buffer-line buf 0))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1 undo point failed: expected ~A, got ~A" original-point (buffer-get-point buf))
      ;; Test redo
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) " world") ()
              "Test 1 redo failed: expected ' world', got '~A'" (buffer-line buf 0))
      (format t "✓ Test 1 passed: Basic undo/redo~%")))
  
  ;; Test 2: Undo/redo at end of line (no change expected)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world test"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (let ((original-lines (loop for i from 0 below (buffer-line-count buf)
                                collect (buffer-line buf i)))
          (original-point (copy-list (buffer-get-point buf))))
      (kill-word buf)  ; Should do nothing at end of line
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
      (format t "✓ Test 2 passed: Undo/redo at end of line~%")))
  
  (format t "All undo/redo kill-word tests passed!~%~%"))

(defun test-kill-word-double-undo-redo ()
  "Test kill-word with double undo/redo operations"
  (format t "Running kill-word double undo/redo tests...~%")
  
  ;; Test: Double undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("one two three"))
    (buffer-set-point buf 0 0)
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      ;; First kill
      (kill-word buf)
      (assert (string= (buffer-line buf 0) " two three") ())
      ;; Second kill  
      (kill-word buf)
      (assert (string= (buffer-line buf 0) " three") ())
      ;; First undo
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) " two three") ())
      ;; Second undo
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) original-line) ())
      (assert (equal (buffer-get-point buf) original-point) ())
      ;; First redo
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) " two three") ())
      ;; Second redo
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) " three") ())
      (format t "✓ Double undo/redo test passed~%")))
  
  (format t "All double undo/redo kill-word tests passed!~%~%"))

(defun run-all-kill-word-tests ()
  "Run all kill-word tests"
  (format t "~%======================================~%")
  (format t "Running Kill-Word Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-kill-word-same-line)
        (test-kill-word-across-lines)
        (test-kill-word-edge-cases)
        (test-kill-word-mark-clearing)
        (test-kill-word-undo-redo)
        (test-kill-word-double-undo-redo)
        (format t "~%======================================~%")
        (format t "All kill-word tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)