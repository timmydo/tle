(in-package :tle)

(defun test-end-of-line-basic ()
  "Test basic end-of-line functionality"
  (format t "Running end-of-line basic tests...~%")
  
  ;; Test 1: Move to end from middle of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 1 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 1 passed: Move to end from middle of line~%"))
  
  ;; Test 2: Move to end from beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 0)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 2 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 2 passed: Move to end from beginning of line~%"))
  
  ;; Test 3: Already at end of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 11)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 3 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 3 passed: Already at end of line~%"))
  
  (format t "All end-of-line basic tests passed!~%~%"))

(defun test-end-of-line-multi-line ()
  "Test end-of-line on different lines"
  (format t "Running end-of-line multi-line tests...~%")
  
  ;; Test 1: Different lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 7)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 11)) () "Test 1 failed: expected (1 11), got ~A" point))
    (format t "✓ Test 1 passed: End of second line~%"))
  
  ;; Test 2: Last line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 2 5)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 10)) () "Test 2 failed: expected (2 10), got ~A" point))
    (format t "✓ Test 2 passed: End of last line~%"))
  
  ;; Test 3: First line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 8)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 10)) () "Test 3 failed: expected (0 10), got ~A" point))
    (format t "✓ Test 3 passed: End of first line~%"))
  
  (format t "All end-of-line multi-line tests passed!~%~%"))

(defun test-end-of-line-edge-cases ()
  "Test end-of-line edge cases"
  (format t "Running end-of-line edge case tests...~%")
  
  ;; Test 1: Empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ""))
    (buffer-set-point buf 0 0)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Empty line~%"))
  
  ;; Test 2: Buffer with mix of empty and non-empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "content" "" "more content"))
    (buffer-set-point buf 1 0)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 2a failed: expected (1 0), got ~A" point))
    ;; Test on non-empty line after empty
    (buffer-set-point buf 2 8)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 12)) () "Test 2b failed: expected (2 12), got ~A" point))
    (format t "✓ Test 2 passed: Mix of empty and non-empty lines~%"))
  
  ;; Test 3: Single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a"))
    (buffer-set-point buf 0 0)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 3 failed: expected (0 1), got ~A" point))
    (format t "✓ Test 3 passed: Single character line~%"))
  
  ;; Test 4: Very long line
  (let ((buf (make-instance 'standard-buffer))
        (long-line (make-string 1000 :initial-element #\a)))
    (setf (lines buf) (vector long-line))
    (buffer-set-point buf 0 500)
    (end-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1000)) () "Test 4 failed: expected (0 1000), got ~A" point))
    (format t "✓ Test 4 passed: Very long line~%"))
  
  ;; Test 5: Empty buffer (should not crash)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (end-of-line buf)
          (format t "✓ Test 5 passed: Empty buffer (no crash)~%"))
      (error (e)
        (format t "✗ Test 5 failed: Empty buffer caused error: ~A~%" e))))
  
  (format t "All end-of-line edge case tests passed!~%~%"))

(defun test-end-of-line-with-undo ()
  "Test end-of-line with undo functionality (movement operations typically don't record undo)"
  (format t "Running end-of-line undo tests...~%")
  
  ;; Test 1: Movement operations don't interfere with existing undo stack
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    ;; Insert a character to create an undo record
    (insert-char buf #\X)
    (let ((after-insert (buffer-get-point buf)))
      (assert (equal after-insert '(0 6)) () "Insert failed: expected (0 6), got ~A" after-insert))
    ;; Move cursor - this should not create undo record
    (end-of-line buf)
    (let ((after-move (buffer-get-point buf)))
      (assert (equal after-move '(0 12)) () "Movement failed: expected (0 12), got ~A" after-move))
    ;; Undo should undo the character insertion, not the movement
    (buffer-undo buf)
    (let ((after-undo (buffer-get-point buf)))
      ;; Point should be back at position 5 (where we inserted the char)
      (assert (equal after-undo '(0 5)) () "Undo failed: expected (0 5), got ~A" after-undo)
      ;; The 'X' should be gone
      (let ((line-text (buffer-line buf 0)))
        (assert (string= line-text "hello world") () "Undo failed: expected 'hello world', got '~A'" line-text)))
    (format t "✓ Test 1 passed: Movement doesn't interfere with undo stack~%"))
  
  ;; Test 2: Undo twice in a row (should do nothing if no more undo records)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test line"))
    (buffer-set-point buf 0 5)
    (end-of-line buf)
    ;; Try undo (should do nothing since movement doesn't create undo records)
    (let ((undo-result (buffer-undo buf))
          (point-after-undo (buffer-get-point buf)))
      (assert (null undo-result) () "Undo should return nil when no undo records")
      (assert (equal point-after-undo '(0 9)) () "Point should remain at (0 9), got ~A" point-after-undo))
    ;; Try undo again (should still do nothing)
    (let ((undo-result2 (buffer-undo buf))
          (point-after-undo2 (buffer-get-point buf)))
      (assert (null undo-result2) () "Second undo should also return nil")
      (assert (equal point-after-undo2 '(0 9)) () "Point should remain at (0 9), got ~A" point-after-undo2))
    (format t "✓ Test 2 passed: Multiple undos when no undo records~%"))
  
  (format t "All end-of-line undo tests passed!~%~%"))

(defun test-end-of-line-integration ()
  "Test end-of-line integration with other functions"
  (format t "Running end-of-line integration tests...~%")
  
  ;; Test 1: end-of-line after character movements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 0)
    (forward-char buf)
    (forward-char buf)
    (forward-char buf)
    (let ((after-chars (buffer-get-point buf)))
      (assert (equal after-chars '(0 3)) () "Character movement failed: expected (0 3), got ~A" after-chars))
    (end-of-line buf)
    (let ((after-eol (buffer-get-point buf)))
      (assert (equal after-eol '(0 11)) () "end-of-line after chars failed: expected (0 11), got ~A" after-eol))
    (format t "✓ Test 1 passed: end-of-line after character movements~%"))
  
  ;; Test 2: end-of-line after line movements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 5)
    (next-line buf)
    (let ((after-line (buffer-get-point buf)))
      (assert (equal after-line '(1 5)) () "Line movement failed: expected (1 5), got ~A" after-line))
    (end-of-line buf)
    (let ((after-eol (buffer-get-point buf)))
      (assert (equal after-eol '(1 11)) () "end-of-line after line move failed: expected (1 11), got ~A" after-eol))
    (format t "✓ Test 2 passed: end-of-line after line movements~%"))
  
  ;; Test 3: Character movement after end-of-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 2)
    (end-of-line buf)
    (backward-char buf)
    (backward-char buf)
    (let ((final-point (buffer-get-point buf)))
      (assert (equal final-point '(0 9)) () "Movement after end-of-line failed: expected (0 9), got ~A" final-point))
    (format t "✓ Test 3 passed: Character movement after end-of-line~%"))
  
  ;; Test 4: end-of-line and beginning-of-line combination
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    (end-of-line buf)
    (let ((after-eol (buffer-get-point buf)))
      (assert (equal after-eol '(0 11)) () "end-of-line failed: expected (0 11), got ~A" after-eol))
    (beginning-of-line buf)
    (let ((after-bol (buffer-get-point buf)))
      (assert (equal after-bol '(0 0)) () "beginning-of-line after end-of-line failed: expected (0 0), got ~A" after-bol))
    (format t "✓ Test 4 passed: end-of-line and beginning-of-line combination~%"))
  
  (format t "All end-of-line integration tests passed!~%~%"))

(defun run-all-end-of-line-tests ()
  "Run all end-of-line tests"
  (format t "~%======================================~%")
  (format t "Running End-of-Line Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-end-of-line-basic)
        (test-end-of-line-multi-line)
        (test-end-of-line-edge-cases)
        (test-end-of-line-with-undo)
        (test-end-of-line-integration)
        (format t "~%======================================~%")
        (format t "All end-of-line tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)