(in-package :tle)

(defun test-move-beginning-of-line-basic ()
  "Test basic move-beginning-of-line functionality"
  (format t "Running move-beginning-of-line basic tests...~%")
  
  ;; Test 1: From middle of line to first non-whitespace
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "  hello world"))
    (buffer-set-point buf 0 8)  ; Position at 'o' in "hello"
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 1 failed: expected (0 2), got ~A" point))
    (format t "✓ Test 1 passed: Move from middle to first non-whitespace~%"))
  
  ;; Test 2: From first non-whitespace to beginning
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "  hello world"))
    (buffer-set-point buf 0 2)  ; Position at first non-whitespace
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Move from first non-whitespace to beginning~%"))
  
  ;; Test 3: From beginning to first non-whitespace
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "  hello world"))
    (buffer-set-point buf 0 0)  ; Position at beginning
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 3 failed: expected (0 2), got ~A" point))
    (format t "✓ Test 3 passed: Move from beginning to first non-whitespace~%"))
  
  ;; Test 4: Line without leading whitespace
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)  ; Position at space
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: Line without leading whitespace~%"))
  
  (format t "All move-beginning-of-line basic tests passed!~%~%"))

(defun test-move-beginning-of-line-whitespace-variations ()
  "Test move-beginning-of-line with different whitespace patterns"
  (format t "Running move-beginning-of-line whitespace variation tests...~%")
  
  ;; Test 1: Spaces and tabs mixed
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector (format nil "~C~C  ~Chello" #\Space #\Tab #\Tab)))
    (buffer-set-point buf 0 10)  ; Somewhere after the text
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: Mixed spaces and tabs~%"))
  
  ;; Test 2: Only whitespace line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "    "))
    (buffer-set-point buf 0 2)
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Only whitespace line~%"))
  
  ;; Test 3: Single space at beginning
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector " hello"))
    (buffer-set-point buf 0 3)
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 3a failed: expected (0 1), got ~A" point))
    ;; Test toggle back
    (move-beginning-of-line buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(0 0)) () "Test 3b failed: expected (0 0), got ~A" point2))
    (format t "✓ Test 3 passed: Single space at beginning~%"))
  
  ;; Test 4: Tab-indented line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector (format nil "~C~Cfunction()" #\Tab #\Tab)))
    (buffer-set-point buf 0 5)
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 4 failed: expected (0 2), got ~A" point))
    (format t "✓ Test 4 passed: Tab-indented line~%"))
  
  (format t "All move-beginning-of-line whitespace variation tests passed!~%~%"))

(defun test-move-beginning-of-line-multi-line ()
  "Test move-beginning-of-line on different lines"
  (format t "Running move-beginning-of-line multi-line tests...~%")
  
  ;; Test 1: Different lines with different indentation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "  first line" "    second line" "third line"))
    ;; Test first line
    (buffer-set-point buf 0 8)
    (move-beginning-of-line buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(0 2)) () "Test 1a failed: expected (0 2), got ~A" point1))
    ;; Test second line
    (buffer-set-point buf 1 10)
    (move-beginning-of-line buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(1 4)) () "Test 1b failed: expected (1 4), got ~A" point2))
    ;; Test third line (no indentation)
    (buffer-set-point buf 2 5)
    (move-beginning-of-line buf)
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(2 0)) () "Test 1c failed: expected (2 0), got ~A" point3))
    (format t "✓ Test 1 passed: Different lines with different indentation~%"))
  
  (format t "All move-beginning-of-line multi-line tests passed!~%~%"))

(defun test-move-beginning-of-line-edge-cases ()
  "Test move-beginning-of-line edge cases"
  (format t "Running move-beginning-of-line edge case tests...~%")
  
  ;; Test 1: Empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ""))
    (buffer-set-point buf 0 0)
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Empty line~%"))
  
  ;; Test 2: Single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a"))
    (buffer-set-point buf 0 1)
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Single character line~%"))
  
  ;; Test 3: Line starting with single non-whitespace
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a   "))
    (buffer-set-point buf 0 2)
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Line starting with single non-whitespace~%"))
  
  ;; Test 4: Empty buffer (should not crash)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (move-beginning-of-line buf)
          (format t "✓ Test 4 passed: Empty buffer (no crash)~%"))
      (error (e)
        (format t "✗ Test 4 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 5: Very long line with late indentation
  (let ((buf (make-instance 'standard-buffer))
        (long-line (format nil "~A~A" (make-string 1000 :initial-element #\Space) "content")))
    (setf (lines buf) (vector long-line))
    (buffer-set-point buf 0 1005)
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1000)) () "Test 5 failed: expected (0 1000), got ~A" point))
    (format t "✓ Test 5 passed: Very long line with late indentation~%"))
  
  (format t "All move-beginning-of-line edge case tests passed!~%~%"))

(defun test-move-beginning-of-line-with-undo ()
  "Test move-beginning-of-line with undo functionality"
  (format t "Running move-beginning-of-line undo tests...~%")
  
  ;; Test 1: Movement operations don't interfere with existing undo stack
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "  hello world"))
    (buffer-set-point buf 0 8)
    ;; Insert a character to create an undo record
    (insert-char buf #\X)
    (let ((after-insert (buffer-get-point buf)))
      (assert (equal after-insert '(0 9)) () "Insert failed: expected (0 9), got ~A" after-insert))
    ;; Move cursor - this should not create undo record
    (move-beginning-of-line buf)
    (let ((after-move (buffer-get-point buf)))
      (assert (equal after-move '(0 2)) () "Movement failed: expected (0 2), got ~A" after-move))
    ;; Undo should undo the character insertion, not the movement
    (buffer-undo buf)
    (let ((after-undo (buffer-get-point buf)))
      ;; Point should be back at position 8 (where we inserted the char)
      (assert (equal after-undo '(0 8)) () "Undo failed: expected (0 8), got ~A" after-undo)
      ;; The 'X' should be gone
      (let ((line-text (aref (lines buf) 0)))
        (assert (string= line-text "  hello world") () "Undo failed: expected '  hello world', got '~A'" line-text)))
    (format t "✓ Test 1 passed: Movement doesn't interfere with undo stack~%"))
  
  ;; Test 2: Undo twice in a row
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "  test line"))
    (buffer-set-point buf 0 5)
    (move-beginning-of-line buf)
    ;; Try undo (should do nothing since movement doesn't create undo records)
    (let ((undo-result (buffer-undo buf))
          (point-after-undo (buffer-get-point buf)))
      (assert (null undo-result) () "Undo should return nil when no undo records")
      (assert (equal point-after-undo '(0 2)) () "Point should remain at (0 2), got ~A" point-after-undo))
    ;; Try undo again (should still do nothing)
    (let ((undo-result2 (buffer-undo buf))
          (point-after-undo2 (buffer-get-point buf)))
      (assert (null undo-result2) () "Second undo should also return nil")
      (assert (equal point-after-undo2 '(0 2)) () "Point should remain at (0 2), got ~A" point-after-undo2))
    (format t "✓ Test 2 passed: Multiple undos when no undo records~%"))
  
  (format t "All move-beginning-of-line undo tests passed!~%~%"))

(defun test-move-beginning-of-line-integration ()
  "Test move-beginning-of-line integration with other functions"
  (format t "Running move-beginning-of-line integration tests...~%")
  
  ;; Test 1: Toggle behavior test
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "    hello world"))
    (buffer-set-point buf 0 8)  ; Middle of line
    ;; First call: should go to first non-whitespace
    (move-beginning-of-line buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(0 4)) () "First move failed: expected (0 4), got ~A" point1))
    ;; Second call: should go to beginning
    (move-beginning-of-line buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(0 0)) () "Second move failed: expected (0 0), got ~A" point2))
    ;; Third call: should go back to first non-whitespace
    (move-beginning-of-line buf)
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(0 4)) () "Third move failed: expected (0 4), got ~A" point3))
    (format t "✓ Test 1 passed: Toggle behavior~%"))
  
  ;; Test 2: Integration with other movement functions
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "    hello world"))
    (buffer-set-point buf 0 8)
    (forward-char buf)
    (move-beginning-of-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "After forward-char + move-beginning-of-line failed: expected (0 4), got ~A" point))
    (format t "✓ Test 2 passed: Integration with forward-char~%"))
  
  ;; Test 3: Comparison with beginning-of-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "    hello world"))
    (buffer-set-point buf 0 8)
    ;; Use beginning-of-line
    (beginning-of-line buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(0 0)) () "beginning-of-line failed: expected (0 0), got ~A" point1))
    ;; Use move-beginning-of-line from position 0
    (move-beginning-of-line buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(0 4)) () "move-beginning-of-line from 0 failed: expected (0 4), got ~A" point2))
    (format t "✓ Test 3 passed: Comparison with beginning-of-line~%"))
  
  (format t "All move-beginning-of-line integration tests passed!~%~%"))

(defun run-all-move-beginning-of-line-tests ()
  "Run all move-beginning-of-line tests"
  (format t "~%======================================~%")
  (format t "Running Move-Beginning-of-Line Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-move-beginning-of-line-basic)
        (test-move-beginning-of-line-whitespace-variations)
        (test-move-beginning-of-line-multi-line)
        (test-move-beginning-of-line-edge-cases)
        (test-move-beginning-of-line-with-undo)
        (test-move-beginning-of-line-integration)
        (format t "~%======================================~%")
        (format t "All move-beginning-of-line tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)