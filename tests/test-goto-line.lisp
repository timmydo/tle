(in-package :tle)

(defun test-goto-line-basic ()
  "Test basic goto-line functionality"
  (format t "Running goto-line basic tests...~%")
  
  ;; Test 1: Go to first line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 2 5)  ; Start on third line
    (goto-line buf 1)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Go to first line~%"))
  
  ;; Test 2: Go to middle line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 5)  ; Start on first line
    (goto-line buf 2)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 2 failed: expected (1 0), got ~A" point))
    (format t "✓ Test 2 passed: Go to middle line~%"))
  
  ;; Test 3: Go to last line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 5)  ; Start on first line
    (goto-line buf 3)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () "Test 3 failed: expected (2 0), got ~A" point))
    (format t "✓ Test 3 passed: Go to last line~%"))
  
  ;; Test 4: Go to same line (no-op)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)  ; Start on second line at column 5
    (goto-line buf 2)  ; Go to second line (1-indexed)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 4 failed: expected (1 0), got ~A" point))
    (format t "✓ Test 4 passed: Go to same line (moves to beginning)~%"))
  
  (format t "All goto-line basic tests passed!~%~%"))

(defun test-goto-line-edge-cases ()
  "Test goto-line edge cases"
  (format t "Running goto-line edge case tests...~%")
  
  ;; Test 1: Line number beyond buffer size (should go to last line)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 0)
    (goto-line buf 100)  ; Way beyond buffer size
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () "Test 1 failed: expected (2 0), got ~A" point))
    (format t "✓ Test 1 passed: Line number beyond buffer size~%"))
  
  ;; Test 2: Line number 0 (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)  ; Start position
    (goto-line buf 0)  ; Invalid line number
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Test 2 failed: expected (1 5), got ~A" point))
    (format t "✓ Test 2 passed: Line number 0 (no change)~%"))
  
  ;; Test 3: Negative line number (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)  ; Start position
    (goto-line buf -5)  ; Negative line number
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Test 3 failed: expected (1 5), got ~A" point))
    (format t "✓ Test 3 passed: Negative line number (no change)~%"))
  
  ;; Test 4: Non-integer line number (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)  ; Start position
    (goto-line buf 2.5)  ; Non-integer line number
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Test 4 failed: expected (1 5), got ~A" point))
    (format t "✓ Test 4 passed: Non-integer line number (no change)~%"))
  
  ;; Test 5: Empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (goto-line buf 1)
          (let ((point (buffer-get-point buf)))
            (assert (equal point '(0 0)) () "Empty buffer: expected (0 0), got ~A" point))
          (format t "✓ Test 5 passed: Empty buffer (no change)~%"))
      (error (e)
        (format t "✗ Test 5 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 6: Single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "only line"))
    (buffer-set-point buf 0 5)
    (goto-line buf 1)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 6 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 6 passed: Single line buffer~%"))
  
  (format t "All goto-line edge case tests passed!~%~%"))

(defun test-goto-line-with-undo ()
  "Test goto-line with undo functionality"
  (format t "Running goto-line undo tests...~%")
  
  ;; Test 1: Movement operations don't interfere with existing undo stack
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)
    ;; Insert a character to create an undo record
    (insert-char buf #\X)
    (let ((after-insert (buffer-get-point buf)))
      (assert (equal after-insert '(1 6)) () "Insert failed: expected (1 6), got ~A" after-insert))
    ;; Move cursor with goto-line - this should not create undo record
    (goto-line buf 3)
    (let ((after-move (buffer-get-point buf)))
      (assert (equal after-move '(2 0)) () "Movement failed: expected (2 0), got ~A" after-move))
    ;; Undo should undo the character insertion, not the movement
    (buffer-undo buf)
    (let ((after-undo (buffer-get-point buf)))
      ;; Point should be back at position where we inserted the char
      (assert (equal after-undo '(1 5)) () "Undo failed: expected (1 5), got ~A" after-undo)
      ;; The 'X' should be gone
      (let ((line-text (aref (lines buf) 1)))
        (assert (string= line-text "second line") () "Undo failed: expected 'second line', got '~A'" line-text)))
    (format t "✓ Test 1 passed: Movement doesn't interfere with undo stack~%"))
  
  ;; Test 2: Undo twice in a row after goto-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)
    (goto-line buf 1)
    ;; Try undo (should do nothing since movement doesn't create undo records)
    (let ((undo-result (buffer-undo buf))
          (point-after-undo (buffer-get-point buf)))
      (assert (null undo-result) () "Undo should return nil when no undo records")
      (assert (equal point-after-undo '(0 0)) () "Point should remain at (0 0), got ~A" point-after-undo))
    ;; Try undo again (should still do nothing)
    (let ((undo-result2 (buffer-undo buf))
          (point-after-undo2 (buffer-get-point buf)))
      (assert (null undo-result2) () "Second undo should also return nil")
      (assert (equal point-after-undo2 '(0 0)) () "Point should remain at (0 0), got ~A" point-after-undo2))
    (format t "✓ Test 2 passed: Multiple undos when no undo records~%"))
  
  ;; Test 3: Redo twice in a row after goto-line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)
    (goto-line buf 2)
    ;; Try redo (should do nothing since movement doesn't create undo/redo records)
    (let ((redo-result (buffer-redo buf))
          (point-after-redo (buffer-get-point buf)))
      (assert (null redo-result) () "Redo should return nil when no redo records")
      (assert (equal point-after-redo '(1 0)) () "Point should remain at (1 0), got ~A" point-after-redo))
    ;; Try redo again (should still do nothing)
    (let ((redo-result2 (buffer-redo buf))
          (point-after-redo2 (buffer-get-point buf)))
      (assert (null redo-result2) () "Second redo should also return nil")
      (assert (equal point-after-redo2 '(1 0)) () "Point should remain at (1 0), got ~A" point-after-redo2))
    (format t "✓ Test 3 passed: Multiple redos when no redo records~%"))
  
  (format t "All goto-line undo tests passed!~%~%"))

(defun test-goto-line-integration ()
  "Test goto-line integration with other functions"
  (format t "Running goto-line integration tests...~%")
  
  ;; Test 1: goto-line after various movements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line" "fourth line"))
    (buffer-set-point buf 0 5)
    (forward-char buf)
    (next-line buf)
    (forward-char buf)
    (forward-char buf)
    (let ((before-goto (buffer-get-point buf)))
      (assert (equal before-goto '(1 8)) () "Setup failed: expected (1 8), got ~A" before-goto))
    (goto-line buf 4)
    (let ((after-goto (buffer-get-point buf)))
      (assert (equal after-goto '(3 0)) () "goto-line failed: expected (3 0), got ~A" after-goto))
    (format t "✓ Test 1 passed: goto-line after various movements~%"))
  
  ;; Test 2: Character movement after goto-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 2 5)
    (goto-line buf 1)
    (forward-char buf)
    (forward-char buf)
    (forward-char buf)
    (let ((final-point (buffer-get-point buf)))
      (assert (equal final-point '(0 3)) () "Movement after goto-line failed: expected (0 3), got ~A" final-point))
    (format t "✓ Test 2 passed: Character movement after goto-line~%"))
  
  ;; Test 3: Line movement after goto-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line" "fourth line"))
    (buffer-set-point buf 3 8)
    (goto-line buf 2)
    (next-line buf)
    (let ((after-line-move (buffer-get-point buf)))
      (assert (equal after-line-move '(2 0)) () "Line movement after goto-line failed: expected (2 0), got ~A" after-line-move))
    (format t "✓ Test 3 passed: Line movement after goto-line~%"))
  
  ;; Test 4: Multiple goto-line calls
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line 1" "line 2" "line 3" "line 4" "line 5"))
    (buffer-set-point buf 0 3)
    (goto-line buf 5)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(4 0)) () "First goto-line failed: expected (4 0), got ~A" point1))
    (goto-line buf 2)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(1 0)) () "Second goto-line failed: expected (1 0), got ~A" point2))
    (goto-line buf 1)
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(0 0)) () "Third goto-line failed: expected (0 0), got ~A" point3))
    (format t "✓ Test 4 passed: Multiple goto-line calls~%"))
  
  ;; Test 5: goto-line with text operations
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (goto-line buf 2)
    (insert-char buf #\X)
    (let ((line-text (aref (lines buf) 1)))
      (assert (string= line-text "Xsecond line") () "Insert after goto-line failed: expected 'Xsecond line', got '~A'" line-text))
    (let ((point-after-insert (buffer-get-point buf)))
      (assert (equal point-after-insert '(1 1)) () "Point after insert failed: expected (1 1), got ~A" point-after-insert))
    (format t "✓ Test 5 passed: goto-line with text operations~%"))
  
  (format t "All goto-line integration tests passed!~%~%"))

(defun test-goto-line-large-buffers ()
  "Test goto-line with large buffers"
  (format t "Running goto-line large buffer tests...~%")
  
  ;; Test 1: Large buffer navigation
  (let ((buf (make-instance 'standard-buffer))
        (many-lines (make-array 1000)))
    (dotimes (i 1000)
      (setf (aref many-lines i) (format nil "Line ~A content" (1+ i))))
    (setf (lines buf) many-lines)
    (buffer-set-point buf 0 0)
    
    ;; Go to middle
    (goto-line buf 500)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(499 0)) () "Goto middle failed: expected (499 0), got ~A" point1))
    
    ;; Go to near end
    (goto-line buf 999)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(998 0)) () "Goto near end failed: expected (998 0), got ~A" point2))
    
    ;; Go to last line
    (goto-line buf 1000)
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(999 0)) () "Goto last failed: expected (999 0), got ~A" point3))
    
    ;; Go beyond buffer (should stay at last line)
    (goto-line buf 1500)
    (let ((point4 (buffer-get-point buf)))
      (assert (equal point4 '(999 0)) () "Goto beyond failed: expected (999 0), got ~A" point4))
    
    (format t "✓ Test 1 passed: Large buffer navigation~%"))
  
  (format t "All goto-line large buffer tests passed!~%~%"))

(defun run-all-goto-line-tests ()
  "Run all goto-line tests"
  (format t "~%======================================~%")
  (format t "Running Goto-Line Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-goto-line-basic)
        (test-goto-line-edge-cases)
        (test-goto-line-with-undo)
        (test-goto-line-integration)
        (test-goto-line-large-buffers)
        (format t "~%======================================~%")
        (format t "All goto-line tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)