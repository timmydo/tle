(in-package :tle)

(defun test-end-of-buffer-basic ()
  "Test basic end-of-buffer functionality"
  (format t "Running end-of-buffer basic tests...~%")
  
  ;; Test 1: From middle of buffer to end
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)  ; Middle of second line
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 10)) () "Test 1 failed: expected (2 10), got ~A" point))
    (format t "✓ Test 1 passed: Move from middle of buffer to end~%"))
  
  ;; Test 2: From beginning of buffer to end
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 0)  ; Beginning of first line
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 10)) () "Test 2 failed: expected (2 10), got ~A" point))
    (format t "✓ Test 2 passed: Move from beginning of buffer to end~%"))
  
  ;; Test 3: Already at end of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 2 10)  ; Already at end
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 10)) () "Test 3 failed: expected (2 10), got ~A" point))
    (format t "✓ Test 3 passed: Already at end of buffer~%"))
  
  ;; Test 4: From middle of last line to end
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)  ; Middle of only line
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 4 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 4 passed: Move from middle of last line to end~%"))
  
  (format t "All end-of-buffer basic tests passed!~%~%"))

(defun test-end-of-buffer-single-line ()
  "Test end-of-buffer with single line buffer"
  (format t "Running end-of-buffer single line tests...~%")
  
  ;; Test 1: Single line buffer, beginning position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 0)
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 1 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 1 passed: Single line buffer from beginning~%"))
  
  ;; Test 2: Single line buffer, middle position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 6)
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 2 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 2 passed: Single line buffer from middle~%"))
  
  ;; Test 3: Single character buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a"))
    (buffer-set-point buf 0 0)
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 3 failed: expected (0 1), got ~A" point))
    (format t "✓ Test 3 passed: Single character buffer~%"))
  
  (format t "All end-of-buffer single line tests passed!~%~%"))

(defun test-end-of-buffer-multi-line ()
  "Test end-of-buffer with multi-line buffers"
  (format t "Running end-of-buffer multi-line tests...~%")
  
  ;; Test 1: Various positions in multi-line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line 1" "line 2" "line 3" "line 4" "line 5"))
    
    ;; From first line
    (buffer-set-point buf 0 3)
    (end-of-buffer buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(4 6)) () "Test 1a failed: expected (4 6), got ~A" point1))
    
    ;; From second line
    (buffer-set-point buf 1 2)
    (end-of-buffer buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(4 6)) () "Test 1b failed: expected (4 6), got ~A" point2))
    
    ;; From middle line
    (buffer-set-point buf 2 4)
    (end-of-buffer buf)
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(4 6)) () "Test 1c failed: expected (4 6), got ~A" point3))
    
    (format t "✓ Test 1 passed: Various positions in multi-line buffer~%"))
  
  ;; Test 2: Very large buffer
  (let ((buf (make-instance 'standard-buffer))
        (many-lines (make-array 100)))
    (dotimes (i 100)
      (setf (aref many-lines i) (format nil "Line ~A content" i)))
    (setf (lines buf) many-lines)
    (buffer-set-point buf 0 0)  ; First line
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf))
          (expected-line (1- (length many-lines)))
          (expected-col (length (aref many-lines (1- (length many-lines))))))
      (assert (equal point (list expected-line expected-col)) () "Test 2 failed: expected (~A ~A), got ~A" expected-line expected-col point))
    (format t "✓ Test 2 passed: Very large buffer~%"))
  
  (format t "All end-of-buffer multi-line tests passed!~%~%"))

(defun test-end-of-buffer-edge-cases ()
  "Test end-of-buffer edge cases"
  (format t "Running end-of-buffer edge case tests...~%")
  
  ;; Test 1: Empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (end-of-buffer buf)
          (let ((point (buffer-get-point buf)))
            (assert (equal point '(0 0)) () "Empty buffer: expected (0 0), got ~A" point))
          (format t "✓ Test 1 passed: Empty buffer~%"))
      (error (e)
        (format t "✗ Test 1 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 2: Single empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ""))
    (buffer-set-point buf 0 0)
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Single empty line~%"))
  
  ;; Test 3: Multiple empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "" ""))
    (buffer-set-point buf 0 0)  ; First empty line
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () "Test 3 failed: expected (2 0), got ~A" point))
    (format t "✓ Test 3 passed: Multiple empty lines~%"))
  
  ;; Test 4: Mixed empty and non-empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "content" "" "more content" "" "final"))
    (buffer-set-point buf 0 0)  ; Beginning of first line
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(4 5)) () "Test 4 failed: expected (4 5), got ~A" point))
    (format t "✓ Test 4 passed: Mixed empty and non-empty lines~%"))
  
  ;; Test 5: Very long lines
  (let ((buf (make-instance 'standard-buffer))
        (long-line (make-string 1000 :initial-element #\a)))
    (setf (lines buf) (vector long-line "second line"))
    (buffer-set-point buf 0 0)  ; First line beginning
    (end-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 11)) () "Test 5 failed: expected (1 11), got ~A" point))
    (format t "✓ Test 5 passed: Very long lines~%"))
  
  (format t "All end-of-buffer edge case tests passed!~%~%"))

(defun test-end-of-buffer-with-undo ()
  "Test end-of-buffer with undo functionality"
  (format t "Running end-of-buffer undo tests...~%")
  
  ;; Test 1: Movement operations don't interfere with existing undo stack
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 0 5)
    ;; Insert a character to create an undo record
    (insert-char buf #\X)
    (let ((after-insert (buffer-get-point buf)))
      (assert (equal after-insert '(0 6)) () "Insert failed: expected (0 6), got ~A" after-insert))
    ;; Move cursor - this should not create undo record
    (end-of-buffer buf)
    (let ((after-move (buffer-get-point buf)))
      (assert (equal after-move '(1 11)) () "Movement failed: expected (1 11), got ~A" after-move))
    ;; Undo should undo the character insertion, not the movement
    (buffer-undo buf)
    (let ((after-undo (buffer-get-point buf)))
      ;; Point should be back at position where we inserted the char
      (assert (equal after-undo '(0 5)) () "Undo failed: expected (0 5), got ~A" after-undo)
      ;; The 'X' should be gone
      (let ((line-text (aref (lines buf) 0)))
        (assert (string= line-text "first line") () "Undo failed: expected 'first line', got '~A'" line-text)))
    (format t "✓ Test 1 passed: Movement doesn't interfere with undo stack~%"))
  
  ;; Test 2: Undo twice in a row
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 0 5)
    (end-of-buffer buf)
    ;; Try undo (should do nothing since movement doesn't create undo records)
    (let ((undo-result (buffer-undo buf))
          (point-after-undo (buffer-get-point buf)))
      (assert (null undo-result) () "Undo should return nil when no undo records")
      (assert (equal point-after-undo '(1 11)) () "Point should remain at (1 11), got ~A" point-after-undo))
    ;; Try undo again (should still do nothing)
    (let ((undo-result2 (buffer-undo buf))
          (point-after-undo2 (buffer-get-point buf)))
      (assert (null undo-result2) () "Second undo should also return nil")
      (assert (equal point-after-undo2 '(1 11)) () "Point should remain at (1 11), got ~A" point-after-undo2))
    (format t "✓ Test 2 passed: Multiple undos when no undo records~%"))
  
  (format t "All end-of-buffer undo tests passed!~%~%"))

(defun test-end-of-buffer-integration ()
  "Test end-of-buffer integration with other functions"
  (format t "Running end-of-buffer integration tests...~%")
  
  ;; Test 1: end-of-buffer after various movements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)
    (backward-char buf)
    (previous-line buf)
    (backward-char buf)
    (backward-char buf)
    (let ((before-eob (buffer-get-point buf)))
      (assert (equal before-eob '(0 2)) () "Setup failed: expected (0 2), got ~A" before-eob))
    (end-of-buffer buf)
    (let ((after-eob (buffer-get-point buf)))
      (assert (equal after-eob '(2 10)) () "end-of-buffer failed: expected (2 10), got ~A" after-eob))
    (format t "✓ Test 1 passed: end-of-buffer after various movements~%"))
  
  ;; Test 2: Character movement after end-of-buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 0 5)
    (end-of-buffer buf)
    (backward-char buf)
    (backward-char buf)
    (backward-char buf)
    (let ((final-point (buffer-get-point buf)))
      (assert (equal final-point '(1 8)) () "Movement after end-of-buffer failed: expected (1 8), got ~A" final-point))
    (format t "✓ Test 2 passed: Character movement after end-of-buffer~%"))
  
  ;; Test 3: Line movement after end-of-buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 2)
    (end-of-buffer buf)
    (previous-line buf)
    (let ((after-line-move (buffer-get-point buf)))
      (assert (equal after-line-move '(1 10)) () "Line movement after end-of-buffer failed: expected (1 10), got ~A" after-line-move))
    (format t "✓ Test 3 passed: Line movement after end-of-buffer~%"))
  
  ;; Test 4: Comparison with end-of-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line  " "second line  "))
    (buffer-set-point buf 0 5)
    ;; Use end-of-line (should go to end of current line)
    (end-of-line buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(0 12)) () "end-of-line failed: expected (0 12), got ~A" point1))
    ;; Move back and use end-of-buffer (should go to end of buffer)
    (buffer-set-point buf 0 5)
    (end-of-buffer buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(1 13)) () "end-of-buffer failed: expected (1 13), got ~A" point2))
    (format t "✓ Test 4 passed: Comparison with end-of-line~%"))
  
  ;; Test 5: Interaction with beginning-of-buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)
    ;; Go to end
    (end-of-buffer buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(2 10)) () "end-of-buffer failed: expected (2 10), got ~A" point1))
    ;; Go to beginning
    (beginning-of-buffer buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(0 0)) () "beginning-of-buffer failed: expected (0 0), got ~A" point2))
    ;; Go to end again
    (end-of-buffer buf)
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(2 10)) () "second end-of-buffer failed: expected (2 10), got ~A" point3))
    (format t "✓ Test 5 passed: Interaction with beginning-of-buffer~%"))
  
  (format t "All end-of-buffer integration tests passed!~%~%"))

(defun run-all-end-of-buffer-tests ()
  "Run all end-of-buffer tests"
  (format t "~%======================================~%")
  (format t "Running End-of-Buffer Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-end-of-buffer-basic)
        (test-end-of-buffer-single-line)
        (test-end-of-buffer-multi-line)
        (test-end-of-buffer-edge-cases)
        (test-end-of-buffer-with-undo)
        (test-end-of-buffer-integration)
        (format t "~%======================================~%")
        (format t "All end-of-buffer tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)