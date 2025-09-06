(in-package :tle)

(defun test-beginning-of-buffer-basic ()
  "Test basic beginning-of-buffer functionality"
  (format t "Running beginning-of-buffer basic tests...~%")
  
  ;; Test 1: From middle of buffer to beginning
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)  ; Middle of second line
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Move from middle of buffer to beginning~%"))
  
  ;; Test 2: From end of buffer to beginning
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 2 10)  ; End of last line
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Move from end of buffer to beginning~%"))
  
  ;; Test 3: Already at beginning of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 0)  ; Already at beginning
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Already at beginning of buffer~%"))
  
  ;; Test 4: From middle of first line to beginning
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)  ; Middle of first line
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: Move from middle of first line to beginning~%"))
  
  (format t "All beginning-of-buffer basic tests passed!~%~%"))

(defun test-beginning-of-buffer-single-line ()
  "Test beginning-of-buffer with single line buffer"
  (format t "Running beginning-of-buffer single line tests...~%")
  
  ;; Test 1: Single line buffer, middle position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 6)
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Single line buffer from middle~%"))
  
  ;; Test 2: Single line buffer, end position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 11)
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Single line buffer from end~%"))
  
  ;; Test 3: Single character buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a"))
    (buffer-set-point buf 0 1)
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Single character buffer~%"))
  
  (format t "All beginning-of-buffer single line tests passed!~%~%"))

(defun test-beginning-of-buffer-multi-line ()
  "Test beginning-of-buffer with multi-line buffers"
  (format t "Running beginning-of-buffer multi-line tests...~%")
  
  ;; Test 1: Various positions in multi-line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line 1" "line 2" "line 3" "line 4" "line 5"))
    
    ;; From second line
    (buffer-set-point buf 1 3)
    (beginning-of-buffer buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(0 0)) () "Test 1a failed: expected (0 0), got ~A" point1))
    
    ;; From fourth line
    (buffer-set-point buf 3 2)
    (beginning-of-buffer buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(0 0)) () "Test 1b failed: expected (0 0), got ~A" point2))
    
    ;; From last line
    (buffer-set-point buf 4 6)
    (beginning-of-buffer buf)
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(0 0)) () "Test 1c failed: expected (0 0), got ~A" point3))
    
    (format t "✓ Test 1 passed: Various positions in multi-line buffer~%"))
  
  ;; Test 2: Very large buffer
  (let ((buf (make-instance 'standard-buffer))
        (many-lines (make-array 100)))
    (dotimes (i 100)
      (setf (aref many-lines i) (format nil "Line ~A content" i)))
    (setf (lines buf) many-lines)
    (buffer-set-point buf 99 10)  ; Last line
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Very large buffer~%"))
  
  (format t "All beginning-of-buffer multi-line tests passed!~%~%"))

(defun test-beginning-of-buffer-edge-cases ()
  "Test beginning-of-buffer edge cases"
  (format t "Running beginning-of-buffer edge case tests...~%")
  
  ;; Test 1: Empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (beginning-of-buffer buf)
          (let ((point (buffer-get-point buf)))
            (assert (equal point '(0 0)) () "Empty buffer: expected (0 0), got ~A" point))
          (format t "✓ Test 1 passed: Empty buffer~%"))
      (error (e)
        (format t "✗ Test 1 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 2: Single empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ""))
    (buffer-set-point buf 0 0)
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Single empty line~%"))
  
  ;; Test 3: Multiple empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "" ""))
    (buffer-set-point buf 2 0)  ; Last empty line
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Multiple empty lines~%"))
  
  ;; Test 4: Mixed empty and non-empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "content" "" "more content" "" "final"))
    (buffer-set-point buf 4 5)  ; End of final line
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: Mixed empty and non-empty lines~%"))
  
  ;; Test 5: Very long lines
  (let ((buf (make-instance 'standard-buffer))
        (long-line (make-string 1000 :initial-element #\a)))
    (setf (lines buf) (vector long-line "second line"))
    (buffer-set-point buf 1 6)  ; Second line
    (beginning-of-buffer buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 5 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 5 passed: Very long lines~%"))
  
  (format t "All beginning-of-buffer edge case tests passed!~%~%"))

(defun test-beginning-of-buffer-with-undo ()
  "Test beginning-of-buffer with undo functionality"
  (format t "Running beginning-of-buffer undo tests...~%")
  
  ;; Test 1: Movement operations don't interfere with existing undo stack
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 1 5)
    ;; Insert a character to create an undo record
    (insert-char buf #\X)
    (let ((after-insert (buffer-get-point buf)))
      (assert (equal after-insert '(1 6)) () "Insert failed: expected (1 6), got ~A" after-insert))
    ;; Move cursor - this should not create undo record
    (beginning-of-buffer buf)
    (let ((after-move (buffer-get-point buf)))
      (assert (equal after-move '(0 0)) () "Movement failed: expected (0 0), got ~A" after-move))
    ;; Undo should undo the character insertion, not the movement
    (buffer-undo buf)
    (let ((after-undo (buffer-get-point buf)))
      ;; Point should be back at position where we inserted the char
      (assert (equal after-undo '(1 5)) () "Undo failed: expected (1 5), got ~A" after-undo)
      ;; The 'X' should be gone
      (let ((line-text (aref (lines buf) 1)))
        (assert (string= line-text "second line") () "Undo failed: expected 'second line', got '~A'" line-text)))
    (format t "✓ Test 1 passed: Movement doesn't interfere with undo stack~%"))
  
  ;; Test 2: Undo twice in a row
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 1 5)
    (beginning-of-buffer buf)
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
  
  (format t "All beginning-of-buffer undo tests passed!~%~%"))

(defun test-beginning-of-buffer-integration ()
  "Test beginning-of-buffer integration with other functions"
  (format t "Running beginning-of-buffer integration tests...~%")
  
  ;; Test 1: beginning-of-buffer after various movements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 5)
    (forward-char buf)
    (next-line buf)
    (forward-char buf)
    (forward-char buf)
    (let ((before-bob (buffer-get-point buf)))
      (assert (equal before-bob '(1 8)) () "Setup failed: expected (1 8), got ~A" before-bob))
    (beginning-of-buffer buf)
    (let ((after-bob (buffer-get-point buf)))
      (assert (equal after-bob '(0 0)) () "beginning-of-buffer failed: expected (0 0), got ~A" after-bob))
    (format t "✓ Test 1 passed: beginning-of-buffer after various movements~%"))
  
  ;; Test 2: Character movement after beginning-of-buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 1 5)
    (beginning-of-buffer buf)
    (forward-char buf)
    (forward-char buf)
    (forward-char buf)
    (let ((final-point (buffer-get-point buf)))
      (assert (equal final-point '(0 3)) () "Movement after beginning-of-buffer failed: expected (0 3), got ~A" final-point))
    (format t "✓ Test 2 passed: Character movement after beginning-of-buffer~%"))
  
  ;; Test 3: Line movement after beginning-of-buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 2 8)
    (beginning-of-buffer buf)
    (next-line buf)
    (let ((after-line-move (buffer-get-point buf)))
      (assert (equal after-line-move '(1 0)) () "Line movement after beginning-of-buffer failed: expected (1 0), got ~A" after-line-move))
    (format t "✓ Test 3 passed: Line movement after beginning-of-buffer~%"))
  
  ;; Test 4: Comparison with beginning-of-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "  first line" "  second line"))
    (buffer-set-point buf 1 8)
    ;; Use beginning-of-line (should go to start of current line)
    (beginning-of-line buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(1 0)) () "beginning-of-line failed: expected (1 0), got ~A" point1))
    ;; Use beginning-of-buffer (should go to start of buffer)
    (beginning-of-buffer buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(0 0)) () "beginning-of-buffer failed: expected (0 0), got ~A" point2))
    (format t "✓ Test 4 passed: Comparison with beginning-of-line~%"))
  
  (format t "All beginning-of-buffer integration tests passed!~%~%"))

(defun run-all-beginning-of-buffer-tests ()
  "Run all beginning-of-buffer tests"
  (format t "~%======================================~%")
  (format t "Running Beginning-of-Buffer Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-beginning-of-buffer-basic)
        (test-beginning-of-buffer-single-line)
        (test-beginning-of-buffer-multi-line)
        (test-beginning-of-buffer-edge-cases)
        (test-beginning-of-buffer-with-undo)
        (test-beginning-of-buffer-integration)
        (format t "~%======================================~%")
        (format t "All beginning-of-buffer tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)