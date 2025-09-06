(in-package :tle)

(defun test-forward-char ()
  "Test forward-char method with various scenarios"
  (format t "Running forward-char tests...~%")
  
  ;; Test 1: Normal forward movement within a line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 5)
    (forward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 1 failed: expected (0 6), got ~A" point))
    (format t "✓ Test 1 passed: Normal forward movement within line~%"))
  
  ;; Test 2: Forward movement at end of line (should wrap to next line)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 5)  ; At end of first line
    (forward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 2 failed: expected (1 0), got ~A" point))
    (format t "✓ Test 2 passed: Forward movement wraps to next line~%"))
  
  ;; Test 3: Forward movement at end of buffer (should stay in place)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 1 5)  ; At end of last line
    (forward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Test 3 failed: expected (1 5), got ~A" point))
    (format t "✓ Test 3 passed: Forward movement at end of buffer stays in place~%"))
  
  ;; Test 4: Forward movement from middle of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "testing"))
    (buffer-set-point buf 0 3)
    (forward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "Test 4 failed: expected (0 4), got ~A" point))
    (format t "✓ Test 4 passed: Forward movement from middle of line~%"))
  
  ;; Test 5: Forward movement with empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "content"))
    (buffer-set-point buf 0 0)  ; At beginning of empty line
    (forward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 5 failed: expected (1 0), got ~A" point))
    (format t "✓ Test 5 passed: Forward movement from empty line~%"))
  
  ;; Test 6: Single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a" "b"))
    (buffer-set-point buf 0 0)
    (forward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 6 failed: expected (0 1), got ~A" point))
    (forward-char buf)  ; Should wrap to next line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 6b failed: expected (1 0), got ~A" point))
    (format t "✓ Test 6 passed: Single character line handling~%"))
  
  (format t "All forward-char tests passed!~%~%"))

(defun test-backward-char ()
  "Test backward-char method with various scenarios"
  (format t "Running backward-char tests...~%")
  
  ;; Test 1: Normal backward movement within a line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 6)
    (backward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: Normal backward movement within line~%"))
  
  ;; Test 2: Backward movement at beginning of line (should wrap to end of previous line)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 1 0)  ; At beginning of second line
    (backward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 2 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 2 passed: Backward movement wraps to end of previous line~%"))
  
  ;; Test 3: Backward movement at beginning of buffer (should stay in place)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 0)  ; At beginning of first line
    (backward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Backward movement at beginning of buffer stays in place~%"))
  
  ;; Test 4: Backward movement from middle of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "testing"))
    (buffer-set-point buf 0 4)
    (backward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 3)) () "Test 4 failed: expected (0 3), got ~A" point))
    (format t "✓ Test 4 passed: Backward movement from middle of line~%"))
  
  ;; Test 5: Backward movement to empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "content"))
    (buffer-set-point buf 1 0)  ; At beginning of second line
    (backward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 5 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 5 passed: Backward movement to empty line~%"))
  
  ;; Test 6: Single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a" "b"))
    (buffer-set-point buf 1 0)
    (backward-char buf)  ; Should wrap to end of previous line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 6 failed: expected (0 1), got ~A" point))
    (backward-char buf)  ; Should move to beginning of line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 6b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 6 passed: Single character line handling~%"))
  
  ;; Test 7: Multiple lines with varying lengths
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "short" "much longer line" "x"))
    (buffer-set-point buf 2 0)  ; Beginning of third line
    (backward-char buf)  ; Should go to end of second line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 16)) () "Test 7 failed: expected (1 16), got ~A" point))
    (format t "✓ Test 7 passed: Multiple lines with varying lengths~%"))
  
  (format t "All backward-char tests passed!~%~%"))

(defun test-combined-movements ()
  "Test combinations of forward and backward movements"
  (format t "Running combined movement tests...~%")
  
  ;; Test 1: Forward then backward should return to original position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    (let ((original-point (buffer-get-point buf)))
      (forward-char buf)
      (backward-char buf)
      (let ((final-point (buffer-get-point buf)))
        (assert (equal original-point final-point) () 
                "Test 1 failed: expected ~A, got ~A" original-point final-point)))
    (format t "✓ Test 1 passed: Forward then backward returns to original position~%"))
  
  ;; Test 2: Multiple forward movements across lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "ab" "cd" "ef"))
    (buffer-set-point buf 0 0)
    (forward-char buf)  ; (0 1)
    (forward-char buf)  ; (0 2) - at end of line
    (forward-char buf)  ; (1 0) - wrap to next line
    (forward-char buf)  ; (1 1)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 1)) () "Test 2 failed: expected (1 1), got ~A" point))
    (format t "✓ Test 2 passed: Multiple forward movements across lines~%"))
  
  ;; Test 3: Multiple backward movements across lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "ab" "cd" "ef"))
    (buffer-set-point buf 2 1)
    (backward-char buf)  ; (2 0)
    (backward-char buf)  ; (1 2) - wrap to end of previous line
    (backward-char buf)  ; (1 1)
    (backward-char buf)  ; (1 0)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 3 failed: expected (1 0), got ~A" point))
    (format t "✓ Test 3 passed: Multiple backward movements across lines~%"))
  
  (format t "All combined movement tests passed!~%~%"))

(defun test-edge-cases ()
  "Test edge cases and boundary conditions"
  (format t "Running edge case tests...~%")
  
  ;; Test 1: Single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "only line"))
    (buffer-set-point buf 0 5)
    (forward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Single line forward failed: expected (0 6), got ~A" point))
    (backward-char buf)
    (backward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "Single line backward failed: expected (0 4), got ~A" point))
    (format t "✓ Test 1 passed: Single line buffer~%"))
  
  ;; Test 2: Empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    ;; These should not crash, but behavior might be undefined
    ;; We'll catch any errors
    (handler-case
        (progn
          (forward-char buf)
          (backward-char buf)
          (format t "✓ Test 2 passed: Empty buffer (no crashes)~%"))
      (error (e)
        (format t "✗ Test 2 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 3: Buffer with only empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "" ""))
    (buffer-set-point buf 1 0)
    (forward-char buf)  ; Should go to next line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () "Empty lines forward failed: expected (2 0), got ~A" point))
    (backward-char buf)  ; Should go back to previous line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Empty lines backward failed: expected (1 0), got ~A" point))
    (format t "✓ Test 3 passed: Buffer with only empty lines~%"))
  
  ;; Test 4: Very long line
  (let ((buf (make-instance 'standard-buffer))
        (long-string (make-string 1000 :initial-element #\a)))
    (setf (lines buf) (vector long-string))
    (buffer-set-point buf 0 500)
    (forward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 501)) () "Long line forward failed: expected (0 501), got ~A" point))
    (backward-char buf)
    (backward-char buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 499)) () "Long line backward failed: expected (0 499), got ~A" point))
    (format t "✓ Test 4 passed: Very long line~%"))
  
  (format t "All edge case tests passed!~%~%"))

(defun run-all-buffer-movement-tests ()
  "Run all buffer movement tests"
  (format t "~%======================================~%")
  (format t "Running Buffer Movement Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-forward-char)
        (test-backward-char)
        (test-combined-movements)
        (test-edge-cases)
        (format t "~%======================================~%")
        (format t "All tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)