(in-package :tle)

(defun test-next-line ()
  "Test next-line method with various scenarios"
  (format t "Running next-line tests...~%")
  
  ;; Test 1: Normal next-line movement preserving column
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world" "second line" "third line"))
    (buffer-set-point buf 0 5)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Test 1 failed: expected (1 5), got ~A" point))
    (format t "✓ Test 1 passed: Normal next-line movement preserving column~%"))
  
  ;; Test 2: Next-line to shorter line (should adjust column)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("long line here" "short" "another line"))
    (buffer-set-point buf 0 10)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Test 2 failed: expected (1 5), got ~A" point))
    (format t "✓ Test 2 passed: Next-line to shorter line adjusts column~%"))
  
  ;; Test 3: Next-line from last line (should stay in place)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "last line"))
    (buffer-set-point buf 1 5)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Test 3 failed: expected (1 5), got ~A" point))
    (format t "✓ Test 3 passed: Next-line from last line stays in place~%"))
  
  ;; Test 4: Next-line to empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("content line" "" "more content"))
    (buffer-set-point buf 0 8)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 4 failed: expected (1 0), got ~A" point))
    (format t "✓ Test 4 passed: Next-line to empty line~%"))
  
  ;; Test 5: Next-line from empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("" "content line"))
    (buffer-set-point buf 0 0)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 5 failed: expected (1 0), got ~A" point))
    (format t "✓ Test 5 passed: Next-line from empty line~%"))
  
  ;; Test 6: Next-line preserving column across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("column test" "also col test" "col test too"))
    (buffer-set-point buf 0 3)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 3)) () "Test 6a failed: expected (1 3), got ~A" point))
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 3)) () "Test 6b failed: expected (2 3), got ~A" point))
    (format t "✓ Test 6 passed: Next-line preserving column across multiple lines~%"))
  
  (format t "All next-line tests passed!~%~%"))

(defun test-previous-line ()
  "Test previous-line method with various scenarios"
  (format t "Running previous-line tests...~%")
  
  ;; Test 1: Normal previous-line movement preserving column
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line" "third line"))
    (buffer-set-point buf 2 5)
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Test 1 failed: expected (1 5), got ~A" point))
    (format t "✓ Test 1 passed: Normal previous-line movement preserving column~%"))
  
  ;; Test 2: Previous-line to shorter line (should adjust column)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("short" "long line here" "another line"))
    (buffer-set-point buf 1 10)
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 2 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 2 passed: Previous-line to shorter line adjusts column~%"))
  
  ;; Test 3: Previous-line from first line (should stay in place)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line"))
    (buffer-set-point buf 0 5)
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 3 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 3 passed: Previous-line from first line stays in place~%"))
  
  ;; Test 4: Previous-line to empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("" "content line"))
    (buffer-set-point buf 1 8)
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: Previous-line to empty line~%"))
  
  ;; Test 5: Previous-line from empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("content line" ""))
    (buffer-set-point buf 1 0)
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 5 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 5 passed: Previous-line from empty line~%"))
  
  ;; Test 6: Previous-line preserving column across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("column test" "also col test" "col test too"))
    (buffer-set-point buf 2 3)
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 3)) () "Test 6a failed: expected (1 3), got ~A" point))
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 3)) () "Test 6b failed: expected (0 3), got ~A" point))
    (format t "✓ Test 6 passed: Previous-line preserving column across multiple lines~%"))
  
  ;; Test 7: Lines with varying lengths
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("a" "very long line with lots of content" "xyz"))
    (buffer-set-point buf 1 20)
    (previous-line buf)  ; Should go to end of shorter line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 7a failed: expected (0 1), got ~A" point))
    (next-line buf)  ; Should preserve column from original position
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 1)) () "Test 7b failed: expected (1 1), got ~A" point))
    (format t "✓ Test 7 passed: Lines with varying lengths~%"))
  
  (format t "All previous-line tests passed!~%~%"))

(defun test-combined-line-movements ()
  "Test combinations of line movements"
  (format t "Running combined line movement tests...~%")
  
  ;; Test 1: Next-line then previous-line should return to original position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two" "line three"))
    (buffer-set-point buf 1 3)
    (let ((original-point (buffer-get-point buf)))
      (next-line buf)
      (previous-line buf)
      (let ((final-point (buffer-get-point buf)))
        (assert (equal original-point final-point) () 
                "Test 1 failed: expected ~A, got ~A" original-point final-point)))
    (format t "✓ Test 1 passed: Next-line then previous-line returns to original~%"))
  
  ;; Test 2: Multiple next-line movements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "second" "third" "fourth"))
    (buffer-set-point buf 0 2)
    (next-line buf)
    (next-line buf)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(3 2)) () "Test 2 failed: expected (3 2), got ~A" point))
    (format t "✓ Test 2 passed: Multiple next-line movements~%"))
  
  ;; Test 3: Multiple previous-line movements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "second" "third" "fourth"))
    (buffer-set-point buf 3 2)
    (previous-line buf)
    (previous-line buf)
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 3 failed: expected (0 2), got ~A" point))
    (format t "✓ Test 3 passed: Multiple previous-line movements~%"))
  
  ;; Test 4: Column preservation across different line lengths
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("short" "very long line with content" "mid length" "x"))
    (buffer-set-point buf 1 15)  ; Position 15 in long line
    (next-line buf)  ; Should clip to end of shorter line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 10)) () "Test 4a failed: expected (2 10), got ~A" point))
    (next-line buf)  ; Should clip to end of very short line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(3 1)) () "Test 4b failed: expected (3 1), got ~A" point))
    (format t "✓ Test 4 passed: Column preservation across different line lengths~%"))
  
  (format t "All combined line movement tests passed!~%~%"))

(defun test-line-movement-edge-cases ()
  "Test edge cases for line movement"
  (format t "Running line movement edge case tests...~%")
  
  ;; Test 1: Single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("only line"))
    (buffer-set-point buf 0 5)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Single line next failed: expected (0 5), got ~A" point))
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Single line previous failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: Single line buffer~%"))
  
  ;; Test 2: Empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #())
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (next-line buf)
          (previous-line buf)
          (format t "✓ Test 2 passed: Empty buffer (no crashes)~%"))
      (error (e)
        (format t "✗ Test 2 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 3: Buffer with only empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("" "" ""))
    (buffer-set-point buf 1 0)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () "Empty lines next failed: expected (2 0), got ~A" point))
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Empty lines previous failed: expected (1 0), got ~A" point))
    (format t "✓ Test 3 passed: Buffer with only empty lines~%"))
  
  ;; Test 4: Very long lines
  (let ((buf (make-instance 'standard-buffer))
        (long-string (make-string 1000 :initial-element #\a))
        (short-string "short"))
    (setf (lines buf) (vector long-string short-string))
    (buffer-set-point buf 0 500)
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "Long line next failed: expected (1 5), got ~A" point))
    (previous-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Long line previous failed: expected (0 5), got ~A" point))
    (format t "✓ Test 4 passed: Very long lines~%"))
  
  ;; Test 5: End of line positions
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line1" "longer line2" "line3"))
    (buffer-set-point buf 0 5)  ; At end of first line
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () "End of line next failed: expected (1 5), got ~A" point))
    (next-line buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 5)) () "End of line next 2 failed: expected (2 5), got ~A" point))
    (format t "✓ Test 5 passed: End of line positions~%"))
  
  (format t "All line movement edge case tests passed!~%~%"))

(defun test-line-char-movement-integration ()
  "Test integration between line and character movements"
  (format t "Running line-character movement integration tests...~%")
  
  ;; Test 1: Character movement then line movement
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world" "second line" "third line"))
    (buffer-set-point buf 0 0)
    (forward-char buf)
    (forward-char buf)
    (forward-char buf)  ; Now at (0 3)
    (next-line buf)     ; Should go to (1 3)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 3)) () "Integration test 1 failed: expected (1 3), got ~A" point))
    (format t "✓ Test 1 passed: Character movement then line movement~%"))
  
  ;; Test 2: Line movement then character movement
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line" "third line"))
    (buffer-set-point buf 0 5)
    (next-line buf)     ; Should go to (1 5)
    (forward-char buf)  ; Should go to (1 6)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 6)) () "Integration test 2 failed: expected (1 6), got ~A" point))
    (format t "✓ Test 2 passed: Line movement then character movement~%"))
  
  ;; Test 3: Mixed movements creating a path
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("abc" "def" "ghi"))
    (buffer-set-point buf 0 0)
    (forward-char buf)      ; (0 1)
    (next-line buf)         ; (1 1)
    (forward-char buf)      ; (1 2)
    (next-line buf)         ; (2 2)
    (backward-char buf)     ; (2 1)
    (previous-line buf)     ; (1 1)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 1)) () "Integration test 3 failed: expected (1 1), got ~A" point))
    (format t "✓ Test 3 passed: Mixed movements creating a path~%"))
  
  (format t "All line-character movement integration tests passed!~%~%"))

(defun run-all-line-movement-tests ()
  "Run all line movement tests"
  (format t "~%======================================~%")
  (format t "Running Line Movement Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-next-line)
        (test-previous-line)
        (test-combined-line-movements)
        (test-line-movement-edge-cases)
        (test-line-char-movement-integration)
        (format t "~%======================================~%")
        (format t "All line movement tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)