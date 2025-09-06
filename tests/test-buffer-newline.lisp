(in-package :tle)

(defun test-insert-newline-beginning ()
  "Test inserting newlines at the beginning of lines"
  (format t "Running insert-newline at beginning tests...~%")
  
  ;; Test 1: Insert newline at beginning of first line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 0)
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 3) () 
            "Test 1 failed: expected 3 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "") () 
            "Test 1 failed: expected empty first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "hello") () 
            "Test 1 failed: expected 'hello' on second line, got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "world") () 
            "Test 1 failed: expected 'world' on third line, got '~A'" (buffer-line buf 2))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () 
              "Test 1 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 1 passed: Insert newline at beginning of first line~%"))
  
  ;; Test 2: Insert newline at beginning of middle line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2" "line3"))
    (buffer-set-point buf 1 0)
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 4) () 
            "Test 2 failed: expected 4 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "line1") () 
            "Test 2 failed: expected 'line1' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "") () 
            "Test 2 failed: expected empty second line, got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "line2") () 
            "Test 2 failed: expected 'line2' on third line, got '~A'" (buffer-line buf 2))
    (assert (string= (buffer-line buf 3) "line3") () 
            "Test 2 failed: expected 'line3' on fourth line, got '~A'" (buffer-line buf 3))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () 
              "Test 2 point failed: expected (2 0), got ~A" point))
    (format t "✓ Test 2 passed: Insert newline at beginning of middle line~%"))
  
  (format t "All insert-newline beginning tests passed!~%~%"))

(defun test-insert-newline-middle ()
  "Test inserting newlines in the middle of lines"
  (format t "Running insert-newline at middle tests...~%")
  
  ;; Test 1: Insert newline in middle of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)  ; Between 'hello' and ' world'
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 2) () 
            "Test 1 failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "hello") () 
            "Test 1 failed: expected 'hello' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) " world") () 
            "Test 1 failed: expected ' world' on second line, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () 
              "Test 1 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 1 passed: Insert newline in middle of line~%"))
  
  ;; Test 2: Insert newline between words
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "one two three"))
    (buffer-set-point buf 0 4)  ; Between 'one ' and 'two three'
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 2) () 
            "Test 2 failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "one ") () 
            "Test 2 failed: expected 'one ' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "two three") () 
            "Test 2 failed: expected 'two three' on second line, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () 
              "Test 2 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 2 passed: Insert newline between words~%"))
  
  ;; Test 3: Multiple newlines in sequence
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 2)  ; Between 'te' and 'st'
    (insert-newline buf)
    (insert-newline buf)
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 4) () 
            "Test 3 failed: expected 4 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "te") () 
            "Test 3 failed: expected 'te' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "") () 
            "Test 3 failed: expected empty second line, got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "") () 
            "Test 3 failed: expected empty third line, got '~A'" (buffer-line buf 2))
    (assert (string= (buffer-line buf 3) "st") () 
            "Test 3 failed: expected 'st' on fourth line, got '~A'" (buffer-line buf 3))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(3 0)) () 
              "Test 3 point failed: expected (3 0), got ~A" point))
    (format t "✓ Test 3 passed: Multiple newlines in sequence~%"))
  
  (format t "All insert-newline middle tests passed!~%~%"))

(defun test-insert-newline-end ()
  "Test inserting newlines at the end of lines"
  (format t "Running insert-newline at end tests...~%")
  
  ;; Test 1: Insert newline at end of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 3) () 
            "Test 1 failed: expected 3 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "hello") () 
            "Test 1 failed: expected 'hello' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "") () 
            "Test 1 failed: expected empty second line, got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "world") () 
            "Test 1 failed: expected 'world' on third line, got '~A'" (buffer-line buf 2))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () 
              "Test 1 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 1 passed: Insert newline at end of line~%"))
  
  ;; Test 2: Insert newline at end of last line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first" "last"))
    (buffer-set-point buf 1 4)  ; At end of "last"
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 3) () 
            "Test 2 failed: expected 3 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "first") () 
            "Test 2 failed: expected 'first' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "last") () 
            "Test 2 failed: expected 'last' on second line, got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "") () 
            "Test 2 failed: expected empty third line, got '~A'" (buffer-line buf 2))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () 
              "Test 2 point failed: expected (2 0), got ~A" point))
    (format t "✓ Test 2 passed: Insert newline at end of last line~%"))
  
  (format t "All insert-newline end tests passed!~%~%"))

(defun test-insert-newline-empty-lines ()
  "Test inserting newlines with empty lines"
  (format t "Running insert-newline with empty lines tests...~%")
  
  ;; Test 1: Insert newline in empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "content"))
    (buffer-set-point buf 0 0)
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 3) () 
            "Test 1 failed: expected 3 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "") () 
            "Test 1 failed: expected empty first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "") () 
            "Test 1 failed: expected empty second line, got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "content") () 
            "Test 1 failed: expected 'content' on third line, got '~A'" (buffer-line buf 2))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () 
              "Test 1 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 1 passed: Insert newline in empty line~%"))
  
  ;; Test 2: Insert newline creating multiple empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "start"))
    (buffer-set-point buf 0 5)  ; At end of "start"
    (insert-newline buf)
    (insert-newline buf)
    (insert-newline buf)
    (insert-char buf #\e)
    (insert-char buf #\n)
    (insert-char buf #\d)
    (assert (= (buffer-line-count buf) 4) () 
            "Test 2 failed: expected 4 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "start") () 
            "Test 2 failed: expected 'start' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "") () 
            "Test 2 failed: expected empty second line, got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "") () 
            "Test 2 failed: expected empty third line, got '~A'" (buffer-line buf 2))
    (assert (string= (buffer-line buf 3) "end") () 
            "Test 2 failed: expected 'end' on fourth line, got '~A'" (buffer-line buf 3))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(3 3)) () 
              "Test 2 point failed: expected (3 3), got ~A" point))
    (format t "✓ Test 2 passed: Insert newline creating multiple empty lines~%"))
  
  (format t "All insert-newline empty lines tests passed!~%~%"))

(defun test-insert-newline-complex ()
  "Test complex newline insertion scenarios"
  (format t "Running complex insert-newline tests...~%")
  
  ;; Test 1: Combination of character and newline insertion
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (insert-char buf #\Space)
    (insert-char buf #\w)
    (insert-char buf #\o)
    (insert-char buf #\r)
    (insert-newline buf)
    (insert-char buf #\l)
    (insert-char buf #\d)
    (assert (= (buffer-line-count buf) 2) () 
            "Test 1 failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "hello wor") () 
            "Test 1 failed: expected 'hello wor' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "ld") () 
            "Test 1 failed: expected 'ld' on second line, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 2)) () 
              "Test 1 point failed: expected (1 2), got ~A" point))
    (format t "✓ Test 1 passed: Combination of character and newline insertion~%"))
  
  ;; Test 2: Splitting a long line with multiple words
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "The quick brown fox jumps"))
    (buffer-set-point buf 0 10)  ; Between 'brown' and ' fox'
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 2) () 
            "Test 2 failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "The quick ") () 
            "Test 2 failed: expected 'The quick ' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "brown fox jumps") () 
            "Test 2 failed: expected 'brown fox jumps' on second line, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () 
              "Test 2 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 2 passed: Splitting a long line with multiple words~%"))
  
  (format t "All complex insert-newline tests passed!~%~%"))

(defun test-insert-newline-mark-clearing ()
  "Test that inserting newlines clears the mark"
  (format t "Running insert-newline mark clearing tests...~%")
  
  ;; Test 1: Mark is cleared after newline insertion
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)  ; Between 'hello' and ' world'
    (buffer-set-mark buf 0 8)   ; Set a mark in the middle of 'world'
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (insert-newline buf)
    (assert (null (buffer-get-mark buf)) () 
            "Test 1 failed: mark should be cleared after newline insertion")
    (assert (= (buffer-line-count buf) 2) () 
            "Test 1 failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "hello") () 
            "Test 1 failed: expected 'hello' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) " world") () 
            "Test 1 failed: expected ' world' on second line, got '~A'" (buffer-line buf 1))
    (format t "✓ Test 1 passed: Mark is cleared after newline insertion~%"))
  
  ;; Test 2: Mark remains cleared after multiple newline insertions
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 2)  ; Between 'te' and 'st'
    (buffer-set-mark buf 0 3)   ; Set a mark
    (insert-newline buf)
    (assert (null (buffer-get-mark buf)) () 
            "Test 2 failed: mark should be cleared after first newline")
    (insert-newline buf)
    (assert (null (buffer-get-mark buf)) () 
            "Test 2 failed: mark should remain cleared after second newline")
    (assert (= (buffer-line-count buf) 3) () 
            "Test 2 failed: expected 3 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "te") () 
            "Test 2 failed: expected 'te' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "") () 
            "Test 2 failed: expected empty second line, got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "st") () 
            "Test 2 failed: expected 'st' on third line, got '~A'" (buffer-line buf 2))
    (format t "✓ Test 2 passed: Mark remains cleared after multiple newline insertions~%"))
  
  (format t "All insert-newline mark clearing tests passed!~%~%"))

(defun run-all-buffer-newline-tests ()
  "Run all buffer newline insertion tests"
  (format t "~%======================================~%")
  (format t "Running Buffer Newline Insertion Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-insert-newline-beginning)
        (test-insert-newline-middle)
        (test-insert-newline-end)
        (test-insert-newline-empty-lines)
        (test-insert-newline-complex)
        (test-insert-newline-mark-clearing)
        (format t "~%======================================~%")
        (format t "All newline insertion tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)