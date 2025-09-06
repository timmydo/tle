(in-package :tle)

(defun test-delete-char-within-line ()
  "Test delete-char when deleting characters within a line"
  (format t "Running delete-char within line tests...~%")
  
  ;; Test 1: Delete character in middle of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 5)  ; At space before "world"
    (delete-char buf)
    (assert (string= (buffer-line buf 0) "helloworld") ()
            "Test 1 failed: expected 'helloworld', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1 point failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: Delete character in middle of line~%"))
  
  ;; Test 2: Delete first character of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 0)
    (delete-char buf)
    (assert (string= (buffer-line buf 0) "ello") ()
            "Test 2 failed: expected 'ello', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Delete first character of line~%"))
  
  ;; Test 3: Delete last character of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 4)  ; At 'o' in "hello"
    (delete-char buf)
    (assert (string= (buffer-line buf 0) "hell") ()
            "Test 3 failed: expected 'hell', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "Test 3 point failed: expected (0 4), got ~A" point))
    (format t "✓ Test 3 passed: Delete last character of line~%"))
  
  ;; Test 4: Delete from single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a" "b" "c"))
    (buffer-set-point buf 1 0)  ; At 'b'
    (delete-char buf)
    (assert (string= (buffer-line buf 1) "") ()
            "Test 4 failed: expected empty string, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 4 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 4 passed: Delete from single character line~%"))
  
  (format t "All within-line delete-char tests passed!~%~%"))

(defun test-delete-char-line-joining ()
  "Test delete-char when joining lines"
  (format t "Running delete-char line joining tests...~%")
  
  ;; Test 1: Delete at end of line (should join with next line)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world" "third"))
    (buffer-set-point buf 0 5)  ; At end of first line
    (delete-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 1 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "helloworld") ()
            "Test 1 failed: expected 'helloworld', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "third") ()
            "Test 1 second line failed: expected 'third', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1 point failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: Delete at end of line joins with next~%"))
  
  ;; Test 2: Join empty line with content
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "content" "more"))
    (buffer-set-point buf 0 0)  ; At end of empty line
    (delete-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "content") ()
            "Test 2 failed: expected 'content', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "more") ()
            "Test 2 second line failed: expected 'more', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 2 passed: Join empty line with content~%"))
  
  ;; Test 3: Join content with empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "content" "" "more"))
    (buffer-set-point buf 0 7)  ; At end of "content"
    (delete-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "content") ()
            "Test 3 failed: expected 'content', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "more") ()
            "Test 3 second line failed: expected 'more', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 3 passed: Join content with empty line~%"))
  
  ;; Test 4: Join two empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "" "content"))
    (buffer-set-point buf 0 0)
    (delete-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 4 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "") ()
            "Test 4 failed: expected empty string, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "content") ()
            "Test 4 second line failed: expected 'content', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 4 passed: Join two empty lines~%"))
  
  (format t "All line joining delete-char tests passed!~%~%"))

(defun test-delete-char-edge-cases ()
  "Test delete-char edge cases and boundary conditions"
  (format t "Running delete-char edge case tests...~%")
  
  ;; Test 1: Delete at end of last line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2"))
    (buffer-set-point buf 1 5)  ; At end of last line
    (let ((original-line-count (buffer-line-count buf))
          (original-line (buffer-line buf 1)))
      (delete-char buf)
      (assert (= (buffer-line-count buf) original-line-count) ()
              "Test 1 line count changed: expected ~A, got ~A" original-line-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 1) original-line) ()
              "Test 1 line changed: expected '~A', got '~A'" original-line (buffer-line buf 1))
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(1 5)) () "Test 1 point failed: expected (1 5), got ~A" point))
      (format t "✓ Test 1 passed: Delete at end of last line does nothing~%")))
  
  ;; Test 2: Delete from single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "solo"))
    (buffer-set-point buf 0 2)  ; At 'l' in "solo"
    (delete-char buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 2 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "soo") ()
            "Test 2 failed: expected 'soo', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Delete from single line buffer~%"))
  
  ;; Test 3: Delete at end of single line buffer (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "solo"))
    (buffer-set-point buf 0 4)  ; At end of "solo"
    (delete-char buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 3 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "solo") ()
            "Test 3 failed: expected 'solo', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 3 passed: Delete at end of single line does nothing~%"))
  
  ;; Test 4: Empty buffer (should not crash)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (delete-char buf)
          (format t "✓ Test 4 passed: Empty buffer (no crash)~%"))
      (error (e)
        (format t "✗ Test 4 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 5: Multiple consecutive deletes
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "abcdef"))
    (buffer-set-point buf 0 1)  ; At 'b'
    (delete-char buf)  ; Delete 'b' -> "acdef"
    (delete-char buf)  ; Delete 'c' -> "adef"
    (delete-char buf)  ; Delete 'd' -> "aef"
    (assert (string= (buffer-line buf 0) "aef") ()
            "Test 5 failed: expected 'aef', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 5 point failed: expected (0 1), got ~A" point))
    (format t "✓ Test 5 passed: Multiple consecutive deletes~%"))
  
  (format t "All edge case delete-char tests passed!~%~%"))

(defun test-delete-char-mark-clearing ()
  "Test that delete-char properly clears the mark"
  (format t "Running delete-char mark clearing tests...~%")
  
  ;; Test 1: Mark should be cleared after delete-char
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (delete-char buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared")
    (format t "✓ Test 1 passed: Mark cleared after delete-char~%"))
  
  ;; Test 2: Mark cleared even when delete does nothing
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 5)  ; At end of line
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 2 setup failed: mark should be set")
    (delete-char buf)  ; Should do nothing at end of last line
    (assert (null (buffer-get-mark buf)) () "Test 2 failed: mark should be cleared even when nothing deleted")
    (format t "✓ Test 2 passed: Mark cleared even when delete does nothing~%"))
  
  (format t "All mark clearing delete-char tests passed!~%~%"))

(defun run-all-delete-char-tests ()
  "Run all delete-char tests"
  (format t "~%======================================~%")
  (format t "Running Delete-Char Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-delete-char-within-line)
        (test-delete-char-line-joining)
        (test-delete-char-edge-cases)
        (test-delete-char-mark-clearing)
        (format t "~%======================================~%")
        (format t "All delete-char tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)