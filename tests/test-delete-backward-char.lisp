(in-package :tle)

(defun test-delete-backward-char-within-line ()
  "Test delete-backward-char when deleting characters within a line"
  (format t "Running delete-backward-char within line tests...~%")
  
  ;; Test 1: Delete character in middle of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 6)  ; After space, before "world"
    (delete-backward-char buf)
    (assert (string= (buffer-line buf 0) "helloworld") ()
            "Test 1 failed: expected 'helloworld', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1 point failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: Delete character in middle of line~%"))
  
  ;; Test 2: Delete last character of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 5)  ; After 'o' in "hello"
    (delete-backward-char buf)
    (assert (string= (buffer-line buf 0) "hell") ()
            "Test 2 failed: expected 'hell', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "Test 2 point failed: expected (0 4), got ~A" point))
    (format t "✓ Test 2 passed: Delete last character of line~%"))
  
  ;; Test 3: Delete first character of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 1)  ; After 'h' in "hello"
    (delete-backward-char buf)
    (assert (string= (buffer-line buf 0) "ello") ()
            "Test 3 failed: expected 'ello', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Delete first character of line~%"))
  
  ;; Test 4: Delete from single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a" "b" "c"))
    (buffer-set-point buf 1 1)  ; After 'b'
    (delete-backward-char buf)
    (assert (string= (buffer-line buf 1) "") ()
            "Test 4 failed: expected empty string, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 4 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 4 passed: Delete from single character line~%"))
  
  (format t "All within-line delete-backward-char tests passed!~%~%"))

(defun test-delete-backward-char-line-joining ()
  "Test delete-backward-char when joining lines"
  (format t "Running delete-backward-char line joining tests...~%")
  
  ;; Test 1: Delete at beginning of line (should join with previous line)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world" "third"))
    (buffer-set-point buf 1 0)  ; At beginning of second line
    (delete-backward-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 1 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "helloworld") ()
            "Test 1 failed: expected 'helloworld', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "third") ()
            "Test 1 second line failed: expected 'third', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1 point failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: Delete at beginning of line joins with previous~%"))
  
  ;; Test 2: Join content with empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "content" "" "more"))
    (buffer-set-point buf 1 0)  ; At beginning of empty line
    (delete-backward-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "content") ()
            "Test 2 failed: expected 'content', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "more") ()
            "Test 2 second line failed: expected 'more', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 7)) () "Test 2 point failed: expected (0 7), got ~A" point))
    (format t "✓ Test 2 passed: Join content with empty line~%"))
  
  ;; Test 3: Join empty line with content
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "content" "more"))
    (buffer-set-point buf 1 0)  ; At beginning of "content"
    (delete-backward-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "content") ()
            "Test 3 failed: expected 'content', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "more") ()
            "Test 3 second line failed: expected 'more', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Join empty line with content~%"))
  
  ;; Test 4: Join two empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "" "content"))
    (buffer-set-point buf 1 0)  ; At beginning of second empty line
    (delete-backward-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 4 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "") ()
            "Test 4 failed: expected empty string, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "content") ()
            "Test 4 second line failed: expected 'content', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: Join two empty lines~%"))
  
  (format t "All line joining delete-backward-char tests passed!~%~%"))

(defun test-delete-backward-char-edge-cases ()
  "Test delete-backward-char edge cases and boundary conditions"
  (format t "Running delete-backward-char edge case tests...~%")
  
  ;; Test 1: Delete at beginning of first line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2"))
    (buffer-set-point buf 0 0)  ; At beginning of first line
    (let ((original-line-count (buffer-line-count buf))
          (original-line (buffer-line buf 0)))
      (delete-backward-char buf)
      (assert (= (buffer-line-count buf) original-line-count) ()
              "Test 1 line count changed: expected ~A, got ~A" original-line-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1 line changed: expected '~A', got '~A'" original-line (buffer-line buf 0))
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Test 1 point failed: expected (0 0), got ~A" point))
      (format t "✓ Test 1 passed: Delete at beginning of first line does nothing~%")))
  
  ;; Test 2: Delete from single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "solo"))
    (buffer-set-point buf 0 3)  ; After 'l' in "solo"
    (delete-backward-char buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 2 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "soo") ()
            "Test 2 failed: expected 'soo', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 2 point failed: expected (0 2), got ~A" point))
    (format t "✓ Test 2 passed: Delete from single line buffer~%"))
  
  ;; Test 3: Delete at beginning of single line buffer (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "solo"))
    (buffer-set-point buf 0 0)  ; At beginning of "solo"
    (delete-backward-char buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 3 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "solo") ()
            "Test 3 failed: expected 'solo', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 3 passed: Delete at beginning of single line does nothing~%"))
  
  ;; Test 4: Empty buffer (should not crash)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (delete-backward-char buf)
          (format t "✓ Test 4 passed: Empty buffer (no crash)~%"))
      (error (e)
        (format t "✗ Test 4 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 5: Multiple consecutive backward deletes
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "abcdef"))
    (buffer-set-point buf 0 4)  ; After 'd'
    (delete-backward-char buf)  ; Delete 'd' -> "abcef"
    (delete-backward-char buf)  ; Delete 'c' -> "abef"
    (delete-backward-char buf)  ; Delete 'b' -> "aef"
    (assert (string= (buffer-line buf 0) "aef") ()
            "Test 5 failed: expected 'aef', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 5 point failed: expected (0 1), got ~A" point))
    (format t "✓ Test 5 passed: Multiple consecutive backward deletes~%"))
  
  (format t "All edge case delete-backward-char tests passed!~%~%"))

(defun test-delete-backward-char-mark-clearing ()
  "Test that delete-backward-char properly clears the mark"
  (format t "Running delete-backward-char mark clearing tests...~%")
  
  ;; Test 1: Mark should be cleared after delete-backward-char
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 6)
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (delete-backward-char buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared")
    (format t "✓ Test 1 passed: Mark cleared after delete-backward-char~%"))
  
  ;; Test 2: Mark cleared even when delete does nothing
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 0)  ; At beginning of line
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 2 setup failed: mark should be set")
    (delete-backward-char buf)  ; Should do nothing at beginning of first line
    (assert (null (buffer-get-mark buf)) () "Test 2 failed: mark should be cleared even when nothing deleted")
    (format t "✓ Test 2 passed: Mark cleared even when delete does nothing~%"))
  
  (format t "All mark clearing delete-backward-char tests passed!~%~%"))

(defun test-delete-backward-char-undo ()
  "Test that delete-backward-char works properly with undo"
  (format t "Running delete-backward-char undo tests...~%")
  
  ;; Test 1: Undo single character deletion
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 3)  ; After 'l'
    (delete-backward-char buf)  ; Delete 'l' -> "helo"
    (assert (string= (buffer-line buf 0) "helo") ()
            "Test 1 delete failed: expected 'helo', got '~A'" (buffer-line buf 0))
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "hello") ()
            "Test 1 undo failed: expected 'hello', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 3)) () "Test 1 undo point failed: expected (0 3), got ~A" point))
    (format t "✓ Test 1 passed: Undo single character deletion~%"))
  
  ;; Test 2: Undo line joining
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 1 0)  ; At beginning of "world"
    (delete-backward-char buf)  ; Join lines -> "helloworld"
    (assert (= (buffer-line-count buf) 1) ()
            "Test 2 delete line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "helloworld") ()
            "Test 2 delete failed: expected 'helloworld', got '~A'" (buffer-line buf 0))
    (buffer-undo buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2 undo line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "hello") ()
            "Test 2 undo first line failed: expected 'hello', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "world") ()
            "Test 2 undo second line failed: expected 'world', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 2 undo point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 2 passed: Undo line joining~%"))
  
  (format t "All undo delete-backward-char tests passed!~%~%"))

(defun run-all-delete-backward-char-tests ()
  "Run all delete-backward-char tests"
  (format t "~%======================================~%")
  (format t "Running Delete-Backward-Char Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-delete-backward-char-within-line)
        (test-delete-backward-char-line-joining)
        (test-delete-backward-char-edge-cases)
        (test-delete-backward-char-mark-clearing)
        (test-delete-backward-char-undo)
        (format t "~%======================================~%")
        (format t "All delete-backward-char tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)