(in-package :tle)

(defun test-kill-line-basic ()
  "Test basic kill-line functionality"
  (format t "Running basic kill-line tests...~%")
  
  ;; Test 1: Kill text from middle of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 5)  ; At space before "world"
    (kill-line buf)
    (assert (string= (buffer-line buf 0) "hello") ()
            "Test 1 failed: expected 'hello', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1 point failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: Kill text from middle of line~%"))
  
  ;; Test 2: Kill from beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 0)
    (kill-line buf)
    (assert (string= (buffer-line buf 0) "") ()
            "Test 2 failed: expected empty string, got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Kill from beginning of line~%"))
  
  ;; Test 3: Kill from end of line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (let ((original-line (buffer-line buf 0)))
      (kill-line buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 3 failed: line should not change when at end, got '~A'" (buffer-line buf 0))
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 5)) () "Test 3 point failed: expected (0 5), got ~A" point))
      (format t "✓ Test 3 passed: Kill from end of line does nothing~%")))
  
  ;; Test 4: Kill entire single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a" "b" "c"))
    (buffer-set-point buf 1 0)  ; At 'b'
    (kill-line buf)
    (assert (string= (buffer-line buf 1) "") ()
            "Test 4 failed: expected empty string, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 4 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 4 passed: Kill entire single character line~%"))
  
  (format t "All basic kill-line tests passed!~%~%"))

(defun test-kill-line-edge-cases ()
  "Test kill-line edge cases and boundary conditions"
  (format t "Running kill-line edge case tests...~%")
  
  ;; Test 1: Kill from empty line (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "content"))
    (buffer-set-point buf 0 0)
    (kill-line buf)
    (assert (string= (buffer-line buf 0) "") ()
            "Test 1 failed: empty line should remain empty, got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Kill from empty line does nothing~%"))
  
  ;; Test 2: Single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "solo line"))
    (buffer-set-point buf 0 4)  ; At space before "line"
    (kill-line buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 2 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "solo") ()
            "Test 2 failed: expected 'solo', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Single line buffer kill~%"))
  
  ;; Test 3: Empty buffer (should not crash)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (kill-line buf)
          (format t "✓ Test 3 passed: Empty buffer (no crash)~%"))
      (error (e)
        (format t "✗ Test 3 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 4: Multiple consecutive kills
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "abcdef"))
    (buffer-set-point buf 0 2)  ; At 'c'
    (kill-line buf)  ; Kill "cdef" -> "ab"
    (assert (string= (buffer-line buf 0) "ab") ()
            "Test 4a failed: expected 'ab', got '~A'" (buffer-line buf 0))
    (buffer-set-point buf 0 1)  ; At 'b'
    (kill-line buf)  ; Kill "b" -> "a"
    (assert (string= (buffer-line buf 0) "a") ()
            "Test 4b failed: expected 'a', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 4 passed: Multiple consecutive kills~%"))
  
  (format t "All edge case kill-line tests passed!~%~%"))

(defun test-kill-line-mark-clearing ()
  "Test that kill-line properly clears the mark"
  (format t "Running kill-line mark clearing tests...~%")
  
  ;; Test 1: Mark should be cleared after kill-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (kill-line buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared")
    (format t "✓ Test 1 passed: Mark cleared after kill-line~%"))
  
  ;; Test 2: Mark cleared even when kill does nothing
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 5)  ; At end of line
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 2 setup failed: mark should be set")
    (kill-line buf)  ; Should do nothing at end of line
    (assert (null (buffer-get-mark buf)) () "Test 2 failed: mark should be cleared even when nothing killed")
    (format t "✓ Test 2 passed: Mark cleared even when kill does nothing~%"))
  
  (format t "All mark clearing kill-line tests passed!~%~%"))

(defun test-kill-line-undo-redo ()
  "Test kill-line undo/redo functionality"
  (format t "Running kill-line undo/redo tests...~%")
  
  ;; Test 1: Basic undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      ;; Kill text
      (kill-line buf)
      (assert (string= (buffer-line buf 0) "hello") ()
              "Test 1a failed: expected 'hello', got '~A'" (buffer-line buf 0))
      ;; Undo the kill
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1b failed: expected '~A', got '~A'" original-line (buffer-line buf 0))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1c failed: expected point ~A, got ~A" original-point (buffer-get-point buf))
      ;; Redo the kill
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) "hello") ()
              "Test 1d failed: expected 'hello', got '~A'" (buffer-line buf 0))
      (format t "✓ Test 1 passed: Basic undo/redo~%")))
  
  ;; Test 2: Multiple operations with undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "abcdef"))
    (buffer-set-point buf 0 2)
    ;; Kill "cdef"
    (kill-line buf)
    (assert (string= (buffer-line buf 0) "ab") ()
            "Test 2a failed: expected 'ab', got '~A'" (buffer-line buf 0))
    ;; Move and kill again
    (buffer-set-point buf 0 1)
    (kill-line buf)
    (assert (string= (buffer-line buf 0) "a") ()
            "Test 2b failed: expected 'a', got '~A'" (buffer-line buf 0))
    ;; Undo last kill
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "ab") ()
            "Test 2c failed: expected 'ab', got '~A'" (buffer-line buf 0))
    ;; Undo first kill
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "abcdef") ()
            "Test 2d failed: expected 'abcdef', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Multiple operations with undo~%"))
  
  (format t "All undo/redo kill-line tests passed!~%~%"))

(defun run-all-kill-line-tests ()
  "Run all kill-line tests"
  (format t "~%======================================~%")
  (format t "Running Kill-Line Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-kill-line-basic)
        (test-kill-line-edge-cases)
        (test-kill-line-mark-clearing)
        (test-kill-line-undo-redo)
        (format t "~%======================================~%")
        (format t "All kill-line tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)