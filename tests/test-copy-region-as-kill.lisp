(in-package :tle)

(defun test-copy-region-as-kill-with-mark ()
  "Test copy-region-as-kill functionality when mark is set"
  (format t "Running copy-region-as-kill with mark tests...~%")
  
  ;; Test 1: Copy region within same line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 6)  ; At 'w' in "world"
    (buffer-set-mark buf 0 2)   ; At 'l' in "hello"
    ;; Capture the original state before copy
    (let ((original-content (copy-seq (lines buf)))
          (original-point (buffer-get-point buf)))
      (copy-region-as-kill buf)
      ;; Verify content is unchanged
      (assert (string= (buffer-line buf 0) "hello world") ()
              "Test 1 failed: content changed, expected 'hello world', got '~A'" (buffer-line buf 0))
      (assert (string= (buffer-line buf 1) "second line") ()
              "Test 1 failed: second line changed")
      ;; Verify point is unchanged
      (let ((point (buffer-get-point buf)))
        (assert (equal point original-point) () "Test 1 point failed: expected ~A, got ~A" original-point point))
      ;; Verify mark is cleared (like kill-region does)
      (assert (null (buffer-get-mark buf)) () "Test 1 mark failed: mark should be cleared")
      (format t "✓ Test 1 passed: Copy region within same line~%")))
  
  ;; Test 2: Copy region across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line one" "line two" "line three"))
    (buffer-set-point buf 2 4)  ; At space in "line three"
    (buffer-set-mark buf 0 5)   ; At 'o' in "line one"
    ;; Capture the original state before copy
    (let ((original-lines (map 'vector #'copy-seq (lines buf)))
          (original-point (buffer-get-point buf)))
      (copy-region-as-kill buf)
      ;; Verify content is unchanged
      (assert (= (buffer-line-count buf) 3) ()
              "Test 2 line count failed: expected 3, got ~A" (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) "line one") ()
              "Test 2 failed: first line changed")
      (assert (string= (buffer-line buf 1) "line two") ()
              "Test 2 failed: second line changed")
      (assert (string= (buffer-line buf 2) "line three") ()
              "Test 2 failed: third line changed")
      ;; Verify point is unchanged
      (let ((point (buffer-get-point buf)))
        (assert (equal point original-point) () "Test 2 point failed: expected ~A, got ~A" original-point point))
      ;; Verify mark is cleared
      (assert (null (buffer-get-mark buf)) () "Test 2 mark failed: mark should be cleared")
      (format t "✓ Test 2 passed: Copy region across multiple lines~%")))
  
  ;; Test 3: Copy region from point to mark (reverse direction)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "abcdef"))
    (buffer-set-point buf 0 2)  ; At 'c'
    (buffer-set-mark buf 0 4)   ; At 'e'
    (let ((original-content (buffer-line buf 0)))
      (copy-region-as-kill buf)
      ;; Verify content is unchanged
      (assert (string= (buffer-line buf 0) "abcdef") ()
              "Test 3 failed: content changed, expected 'abcdef', got '~A'" (buffer-line buf 0))
      ;; Verify point is unchanged
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 2)) () "Test 3 point failed: expected (0 2), got ~A" point))
      (format t "✓ Test 3 passed: Copy region from point to mark (reverse)~%")))
  
  (format t "All copy-region-as-kill with mark tests passed!~%~%"))

(defun test-copy-region-as-kill-without-mark ()
  "Test copy-region-as-kill functionality when no mark is set (should copy whole line)"
  (format t "Running copy-region-as-kill without mark tests...~%")
  
  ;; Test 1: Copy whole line in multi-line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 7)  ; At 'l' in "second line"
    (let ((original-lines (map 'vector #'copy-seq (lines buf)))
          (original-point (buffer-get-point buf)))
      (copy-region-as-kill buf)
      ;; Verify content is unchanged
      (assert (= (buffer-line-count buf) 3) ()
              "Test 1 line count failed: expected 3, got ~A" (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) "first line") ()
              "Test 1 failed: first line changed")
      (assert (string= (buffer-line buf 1) "second line") ()
              "Test 1 failed: second line changed")
      (assert (string= (buffer-line buf 2) "third line") ()
              "Test 1 failed: third line changed")
      ;; Verify point is unchanged
      (let ((point (buffer-get-point buf)))
        (assert (equal point original-point) () "Test 1 point failed: expected ~A, got ~A" original-point point))
      (format t "✓ Test 1 passed: Copy whole line in multi-line buffer~%")))
  
  ;; Test 2: Copy whole line in single-line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "only line"))
    (buffer-set-point buf 0 4)  ; At 'l' in "only line"
    (let ((original-content (buffer-line buf 0)))
      (copy-region-as-kill buf)
      ;; Verify content is unchanged
      (assert (string= (buffer-line buf 0) "only line") ()
              "Test 2 failed: content changed")
      ;; Verify point is unchanged
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 4)) () "Test 2 point failed: expected (0 4), got ~A" point))
      (format t "✓ Test 2 passed: Copy whole line in single-line buffer~%")))
  
  (format t "All copy-region-as-kill without mark tests passed!~%~%"))

(defun test-copy-region-as-kill-edge-cases ()
  "Test copy-region-as-kill edge cases and boundary conditions"
  (format t "Running copy-region-as-kill edge case tests...~%")
  
  ;; Test 1: Empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (copy-region-as-kill buf)  ; Should handle gracefully
    (format t "✓ Test 1 passed: Empty buffer handling~%"))
  
  ;; Test 2: Single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "x"))
    (buffer-set-point buf 0 0)
    (buffer-set-mark buf 0 1)
    (copy-region-as-kill buf)
    (assert (string= (buffer-line buf 0) "x") ()
            "Test 2 failed: single character line changed")
    (format t "✓ Test 2 passed: Single character line~%"))
  
  ;; Test 3: Copy empty region (mark and point at same position)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test line"))
    (buffer-set-point buf 0 4)
    (buffer-set-mark buf 0 4)  ; Same position as point
    (copy-region-as-kill buf)
    (assert (string= (buffer-line buf 0) "test line") ()
            "Test 3 failed: content changed with empty region")
    (format t "✓ Test 3 passed: Empty region (mark equals point)~%"))
  
  ;; Test 4: Copy at buffer boundaries
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "start" "middle" "end"))
    (buffer-set-point buf 2 3)  ; At end of last line
    (buffer-set-mark buf 0 0)   ; At start of first line
    (copy-region-as-kill buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 4 failed: buffer boundaries copy changed line count")
    (format t "✓ Test 4 passed: Copy at buffer boundaries~%"))
  
  (format t "All edge case copy-region-as-kill tests passed!~%~%"))

(defun test-copy-region-as-kill-undo-redo ()
  "Test copy-region-as-kill undo/redo functionality - should NOT create undo records"
  (format t "Running copy-region-as-kill undo/redo tests...~%")
  
  ;; Test 1: Copy operation should not create undo record
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 6)
    (buffer-set-mark buf 0 2)
    ;; Record initial undo state
    (let ((initial-undo-tree-size (tree-size (buffer-undo-tree buf))))
      (copy-region-as-kill buf)
      ;; Verify no undo record was created
      (assert (= (tree-size (buffer-undo-tree buf)) initial-undo-tree-size) ()
              "Test 1 failed: copy-region-as-kill should not create undo records")
      (format t "✓ Test 1 passed: Copy operation creates no undo record~%")))
  
  ;; Test 2: Multiple copy operations should not affect undo history
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line one" "line two" "line three"))
    (buffer-set-point buf 1 0)
    ;; Perform some operation that creates an undo record
    (insert-char buf #\X)
    (let ((undo-tree-size-after-insert (tree-size (buffer-undo-tree buf))))
      ;; Now do multiple copy operations
      (buffer-set-mark buf 0 0)
      (copy-region-as-kill buf)
      (copy-region-as-kill buf)  ; Second copy in a row
      (copy-region-as-kill buf)  ; Third copy in a row
      ;; Verify undo tree size hasn't changed
      (assert (= (tree-size (buffer-undo-tree buf)) undo-tree-size-after-insert) ()
              "Test 2 failed: multiple copy operations should not affect undo tree")
      ;; Test undo still works for the insert operation
      (buffer-undo buf)
      (assert (string= (buffer-line buf 1) "line two") ()
              "Test 2 failed: undo should still work after copy operations")
      (format t "✓ Test 2 passed: Multiple copy operations don't affect undo~%")))
  
  ;; Test 3: Copy operation twice in a row
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test text"))
    (buffer-set-point buf 0 4)
    (buffer-set-mark buf 0 0)
    (let ((original-content (buffer-line buf 0))
          (original-point (buffer-get-point buf))
          (original-undo-size (tree-size (buffer-undo-tree buf))))
      ;; First copy
      (copy-region-as-kill buf)
      ;; Second copy immediately after
      (copy-region-as-kill buf)  ; Note: mark should be cleared after first copy
      ;; Verify content unchanged
      (assert (string= (buffer-line buf 0) original-content) ()
              "Test 3 failed: content changed after double copy")
      ;; Verify no undo records created
      (assert (= (tree-size (buffer-undo-tree buf)) original-undo-size) ()
              "Test 3 failed: double copy should not create undo records")
      (format t "✓ Test 3 passed: Copy operation twice in a row~%")))
  
  (format t "All undo/redo copy-region-as-kill tests passed!~%~%"))

(defun test-copy-region-as-kill-mark-clearing ()
  "Test that copy-region-as-kill properly clears the mark"
  (format t "Running copy-region-as-kill mark clearing tests...~%")
  
  ;; Test 1: Mark clearing with region copy
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 0)
    (assert (not (null (buffer-get-mark buf))) () "Pre-test: mark should be set")
    (copy-region-as-kill buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared after copy")
    (format t "✓ Test 1 passed: Mark cleared after region copy~%"))
  
  ;; Test 2: No mark clearing when copying whole line (no mark set)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test line"))
    (buffer-set-point buf 0 2)
    (assert (null (buffer-get-mark buf)) () "Pre-test: mark should be null")
    (copy-region-as-kill buf)
    (assert (null (buffer-get-mark buf)) () "Test 2 failed: mark should remain null")
    (format t "✓ Test 2 passed: Mark remains null when copying whole line~%"))
  
  (format t "All mark clearing copy-region-as-kill tests passed!~%~%"))

(defun run-all-copy-region-as-kill-tests ()
  "Run all copy-region-as-kill tests"
  (handler-case
      (progn
        (test-copy-region-as-kill-with-mark)
        (test-copy-region-as-kill-without-mark)
        (test-copy-region-as-kill-edge-cases)
        (test-copy-region-as-kill-undo-redo)
        (test-copy-region-as-kill-mark-clearing)
        (format t "All copy-region-as-kill tests passed successfully!~%")
        t)
    (error (e)
      (format t "Test failed with error: ~A~%" e)
      nil)))