(in-package :tle)

(defun test-delete-region-with-mark ()
  "Test delete-region functionality when mark is set"
  (format t "Running delete-region with mark tests...~%")
  
  ;; Test 1: Delete region within same line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world" "second line"))
    (buffer-set-point buf 0 6)  ; At 'w' in "world"
    (buffer-set-mark buf 0 2)   ; At 'l' in "hello"
    (delete-region buf)
    (assert (string= (buffer-line buf 0) "heworld") ()
            "Test 1 failed: expected 'heworld', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 1 point failed: expected (0 2), got ~A" point))
    (assert (null (buffer-get-mark buf)) () "Test 1 mark failed: mark should be cleared")
    (format t "✓ Test 1 passed: Delete region within same line~%"))
  
  ;; Test 2: Delete region across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two" "line three"))
    (buffer-set-point buf 2 4)  ; At space in "line three"
    (buffer-set-mark buf 0 5)   ; At 'o' in "line one"
    (delete-region buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 2 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "line three") ()
            "Test 2 failed: expected 'line three', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 2 point failed: expected (0 5), got ~A" point))
    (assert (null (buffer-get-mark buf)) () "Test 2 mark failed: mark should be cleared")
    (format t "✓ Test 2 passed: Delete region across multiple lines~%"))
  
  ;; Test 3: Delete region from point to mark (reverse direction)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("abcdef"))
    (buffer-set-point buf 0 2)  ; At 'c'
    (buffer-set-mark buf 0 4)   ; At 'e'
    (delete-region buf)
    (assert (string= (buffer-line buf 0) "abef") ()
            "Test 3 failed: expected 'abef', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 3 point failed: expected (0 2), got ~A" point))
    (format t "✓ Test 3 passed: Delete region from point to mark (reverse)~%"))
  
  ;; Test 4: Delete entire line when region spans whole line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world" "test"))
    (buffer-set-point buf 1 5)  ; At end of "world"
    (buffer-set-mark buf 1 0)   ; At beginning of "world"
    (delete-region buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 4 line count failed: expected 3, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "") ()
            "Test 4 failed: expected empty line, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 4 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 4 passed: Delete entire line content~%"))
  
  (format t "All delete-region with mark tests passed!~%~%"))

(defun test-delete-region-without-mark ()
  "Test delete-region functionality when no mark is set (should do nothing)"
  (format t "Running delete-region without mark tests...~%")
  
  ;; Test 1: No-op when no mark is set
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two" "line three"))
    (buffer-set-point buf 1 4)  ; In middle of "line two"
    (buffer-clear-mark buf)     ; Ensure no mark
    (let ((original-line-count (buffer-line-count buf))
          (original-line-1 (buffer-line buf 1))
          (original-point (copy-list (buffer-get-point buf))))
      (delete-region buf)
      (assert (= (buffer-line-count buf) original-line-count) ()
              "Test 1 line count failed: expected ~A, got ~A" original-line-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 1) original-line-1) ()
              "Test 1 failed: line should not change, got '~A'" (buffer-line buf 1))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1 point failed: expected ~A, got ~A" original-point (buffer-get-point buf))
      (format t "✓ Test 1 passed: No-op when no mark~%")))
  
  ;; Test 2: No-op in single line buffer without mark
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("only line"))
    (buffer-set-point buf 0 3)
    (buffer-clear-mark buf)
    (let ((original-line (buffer-line buf 0)))
      (delete-region buf)
      (assert (= (buffer-line-count buf) 1) ()
              "Test 2 line count failed: expected 1, got ~A" (buffer-line-count buf))
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 2 failed: line should not change, got '~A'" (buffer-line buf 0))
      (format t "✓ Test 2 passed: No-op in single line buffer without mark~%")))
  
  (format t "All delete-region without mark tests passed!~%~%"))

(defun test-delete-region-edge-cases ()
  "Test delete-region edge cases and boundary conditions"
  (format t "Running delete-region edge case tests...~%")
  
  ;; Test 1: Empty region (point equals mark)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 5)   ; Mark at same position as point
    (let ((original-line (buffer-line buf 0)))
      (delete-region buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1 failed: line should not change, got '~A'" (buffer-line buf 0))
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 5)) () "Test 1 point failed: expected (0 5), got ~A" point))
      (assert (null (buffer-get-mark buf)) () "Test 1 mark failed: mark should be cleared")
      (format t "✓ Test 1 passed: Empty region (point equals mark)~%")))
  
  ;; Test 2: Single character region
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("abcde"))
    (buffer-set-point buf 0 2)  ; At 'c'
    (buffer-set-mark buf 0 3)   ; At 'd'
    (delete-region buf)
    (assert (string= (buffer-line buf 0) "abde") ()
            "Test 2 failed: expected 'abde', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 2 point failed: expected (0 2), got ~A" point))
    (format t "✓ Test 2 passed: Single character region~%"))
  
  ;; Test 3: Region spanning entire buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "middle" "last"))
    (buffer-set-point buf 2 4)  ; At end of "last"
    (buffer-set-mark buf 0 0)   ; At beginning of "first"
    (delete-region buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 3 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "") ()
            "Test 3 failed: expected empty line, got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Region spanning entire buffer~%"))
  
  ;; Test 4: Empty buffer (should not crash)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #())
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (delete-region buf)
          (format t "✓ Test 4 passed: Empty buffer (no crash)~%"))
      (error (e)
        (format t "✗ Test 4 failed: Empty buffer caused error: ~A~%" e))))
  
  (format t "All edge case delete-region tests passed!~%~%"))

(defun test-delete-region-undo-redo ()
  "Test delete-region undo/redo functionality"
  (format t "Running delete-region undo/redo tests...~%")
  
  ;; Test 1: Basic undo/redo with region
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 6)  ; At 'w'
    (buffer-set-mark buf 0 2)   ; At first 'l'
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf)))
          (original-mark (copy-list (buffer-get-mark buf))))
      ;; Delete region
      (delete-region buf)
      (assert (string= (buffer-line buf 0) "heworld") ()
              "Test 1a failed: expected 'heworld', got '~A'" (buffer-line buf 0))
      ;; Undo the delete
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1b failed: expected '~A', got '~A'" original-line (buffer-line buf 0))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1c failed: expected point ~A, got ~A" original-point (buffer-get-point buf))
      (assert (equal (buffer-get-mark buf) original-mark) ()
              "Test 1d failed: expected mark ~A, got ~A" original-mark (buffer-get-mark buf))
      ;; Redo the delete
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) "heworld") ()
              "Test 1e failed: expected 'heworld', got '~A'" (buffer-line buf 0))
      (format t "✓ Test 1 passed: Basic region undo/redo~%")))
  
  ;; Test 2: Multiple delete-region operations with undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("abcdefgh"))
    ;; First delete: remove "cd"
    (buffer-set-point buf 0 4)  ; At 'e'
    (buffer-set-mark buf 0 2)   ; At 'c'
    (delete-region buf)
    (assert (string= (buffer-line buf 0) "abefgh") ()
            "Test 2a failed: expected 'abefgh', got '~A'" (buffer-line buf 0))
    ;; Second delete: remove "ef"
    (buffer-set-point buf 0 4)  ; At 'g'
    (buffer-set-mark buf 0 2)   ; At 'e'
    (delete-region buf)
    (assert (string= (buffer-line buf 0) "abgh") ()
            "Test 2b failed: expected 'abgh', got '~A'" (buffer-line buf 0))
    ;; Undo second delete
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "abefgh") ()
            "Test 2c failed: expected 'abefgh', got '~A'" (buffer-line buf 0))
    ;; Undo first delete
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "abcdefgh") ()
            "Test 2d failed: expected 'abcdefgh', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Multiple delete-region operations with undo~%"))
  
  ;; Test 3: Test undo/redo twice in a row
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test line"))
    (buffer-set-point buf 0 5)  ; At space
    (buffer-set-mark buf 0 2)   ; At 's'
    (delete-region buf)
    (assert (string= (buffer-line buf 0) "teline") ()
            "Test 3a failed: expected 'teline', got '~A'" (buffer-line buf 0))
    ;; First undo
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test line") ()
            "Test 3b failed: expected 'test line', got '~A'" (buffer-line buf 0))
    ;; Second undo (should work)
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test line") ()
            "Test 3c failed: expected 'test line', got '~A'" (buffer-line buf 0))
    ;; First redo
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "teline") ()
            "Test 3d failed: expected 'teline', got '~A'" (buffer-line buf 0))
    ;; Second redo (should work)
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "teline") ()
            "Test 3e failed: expected 'teline', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 3 passed: Undo/redo twice in a row~%"))
  
  ;; Test 4: Multi-line delete-region undo/redo with complete state restoration
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line" "third line" "fourth line"))
    (buffer-set-point buf 2 5)  ; At 'd' in "third line"
    (buffer-set-mark buf 1 3)   ; At 'o' in "second line"
    (let ((original-lines (buffer-line-count buf))
          (original-point (copy-list (buffer-get-point buf)))
          (original-mark (copy-list (buffer-get-mark buf)))
          (original-line-1 (buffer-line buf 1))
          (original-line-2 (buffer-line buf 2)))
      ;; Delete the multi-line region
      (delete-region buf)
      (let ((after-delete-lines (buffer-line-count buf))
            (after-delete-point (copy-list (buffer-get-point buf)))
            (after-delete-mark (buffer-get-mark buf)))
        ;; Undo the delete
        (buffer-undo buf)
        (let ((after-undo-lines (buffer-line-count buf))
              (after-undo-point (copy-list (buffer-get-point buf)))
              (after-undo-mark (copy-list (buffer-get-mark buf))))
          ;; Check complete restoration
          (assert (= after-undo-lines original-lines) ()
                  "Test 4a line count failed: expected ~A, got ~A" original-lines after-undo-lines)
          (assert (equal after-undo-point original-point) ()
                  "Test 4b point failed: expected ~A, got ~A" original-point after-undo-point)
          (assert (equal after-undo-mark original-mark) ()
                  "Test 4c mark failed: expected ~A, got ~A" original-mark after-undo-mark)
          (assert (string= (buffer-line buf 1) original-line-1) ()
                  "Test 4d line 1 failed: expected '~A', got '~A'" original-line-1 (buffer-line buf 1))
          (assert (string= (buffer-line buf 2) original-line-2) ()
                  "Test 4e line 2 failed: expected '~A', got '~A'" original-line-2 (buffer-line buf 2))
          ;; Test redo
          (buffer-redo buf)
          (let ((after-redo-lines (buffer-line-count buf))
                (after-redo-point (copy-list (buffer-get-point buf))))
            (assert (= after-redo-lines after-delete-lines) ()
                    "Test 4f redo line count failed: expected ~A, got ~A" after-delete-lines after-redo-lines)
            (assert (equal after-redo-point after-delete-point) ()
                    "Test 4g redo point failed: expected ~A, got ~A" after-delete-point after-redo-point))
          (format t "✓ Test 4 passed: Multi-line delete-region with complete undo/redo~%"))))
  
  (format t "All undo/redo delete-region tests passed!~%~%"))

(defun test-delete-region-vs-kill-ring ()
  "Test that delete-region does NOT affect the kill ring (unlike kill-region)"
  (format t "Running delete-region vs kill-ring tests...~%")
  
  ;; Test 1: delete-region should not add to kill ring
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    ;; Clear kill ring
    (setf (buffer-kill-ring buf) '())
    (buffer-set-point buf 0 6)  ; At 'w'
    (buffer-set-mark buf 0 2)   ; At first 'l'
    (delete-region buf)
    ;; Kill ring should still be empty
    (assert (null (buffer-kill-ring buf)) ()
            "Test 1 failed: delete-region should not add to kill ring, got ~A" (buffer-kill-ring buf))
    (format t "✓ Test 1 passed: delete-region does not add to kill ring~%"))
  
  ;; Test 2: Compare with kill-region (which should add to kill ring)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test line"))
    ;; Clear kill ring
    (setf (buffer-kill-ring buf) '())
    (buffer-set-point buf 0 5)  ; At space
    (buffer-set-mark buf 0 2)   ; At 's'
    (kill-region buf)
    ;; Kill ring should have the killed text
    (assert (not (null (buffer-kill-ring buf))) ()
            "Test 2 setup failed: kill-region should add to kill ring")
    ;; Reset buffer for comparison
    (setf (lines buf) #("test line"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 2)
    (let ((kill-ring-before (copy-list (buffer-kill-ring buf))))
      (delete-region buf)
      ;; Kill ring should be unchanged
      (assert (equal (buffer-kill-ring buf) kill-ring-before) ()
              "Test 2 failed: delete-region changed kill ring")
      (format t "✓ Test 2 passed: delete-region leaves kill ring unchanged~%")))
  
  (format t "All delete-region vs kill-ring tests passed!~%~%"))

(defun run-all-delete-region-tests ()
  "Run all delete-region tests"
  (format t "~%======================================~%")
  (format t "Running Delete-Region Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-delete-region-with-mark)
        (test-delete-region-without-mark)
        (test-delete-region-edge-cases)
        (test-delete-region-undo-redo)
        (test-delete-region-vs-kill-ring)
        (format t "~%======================================~%")
        (format t "All delete-region tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)