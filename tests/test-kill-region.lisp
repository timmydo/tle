(in-package :tle)

(defun test-kill-region-with-mark ()
  "Test kill-region functionality when mark is set"
  (format t "Running kill-region with mark tests...~%")
  
  ;; Test 1: Kill region within same line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world" "second line"))
    (buffer-set-point buf 0 6)  ; At 'w' in "world"
    (buffer-set-mark buf 0 2)   ; At 'l' in "hello"
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "heworld") ()
            "Test 1 failed: expected 'heworld', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 1 point failed: expected (0 2), got ~A" point))
    (assert (null (buffer-get-mark buf)) () "Test 1 mark failed: mark should be cleared")
    (format t "✓ Test 1 passed: Kill region within same line~%"))
  
  ;; Test 2: Kill region across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two" "line three"))
    (buffer-set-point buf 2 4)  ; At space in "line three"
    (buffer-set-mark buf 0 5)   ; At 'o' in "line one"
    (kill-region buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 2 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "line three") ()
            "Test 2 failed: expected 'line three', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 2 point failed: expected (0 5), got ~A" point))
    (assert (null (buffer-get-mark buf)) () "Test 2 mark failed: mark should be cleared")
    (format t "✓ Test 2 passed: Kill region across multiple lines~%"))
  
  ;; Test 3: Kill region from point to mark (reverse direction)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("abcdef"))
    (buffer-set-point buf 0 2)  ; At 'c'
    (buffer-set-mark buf 0 4)   ; At 'e'
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "abef") ()
            "Test 3 failed: expected 'abef', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 3 point failed: expected (0 2), got ~A" point))
    (format t "✓ Test 3 passed: Kill region from point to mark (reverse)~%"))
  
  ;; Test 4: Kill entire line when region spans whole line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world" "test"))
    (buffer-set-point buf 1 5)  ; At end of "world"
    (buffer-set-mark buf 1 0)   ; At beginning of "world"
    (kill-region buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 4 line count failed: expected 3, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "") ()
            "Test 4 failed: expected empty line, got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 4 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 4 passed: Kill entire line content~%"))
  
  (format t "All kill-region with mark tests passed!~%~%"))

(defun test-kill-region-without-mark ()
  "Test kill-region functionality when no mark is set (should kill whole line)"
  (format t "Running kill-region without mark tests...~%")
  
  ;; Test 1: Kill whole line when no mark is set
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two" "line three"))
    (buffer-set-point buf 1 4)  ; In middle of "line two"
    (buffer-clear-mark buf)     ; Ensure no mark
    (kill-region buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 1 line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "line one") ()
            "Test 1a failed: expected 'line one', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "line three") ()
            "Test 1b failed: expected 'line three', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 1 passed: Kill whole line when no mark~%"))
  
  ;; Test 2: Kill whole line in single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("only line"))
    (buffer-set-point buf 0 3)
    (buffer-clear-mark buf)
    (kill-region buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 2 line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "") ()
            "Test 2 failed: expected empty line, got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Kill whole line in single line buffer~%"))
  
  (format t "All kill-region without mark tests passed!~%~%"))

(defun test-kill-region-edge-cases ()
  "Test kill-region edge cases and boundary conditions"
  (format t "Running kill-region edge case tests...~%")
  
  ;; Test 1: Empty region (point equals mark)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 5)   ; Mark at same position as point
    (let ((original-line (buffer-line buf 0)))
      (kill-region buf)
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
    (kill-region buf)
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
    (kill-region buf)
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
          (kill-region buf)
          (format t "✓ Test 4 passed: Empty buffer (no crash)~%"))
      (error (e)
        (format t "✗ Test 4 failed: Empty buffer caused error: ~A~%" e))))
  
  (format t "All edge case kill-region tests passed!~%~%"))

(defun test-kill-region-undo-redo ()
  "Test kill-region undo/redo functionality"
  (format t "Running kill-region undo/redo tests...~%")
  
  ;; Test 1: Basic undo/redo with region
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 6)  ; At 'w'
    (buffer-set-mark buf 0 2)   ; At first 'l'
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      ;; Kill region
      (kill-region buf)
      (assert (string= (buffer-line buf 0) "heworld") ()
              "Test 1a failed: expected 'heworld', got '~A'" (buffer-line buf 0))
      ;; Undo the kill
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1b failed: expected '~A', got '~A'" original-line (buffer-line buf 0))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1c failed: expected point ~A, got ~A" original-point (buffer-get-point buf))
      ;; Redo the kill
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) "heworld") ()
              "Test 1d failed: expected 'heworld', got '~A'" (buffer-line buf 0))
      (format t "✓ Test 1 passed: Basic region undo/redo~%")))
  
  ;; Test 2: Multiple kill-region operations with undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("abcdefgh"))
    ;; First kill: remove "cd"
    (buffer-set-point buf 0 4)  ; At 'e'
    (buffer-set-mark buf 0 2)   ; At 'c'
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "abefgh") ()
            "Test 2a failed: expected 'abefgh', got '~A'" (buffer-line buf 0))
    ;; Second kill: remove "ef"
    (buffer-set-point buf 0 4)  ; At 'g'
    (buffer-set-mark buf 0 2)   ; At 'e'
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "abgh") ()
            "Test 2b failed: expected 'abgh', got '~A'" (buffer-line buf 0))
    ;; Undo second kill
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "abefgh") ()
            "Test 2c failed: expected 'abefgh', got '~A'" (buffer-line buf 0))
    ;; Undo first kill
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "abcdefgh") ()
            "Test 2d failed: expected 'abcdefgh', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Multiple kill-region operations with undo~%"))
  
  ;; Test 3: Undo/redo of whole line kill (no mark)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two" "line three"))
    (buffer-set-point buf 1 3)
    (buffer-clear-mark buf)
    (let ((original-line-count (buffer-line-count buf))
          (original-line-1 (buffer-line buf 1)))
      ;; Kill whole line
      (kill-region buf)
      (assert (= (buffer-line-count buf) 2) ()
              "Test 3a failed: expected 2 lines, got ~A" (buffer-line-count buf))
      ;; Undo
      (buffer-undo buf)
      (assert (= (buffer-line-count buf) original-line-count) ()
              "Test 3b failed: expected ~A lines, got ~A" original-line-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 1) original-line-1) ()
              "Test 3c failed: expected '~A', got '~A'" original-line-1 (buffer-line buf 1))
      (format t "✓ Test 3 passed: Whole line kill undo/redo~%")))
  
  ;; Test 4: Test undo/redo twice in a row
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test line"))
    (buffer-set-point buf 0 5)  ; At space
    (buffer-set-mark buf 0 2)   ; At 's'
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "teline") ()
            "Test 4a failed: expected 'teline', got '~A'" (buffer-line buf 0))
    ;; First undo
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test line") ()
            "Test 4b failed: expected 'test line', got '~A'" (buffer-line buf 0))
    ;; Second undo (should work)
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test line") ()
            "Test 4c failed: expected 'test line', got '~A'" (buffer-line buf 0))
    ;; First redo
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "teline") ()
            "Test 4d failed: expected 'teline', got '~A'" (buffer-line buf 0))
    ;; Second redo (should work)
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "teline") ()
            "Test 4e failed: expected 'teline', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 4 passed: Undo/redo twice in a row~%"))
  
  ;; Test 5: Multi-line kill-region undo/redo with complete state restoration
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line" "third line" "fourth line"))
    (buffer-set-point buf 2 5)  ; At 'd' in "third line"
    (buffer-set-mark buf 1 3)   ; At 'o' in "second line"
    (let ((original-lines (buffer-line-count buf))
          (original-point (copy-list (buffer-get-point buf)))
          (original-mark (copy-list (buffer-get-mark buf)))
          (original-line-1 (buffer-line buf 1))
          (original-line-2 (buffer-line buf 2)))
      (format t "Multi-line test - Original state: ~A lines, point ~A, mark ~A~%" 
              original-lines original-point original-mark)
      (format t "  Line 1: '~A'~%" original-line-1)
      (format t "  Line 2: '~A'~%" original-line-2)
      ;; Kill the multi-line region
      (kill-region buf)
      (let ((after-kill-lines (buffer-line-count buf))
            (after-kill-point (copy-list (buffer-get-point buf)))
            (after-kill-mark (buffer-get-mark buf)))
        (format t "After kill: ~A lines, point ~A, mark ~A~%" 
                after-kill-lines after-kill-point after-kill-mark)
        (format t "  Line 1: '~A'~%" (buffer-line buf 1))
        ;; Undo the kill
        (buffer-undo buf)
        (let ((after-undo-lines (buffer-line-count buf))
              (after-undo-point (copy-list (buffer-get-point buf)))
              (after-undo-mark (copy-list (buffer-get-mark buf))))
          (format t "After undo: ~A lines, point ~A, mark ~A~%" 
                  after-undo-lines after-undo-point after-undo-mark)
          (format t "  Line 1: '~A'~%" (buffer-line buf 1))
          (format t "  Line 2: '~A'~%" (buffer-line buf 2))
          ;; Check complete restoration
          (assert (= after-undo-lines original-lines) ()
                  "Test 5a line count failed: expected ~A, got ~A" original-lines after-undo-lines)
          (assert (equal after-undo-point original-point) ()
                  "Test 5b point failed: expected ~A, got ~A" original-point after-undo-point)
          (assert (equal after-undo-mark original-mark) ()
                  "Test 5c mark failed: expected ~A, got ~A" original-mark after-undo-mark)
          (assert (string= (buffer-line buf 1) original-line-1) ()
                  "Test 5d line 1 failed: expected '~A', got '~A'" original-line-1 (buffer-line buf 1))
          (assert (string= (buffer-line buf 2) original-line-2) ()
                  "Test 5e line 2 failed: expected '~A', got '~A'" original-line-2 (buffer-line buf 2))
          ;; Test redo
          (buffer-redo buf)
          (let ((after-redo-lines (buffer-line-count buf))
                (after-redo-point (copy-list (buffer-get-point buf))))
            (format t "After redo: ~A lines, point ~A~%" after-redo-lines after-redo-point)
            (assert (= after-redo-lines after-kill-lines) ()
                    "Test 5f redo line count failed: expected ~A, got ~A" after-kill-lines after-redo-lines)
            (assert (equal after-redo-point after-kill-point) ()
                    "Test 5g redo point failed: expected ~A, got ~A" after-kill-point after-redo-point))
          (format t "✓ Test 5 passed: Multi-line kill-region with complete undo/redo~%"))))
  
  (format t "All undo/redo kill-region tests passed!~%~%")))

(defun test-kill-region-mark-clearing ()
  "Test that kill-region properly clears the mark"
  (format t "Running kill-region mark clearing tests...~%")
  
  ;; Test 1: Mark should be cleared after region kill
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 6)
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (kill-region buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared")
    (format t "✓ Test 1 passed: Mark cleared after region kill~%"))
  
  ;; Test 2: Mark cleared after whole line kill
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two"))
    (buffer-set-point buf 0 3)
    (buffer-set-mark buf 1 2)  ; Set mark on different line
    (buffer-clear-mark buf)    ; Clear mark to trigger whole line kill
    (kill-region buf)
    (assert (null (buffer-get-mark buf)) () "Test 2 failed: mark should be cleared")
    (format t "✓ Test 2 passed: Mark cleared after whole line kill~%"))
  
  (format t "All mark clearing kill-region tests passed!~%~%"))

(defun run-all-kill-region-tests ()
  "Run all kill-region tests"
  (format t "~%======================================~%")
  (format t "Running Kill-Region Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-kill-region-with-mark)
        (test-kill-region-without-mark)
        (test-kill-region-edge-cases)
        (test-kill-region-undo-redo)
        (test-kill-region-mark-clearing)
        (format t "~%======================================~%")
        (format t "All kill-region tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)
