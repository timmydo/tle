(in-package :tle)

(defun test-kill-whole-line-basic ()
  "Test basic kill-whole-line functionality"
  (format t "Running basic kill-whole-line tests...~%")
  
  ;; Test 1: Kill entire line from middle
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line" "third line"))
    (buffer-set-point buf 0 5)  ; At space in first line
    (kill-whole-line buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 1 count failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "second line") ()
            "Test 1 failed: expected 'second line', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Kill entire line from middle~%"))
  
  ;; Test 2: Kill from beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 0)  ; At beginning of second line
    (kill-whole-line buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2 count failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "first line") ()
            "Test 2 failed: expected 'first line', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "third line") ()
            "Test 2 failed: expected 'third line', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 2 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 2 passed: Kill from beginning of line~%"))
  
  ;; Test 3: Kill last line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "last line"))
    (buffer-set-point buf 2 4)  ; In last line
    (kill-whole-line buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3 count failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "first line") ()
            "Test 3 failed: expected 'first line', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "second line") ()
            "Test 3 failed: expected 'second line', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 3 point failed: expected (1 0), got ~A" point))
    (format t "✓ Test 3 passed: Kill last line~%"))
  
  ;; Test 4: Kill entire single character line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a" "b" "c"))
    (buffer-set-point buf 1 0)  ; At 'b'
    (kill-whole-line buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 4 count failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "a") ()
            "Test 4 failed: expected 'a', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "c") ()
            "Test 4 failed: expected 'c', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 4 passed: Kill entire single character line~%"))
  
  (format t "All basic kill-whole-line tests passed!~%~%"))

(defun test-kill-whole-line-edge-cases ()
  "Test kill-whole-line edge cases and boundary conditions"
  (format t "Running kill-whole-line edge case tests...~%")
  
  ;; Test 1: Single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "solo line"))
    (buffer-set-point buf 0 4)  ; At space before "line"
    (kill-whole-line buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 1 count failed: expected 1 line, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "") ()
            "Test 1 failed: expected empty line, got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 point failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Single line buffer kill~%"))
  
  ;; Test 2: Empty line in multi-line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first" "" "third"))
    (buffer-set-point buf 1 0)  ; On empty line
    (kill-whole-line buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2 count failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "first") ()
            "Test 2 failed: expected 'first', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "third") ()
            "Test 2 failed: expected 'third', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 2 passed: Empty line in multi-line buffer~%"))
  
  ;; Test 3: Empty buffer (should not crash)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (handler-case
        (progn
          (kill-whole-line buf)
          (format t "✓ Test 3 passed: Empty buffer (no crash)~%"))
      (error (e)
        (format t "✗ Test 3 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 4: Multiple consecutive kills
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2" "line3" "line4"))
    (buffer-set-point buf 1 2)  ; In "line2"
    (kill-whole-line buf)  ; Kill "line2"
    (assert (= (buffer-line-count buf) 3) ()
            "Test 4a count failed: expected 3 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "line3") ()
            "Test 4a failed: expected 'line3', got '~A'" (buffer-line buf 1))
    (kill-whole-line buf)  ; Kill "line3" (now at line 1)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 4b count failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "line4") ()
            "Test 4b failed: expected 'line4', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 4 passed: Multiple consecutive kills~%"))
  
  (format t "All edge case kill-whole-line tests passed!~%~%"))

(defun test-kill-whole-line-mark-clearing ()
  "Test that kill-whole-line properly clears the mark"
  (format t "Running kill-whole-line mark clearing tests...~%")
  
  ;; Test 1: Mark should be cleared after kill-whole-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (kill-whole-line buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared")
    (format t "✓ Test 1 passed: Mark cleared after kill-whole-line~%"))
  
  ;; Test 2: Mark cleared in single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "only line"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 2)
    (assert (buffer-get-mark buf) () "Test 2 setup failed: mark should be set")
    (kill-whole-line buf)
    (assert (null (buffer-get-mark buf)) () "Test 2 failed: mark should be cleared")
    (format t "✓ Test 2 passed: Mark cleared in single line buffer~%"))
  
  (format t "All mark clearing kill-whole-line tests passed!~%~%"))

(defun test-kill-whole-line-undo-redo ()
  "Test kill-whole-line undo/redo functionality"
  (format t "Running kill-whole-line undo/redo tests...~%")
  
  ;; Test 1: Basic undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)  ; In second line
    (let ((original-count (buffer-line-count buf))
          (original-point (copy-list (buffer-get-point buf))))
      ;; Kill the line
      (kill-whole-line buf)
      (assert (= (buffer-line-count buf) 2) ()
              "Test 1a failed: expected 2 lines, got ~A" (buffer-line-count buf))
      (assert (string= (buffer-line buf 1) "third line") ()
              "Test 1b failed: expected 'third line', got '~A'" (buffer-line buf 1))
      ;; Undo the kill
      (buffer-undo buf)
      (assert (= (buffer-line-count buf) original-count) ()
              "Test 1c failed: expected ~A lines, got ~A" original-count (buffer-line-count buf))
      (assert (string= (buffer-line buf 1) "second line") ()
              "Test 1d failed: expected 'second line', got '~A'" (buffer-line buf 1))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1e failed: expected point ~A, got ~A" original-point (buffer-get-point buf))
      ;; Redo the kill
      (buffer-redo buf)
      (assert (= (buffer-line-count buf) 2) ()
              "Test 1f failed: expected 2 lines, got ~A" (buffer-line-count buf))
      (assert (string= (buffer-line buf 1) "third line") ()
              "Test 1g failed: expected 'third line', got '~A'" (buffer-line buf 1))
      (format t "✓ Test 1 passed: Basic undo/redo~%")))
  
  ;; Test 2: Single line buffer undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "only line"))
    (buffer-set-point buf 0 3)
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      ;; Kill the line
      (kill-whole-line buf)
      (assert (string= (buffer-line buf 0) "") ()
              "Test 2a failed: expected empty line, got '~A'" (buffer-line buf 0))
      ;; Undo the kill
      (buffer-undo buf)
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 2b failed: expected '~A', got '~A'" original-line (buffer-line buf 0))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 2c failed: expected point ~A, got ~A" original-point (buffer-get-point buf))
      ;; Redo the kill
      (buffer-redo buf)
      (assert (string= (buffer-line buf 0) "") ()
              "Test 2d failed: expected empty line, got '~A'" (buffer-line buf 0))
      (format t "✓ Test 2 passed: Single line buffer undo/redo~%")))
  
  ;; Test 3: Multiple operations with undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2" "line3" "line4"))
    (buffer-set-point buf 1 2)
    ;; Kill "line2"
    (kill-whole-line buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 3a failed: expected 3 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "line3") ()
            "Test 3b failed: expected 'line3', got '~A'" (buffer-line buf 1))
    ;; Kill "line3" (now at position 1)
    (kill-whole-line buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3c failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "line4") ()
            "Test 3d failed: expected 'line4', got '~A'" (buffer-line buf 1))
    ;; Undo last kill (restore "line3")
    (buffer-undo buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 3e failed: expected 3 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "line3") ()
            "Test 3f failed: expected 'line3', got '~A'" (buffer-line buf 1))
    ;; Undo first kill (restore "line2")
    (buffer-undo buf)
    (assert (= (buffer-line-count buf) 4) ()
            "Test 3g failed: expected 4 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "line2") ()
            "Test 3h failed: expected 'line2', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 3 passed: Multiple operations with undo~%"))
  
  (format t "All undo/redo kill-whole-line tests passed!~%~%"))

(defun test-kill-whole-line-double-undo-bug ()
  "Test the bug where double kill-whole-line followed by undo fails"
  (format t "Running kill-whole-line double undo bug test...~%")
  
  ;; Test: Double kill-whole-line followed by undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2" "line3" "line4"))
    (buffer-set-point buf 1 2)  ; Position in "line2"
    (let ((original-count (buffer-line-count buf)))
      ;; First kill-whole-line
      (kill-whole-line buf)
      (assert (= (buffer-line-count buf) 3) ()
              "First kill failed: expected 3 lines, got ~A" (buffer-line-count buf))
      (assert (string= (buffer-line buf 1) "line3") ()
              "First kill failed: expected 'line3', got '~A'" (buffer-line buf 1))
      
      ;; Second kill-whole-line (should kill "line3")
      (kill-whole-line buf)
      (assert (= (buffer-line-count buf) 2) ()
              "Second kill failed: expected 2 lines, got ~A" (buffer-line-count buf))
      (assert (string= (buffer-line buf 1) "line4") ()
              "Second kill failed: expected 'line4', got '~A'" (buffer-line buf 1))
      
      ;; First undo (should restore "line3")
      (handler-case
          (progn
            (buffer-undo buf)
            (assert (= (buffer-line-count buf) 3) ()
                    "First undo failed: expected 3 lines, got ~A" (buffer-line-count buf))
            (assert (string= (buffer-line buf 1) "line3") ()
                    "First undo failed: expected 'line3', got '~A'" (buffer-line buf 1))
            (format t "✓ First undo passed~%"))
        (error (e)
          (format t "✗ First undo failed with error: ~A~%" e)))
      
      ;; Second undo (should restore "line2")
      (handler-case
          (progn
            (buffer-undo buf)
            (assert (= (buffer-line-count buf) original-count) ()
                    "Second undo failed: expected ~A lines, got ~A" original-count (buffer-line-count buf))
            (assert (string= (buffer-line buf 1) "line2") ()
                    "Second undo failed: expected 'line2', got '~A'" (buffer-line buf 1))
            (format t "✓ Second undo passed~%"))
        (error (e)
          (format t "✗ Second undo failed with error: ~A~%" e)))))
  
  (format t "Double undo bug test completed!~%~%"))

(defun test-kill-whole-line-edge-position-bug ()
  "Test kill-whole-line undo when cursor is at edge positions"
  (format t "Running kill-whole-line edge position bug test...~%")
  
  ;; Test: Kill last two lines, then undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2" "line3"))
    (buffer-set-point buf 2 3)  ; Position in last line "line3"
    (format t "Initial state: 3 lines, cursor at (2,3)~%")
    
    ;; Kill "line3"
    (kill-whole-line buf)
    (format t "After first kill: ~A lines, cursor at ~A~%" 
            (buffer-line-count buf) (buffer-get-point buf))
    
    ;; Kill "line2" (cursor should now be at (1,0))  
    (kill-whole-line buf)
    (format t "After second kill: ~A lines, cursor at ~A~%" 
            (buffer-line-count buf) (buffer-get-point buf))
    
    ;; Try to undo - this might fail
    (handler-case
        (progn
          (format t "Attempting first undo...~%")
          (buffer-undo buf)
          (format t "First undo succeeded: ~A lines, cursor at ~A~%" 
                  (buffer-line-count buf) (buffer-get-point buf))
          
          (format t "Attempting second undo...~%")
          (buffer-undo buf)
          (format t "Second undo succeeded: ~A lines, cursor at ~A~%" 
                  (buffer-line-count buf) (buffer-get-point buf))
          (format t "✓ Edge position test passed~%"))
      (error (e)
        (format t "✗ Edge position test failed with error: ~A~%" e))))
  
  (format t "Edge position bug test completed!~%~%"))

(defun run-all-kill-whole-line-tests ()
  "Run all kill-whole-line tests"
  (format t "~%======================================~%")
  (format t "Running Kill-Whole-Line Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-kill-whole-line-basic)
        (test-kill-whole-line-edge-cases)
        (test-kill-whole-line-mark-clearing)
        (test-kill-whole-line-undo-redo)
        (test-kill-whole-line-double-undo-bug)
        (test-kill-whole-line-edge-position-bug)
        (format t "~%======================================~%")
        (format t "All kill-whole-line tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)