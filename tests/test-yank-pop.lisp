(in-package :tle)

(defun test-yank-pop-basic ()
  "Test basic yank-pop functionality"
  (format t "Running basic yank-pop tests...~%")
  
  ;; Test 1: yank-pop with no previous yank should do nothing
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 6)
    ;; Add some entries to kill ring
    (add-to-kill-ring buf "first")
    (add-to-kill-ring buf "second")
    ;; Try yank-pop without previous yank
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      (yank-pop buf)  ; Should do nothing
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1 failed: line should not change, got '~A'" (buffer-line buf 0))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1 point failed: expected ~A, got ~A" original-point (buffer-get-point buf))
      (format t "✓ Test 1 passed: yank-pop without previous yank does nothing~%")))
  
  ;; Test 2: yank-pop with only one entry in kill ring should do nothing
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 6)
    ;; Add single entry to kill ring
    (add-to-kill-ring buf "single")
    ;; Yank first
    (yank buf)
    (assert (string= (buffer-line buf 0) "hello singleworld") ()
            "Test 2 setup failed: expected 'hello singleworld', got '~A'" (buffer-line buf 0))
    ;; Try yank-pop with single entry - should do nothing
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "hello singleworld") ()
            "Test 2 failed: line should not change, got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: yank-pop with single kill ring entry does nothing~%"))
  
  ;; Test 3: Basic yank-pop cycling through kill ring
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test "))
    (buffer-set-point buf 0 5)  ; At end
    ;; Build kill ring in order: "first", "second", "third" (most recent)
    (add-to-kill-ring buf "first")
    (add-to-kill-ring buf "second")
    (add-to-kill-ring buf "third")
    ;; Yank should get "third" (most recent)
    (yank buf)
    (assert (string= (buffer-line buf 0) "test third") ()
            "Test 3a failed: expected 'test third', got '~A'" (buffer-line buf 0))
    ;; First yank-pop should get "second"
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "test second") ()
            "Test 3b failed: expected 'test second', got '~A'" (buffer-line buf 0))
    ;; Second yank-pop should get "first"
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "test first") ()
            "Test 3c failed: expected 'test first', got '~A'" (buffer-line buf 0))
    ;; Third yank-pop should wrap around to "third"
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "test third") ()
            "Test 3d failed: expected 'test third', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 3 passed: Basic yank-pop cycling~%"))
  
  (format t "All basic yank-pop tests passed!~%~%"))

(defun test-yank-pop-multiline ()
  "Test yank-pop with multi-line content"
  (format t "Running multi-line yank-pop tests...~%")
  
  ;; Test 1: yank-pop replacing single-line with multi-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("start end"))
    (buffer-set-point buf 0 6)  ; After "start "
    ;; Add multi-line and single-line to kill ring
    (add-to-kill-ring buf "single")
    (add-to-kill-ring buf (concatenate 'string "line1" (string #\Newline) "line2"))
    ;; Yank multi-line first
    (yank buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 1a line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "start line1") ()
            "Test 1b failed: expected 'start line1', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "line2end") ()
            "Test 1c failed: expected 'line2end', got '~A'" (buffer-line buf 1))
    ;; yank-pop should replace with single-line
    (yank-pop buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 1d line count failed: expected 1, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "start singleend") ()
            "Test 1e failed: expected 'start singleend', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Multi-line to single-line yank-pop~%"))
  
  ;; Test 2: yank-pop replacing single-line with multi-line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("prefix suffix"))
    (buffer-set-point buf 0 7)  ; After "prefix "
    ;; Add single-line and multi-line to kill ring
    (add-to-kill-ring buf (concatenate 'string "multi1" (string #\Newline) "multi2"))
    (add-to-kill-ring buf "replace")
    ;; Yank single-line first
    (yank buf)
    (assert (string= (buffer-line buf 0) "prefix replacesuffix") ()
            "Test 2a failed: expected 'prefix replacesuffix', got '~A'" (buffer-line buf 0))
    ;; yank-pop should replace with multi-line
    (yank-pop buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2b line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "prefix multi1") ()
            "Test 2c failed: expected 'prefix multi1', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "multi2suffix") ()
            "Test 2d failed: expected 'multi2suffix', got '~A'" (buffer-line buf 1))
    (format t "✓ Test 2 passed: Single-line to multi-line yank-pop~%"))
  
  (format t "All multi-line yank-pop tests passed!~%~%"))

(defun test-yank-pop-undo-redo ()
  "Test yank-pop undo/redo functionality"
  (format t "Running yank-pop undo/redo tests...~%")
  
  ;; Test 1: Basic yank-pop undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test "))
    (buffer-set-point buf 0 5)
    ;; Build kill ring
    (add-to-kill-ring buf "first")
    (add-to-kill-ring buf "second")
    ;; Yank then yank-pop
    (yank buf)
    (assert (string= (buffer-line buf 0) "test second") ()
            "Test 1a failed: expected 'test second', got '~A'" (buffer-line buf 0))
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "test first") ()
            "Test 1b failed: expected 'test first', got '~A'" (buffer-line buf 0))
    ;; Undo yank-pop
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test second") ()
            "Test 1c failed: expected 'test second', got '~A'" (buffer-line buf 0))
    ;; Redo yank-pop
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "test first") ()
            "Test 1d failed: expected 'test first', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Basic yank-pop undo/redo~%"))
  
  ;; Test 2: Multiple yank-pop operations undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("base "))
    (buffer-set-point buf 0 5)
    ;; Build kill ring with 3 entries
    (add-to-kill-ring buf "entry1")
    (add-to-kill-ring buf "entry2") 
    (add-to-kill-ring buf "entry3")
    ;; Yank then two yank-pops
    (yank buf)
    (assert (string= (buffer-line buf 0) "base entry3") ()
            "Test 2a failed: expected 'base entry3', got '~A'" (buffer-line buf 0))
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "base entry2") ()
            "Test 2b failed: expected 'base entry2', got '~A'" (buffer-line buf 0))
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "base entry1") ()
            "Test 2c failed: expected 'base entry1', got '~A'" (buffer-line buf 0))
    ;; Undo twice to get back to entry3
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "base entry2") ()
            "Test 2d failed: expected 'base entry2', got '~A'" (buffer-line buf 0))
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "base entry3") ()
            "Test 2e failed: expected 'base entry3', got '~A'" (buffer-line buf 0))
    ;; Redo twice to get back to entry1
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "base entry2") ()
            "Test 2f failed: expected 'base entry2', got '~A'" (buffer-line buf 0))
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "base entry1") ()
            "Test 2g failed: expected 'base entry1', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Multiple yank-pop undo/redo~%"))
  
  (format t "All yank-pop undo/redo tests passed!~%~%"))

(defun test-yank-pop-twice-in-row ()
  "Test yank-pop called twice in a row"
  (format t "Running yank-pop twice in a row tests...~%")
  
  ;; Test: yank-pop called twice in succession should work correctly
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("start "))
    (buffer-set-point buf 0 6)
    ;; Build kill ring with 4 entries to test wrapping
    (add-to-kill-ring buf "alpha")
    (add-to-kill-ring buf "beta")
    (add-to-kill-ring buf "gamma")
    (add-to-kill-ring buf "delta")
    
    ;; Yank should get "delta" (most recent)
    (yank buf)
    (assert (string= (buffer-line buf 0) "start delta") ()
            "Setup failed: expected 'start delta', got '~A'" (buffer-line buf 0))
    
    ;; First yank-pop should get "gamma"
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "start gamma") ()
            "First yank-pop failed: expected 'start gamma', got '~A'" (buffer-line buf 0))
    
    ;; Second yank-pop (immediately after first) should get "beta"
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "start beta") ()
            "Second yank-pop failed: expected 'start beta', got '~A'" (buffer-line buf 0))
    
    ;; Point should be at correct position after text
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 10)) () 
              "Point position failed: expected (0 10), got ~A" point))
    
    (format t "✓ Test passed: yank-pop twice in a row works correctly~%"))
  
  (format t "All yank-pop twice in a row tests passed!~%~%"))

(defun test-yank-pop-edge-cases ()
  "Test yank-pop edge cases and boundary conditions"
  (format t "Running yank-pop edge case tests...~%")
  
  ;; Test 1: yank-pop at beginning of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello"))
    (buffer-set-point buf 0 0)  ; At beginning
    (add-to-kill-ring buf "first")
    (add-to-kill-ring buf "second")
    (yank buf)
    (assert (string= (buffer-line buf 0) "secondhello") ()
            "Test 1a failed: expected 'secondhello', got '~A'" (buffer-line buf 0))
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "firsthello") ()
            "Test 1b failed: expected 'firsthello', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: yank-pop at beginning of buffer~%"))
  
  ;; Test 2: yank-pop at end of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("prefix"))
    (buffer-set-point buf 0 6)  ; At end
    (add-to-kill-ring buf "one")
    (add-to-kill-ring buf "two")
    (yank buf)
    (assert (string= (buffer-line buf 0) "prefixtwo") ()
            "Test 2a failed: expected 'prefixtwo', got '~A'" (buffer-line buf 0))
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "prefixone") ()
            "Test 2b failed: expected 'prefixone', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: yank-pop at end of buffer~%"))
  
  ;; Test 3: yank-pop cycling through same entries multiple times
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test "))
    (buffer-set-point buf 0 5)
    (add-to-kill-ring buf "A")
    (add-to-kill-ring buf "B")
    (yank buf)
    (assert (string= (buffer-line buf 0) "test B") ()
            "Test 3a failed: expected 'test B', got '~A'" (buffer-line buf 0))
    ;; Cycle through: B -> A -> B -> A -> B
    (yank-pop buf)  ; A
    (assert (string= (buffer-line buf 0) "test A") ()
            "Test 3b failed: expected 'test A', got '~A'" (buffer-line buf 0))
    (yank-pop buf)  ; B
    (assert (string= (buffer-line buf 0) "test B") ()
            "Test 3c failed: expected 'test B', got '~A'" (buffer-line buf 0))
    (yank-pop buf)  ; A
    (assert (string= (buffer-line buf 0) "test A") ()
            "Test 3d failed: expected 'test A', got '~A'" (buffer-line buf 0))
    (yank-pop buf)  ; B
    (assert (string= (buffer-line buf 0) "test B") ()
            "Test 3e failed: expected 'test B', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 3 passed: Multiple cycling through kill ring~%"))
  
  (format t "All yank-pop edge case tests passed!~%~%"))

(defun test-yank-pop-interaction-with-kill-operations ()
  "Test yank-pop interaction with kill operations"
  (format t "Running yank-pop interaction tests...~%")
  
  ;; Test: yank-pop after new kill operations should reset state
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("original text to test"))
    (buffer-set-point buf 0 9)  ; After "original "
    
    ;; Build initial kill ring
    (add-to-kill-ring buf "old1")
    (add-to-kill-ring buf "old2")
    
    ;; Yank and yank-pop
    (yank buf)
    (assert (string= (buffer-line buf 0) "original old2text to test") ()
            "Setup 1 failed: expected 'original old2text to test', got '~A'" (buffer-line buf 0))
    (yank-pop buf)
    (assert (string= (buffer-line buf 0) "original old1text to test") ()
            "Setup 2 failed: expected 'original old1text to test', got '~A'" (buffer-line buf 0))
    
    ;; Now perform a new kill operation
    (buffer-set-point buf 0 13)  ; After "original old1"
    (buffer-set-mark buf 0 17)   ; Select "text"
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "original old1 to test") ()
            "Kill operation failed: expected 'original old1 to test', got '~A'" (buffer-line buf 0))
    
    ;; yank-pop should not work now because last operation wasn't yank
    (let ((before-line (buffer-line buf 0)))
      (yank-pop buf)
      (assert (string= (buffer-line buf 0) before-line) ()
              "yank-pop should not work after kill operation, but line changed to '~A'" (buffer-line buf 0)))
    
    ;; But regular yank should work with new kill ring content
    (buffer-set-point buf 0 13)  ; After "original old1"
    (yank buf)
    (assert (string= (buffer-line buf 0) "original old1text to test") ()
            "Yank after kill failed: expected 'original old1text to test', got '~A'" (buffer-line buf 0))
    
    (format t "✓ Test passed: yank-pop interaction with kill operations~%"))
  
  (format t "All yank-pop interaction tests passed!~%~%"))

(defun run-all-yank-pop-tests ()
  "Run all yank-pop tests"
  (format t "~%==========================================~%")
  (format t "Running Yank-Pop Tests~%")
  (format t "==========================================~%~%")
  
  (handler-case
      (progn
        (test-yank-pop-basic)
        (test-yank-pop-multiline)
        (test-yank-pop-undo-redo)
        (test-yank-pop-twice-in-row)
        (test-yank-pop-edge-cases)
        (test-yank-pop-interaction-with-kill-operations)
        (format t "~%==========================================~%")
        (format t "All yank-pop tests passed successfully!~%")
        (format t "==========================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "==========================================~%")))
  
  t)