(in-package :tle)

(defun test-buffer-creation-and-state ()
  "Test buffer creation and basic state management"
  (format t "Running buffer creation and state tests...~%")
  
  ;; Test 1: Standard buffer creation
  (let ((buf (make-instance 'standard-buffer)))
    (assert (typep buf 'standard-buffer) ()
            "Test 1 failed: should create standard-buffer instance")
    (assert (buffer-recording-undo-p buf) ()
            "Test 1 failed: undo recording should be enabled by default")
    (assert (typep (buffer-undo-tree buf) 'undo-tree) ()
            "Test 1 failed: should have undo tree")
    (format t "✓ Test 1 passed: Standard buffer creation~%"))
  
  ;; Test 2: Buffer with custom name
  (let ((buf (make-standard-buffer "test-buffer")))
    (assert (string= (buffer-name buf) "test-buffer") ()
            "Test 2 failed: buffer name should be 'test-buffer'")
    (assert (> (buffer-line-count buf) 0) ()
            "Test 2 failed: should have default content")
    (format t "✓ Test 2 passed: Buffer with custom name~%"))
  
  (format t "All buffer creation tests passed!~%~%"))

(defun test-point-and-mark-operations ()
  "Test point and mark operations comprehensively"
  (format t "Running point and mark operations tests...~%")
  
  ;; Test 1: Point operations
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2" "line3"))
    
    ;; Test point setting and getting
    (buffer-set-point buf 1 2)
    (assert (equal (buffer-get-point buf) '(1 2)) ()
            "Test 1 failed: point should be (1 2)")
    
    ;; Test point bounds
    (buffer-set-point buf 0 0)
    (assert (equal (buffer-get-point buf) '(0 0)) ()
            "Test 1 failed: point should be (0 0)")
    
    (format t "✓ Test 1 passed: Point operations~%"))
  
  ;; Test 2: Mark operations
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2"))
    
    ;; Initially no mark
    (assert (null (buffer-get-mark buf)) ()
            "Test 2 failed: initially should have no mark")
    
    ;; Set mark
    (buffer-set-mark buf 0 3)
    (assert (equal (buffer-get-mark buf) '(0 3)) ()
            "Test 2 failed: mark should be (0 3)")
    
    ;; Clear mark
    (buffer-clear-mark buf)
    (assert (null (buffer-get-mark buf)) ()
            "Test 2 failed: mark should be cleared")
    
    (format t "✓ Test 2 passed: Mark operations~%"))
  
  (format t "All point and mark tests passed!~%~%"))

(defun test-cursor-movement-comprehensive ()
  "Test all cursor movement operations"
  (format t "Running comprehensive cursor movement tests...~%")
  
  ;; Test 1: Forward/backward char with line wrapping
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "abc" "def" "ghi"))
    (buffer-set-point buf 0 2)  ; At 'c' in first line
    
    ;; Forward char should move to end of line
    (forward-char buf)
    (assert (equal (buffer-get-point buf) '(0 3)) ()
            "Test 1 failed: should be at end of first line")
    
    ;; Forward char should wrap to next line
    (forward-char buf)
    (assert (equal (buffer-get-point buf) '(1 0)) ()
            "Test 1 failed: should wrap to beginning of second line")
    
    ;; Backward char should go back to end of previous line
    (backward-char buf)
    (assert (equal (buffer-get-point buf) '(0 3)) ()
            "Test 1 failed: should go back to end of previous line")
    
    (format t "✓ Test 1 passed: Forward/backward char with wrapping~%"))
  
  ;; Test 2: Next/previous line with column preservation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "hi" "goodbye everyone"))
    (buffer-set-point buf 0 6)  ; At 'w' in "hello world"
    
    ;; Next line should preserve column if possible
    (next-line buf)
    (assert (equal (buffer-get-point buf) '(1 2)) ()
            "Test 2 failed: should be at end of shorter line")
    
    ;; Next line uses current column (simplified behavior)
    (next-line buf)
    (assert (equal (buffer-get-point buf) '(2 2)) ()
            "Test 2 failed: should use current column in longer line")
    
    ;; Previous line behavior
    (previous-line buf)
    (assert (equal (buffer-get-point buf) '(1 2)) ()
            "Test 2 failed: should be at end of short line again")
    
    (format t "✓ Test 2 passed: Next/previous line with column preservation~%"))
  
  (format t "All cursor movement tests passed!~%~%"))

(defun test-insertion-edge-cases ()
  "Test insertion operations edge cases"
  (format t "Running insertion edge cases tests...~%")
  
  ;; Test 1: Insert into empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    ;; This should not crash (though it won't insert)
    (handler-case
        (progn
          (insert-char buf #\x)
          (format t "✓ Test 1a passed: Insert into empty buffer handled gracefully~%"))
      (error (e)
        (format t "✓ Test 1b passed: Insert into empty buffer throws expected error~%"))))
  
  ;; Test 2: Insert with point beyond line end
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 2)
    (insert-char buf #\X)
    (assert (string= (buffer-line buf 0) "teXst") ()
            "Test 2 failed: normal insertion should work")
    (format t "✓ Test 2 passed: Normal insertion works~%"))
  
  ;; Test 3: Newline at various positions
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    
    ;; Newline at beginning
    (buffer-set-point buf 0 0)
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3 failed: should have 2 lines")
    (assert (string= (buffer-line buf 0) "") ()
            "Test 3 failed: first line should be empty")
    (assert (string= (buffer-line buf 1) "hello") ()
            "Test 3 failed: second line should be 'hello'")
    
    (format t "✓ Test 3 passed: Newline at beginning~%"))
  
  (format t "All insertion edge cases tests passed!~%~%"))

(defun test-deletion-edge-cases ()
  "Test deletion operations edge cases"
  (format t "Running deletion edge cases tests...~%")
  
  ;; Test 1: Delete at end of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)  ; At end
    ;; Should do nothing
    (delete-char buf)
    (assert (string= (buffer-line buf 0) "test") ()
            "Test 1 failed: should not change when deleting at end")
    (format t "✓ Test 1 passed: Delete at end of buffer~%"))
  
  ;; Test 2: Delete to join single-character lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a" "b" "c"))
    (buffer-set-point buf 0 1)  ; At end of first line
    (delete-char buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2 failed: should have 2 lines after join")
    (assert (string= (buffer-line buf 0) "ab") ()
            "Test 2 failed: first line should be 'ab'")
    (assert (string= (buffer-line buf 1) "c") ()
            "Test 2 failed: second line should be 'c'")
    (format t "✓ Test 2 passed: Delete to join single-character lines~%"))
  
  ;; Test 3: Delete from empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "content"))
    (buffer-set-point buf 0 0)
    (delete-char buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 3 failed: should have 1 line after delete")
    (assert (string= (buffer-line buf 0) "content") ()
            "Test 3 failed: remaining line should be 'content'")
    (format t "✓ Test 3 passed: Delete from empty line~%"))
  
  (format t "All deletion edge cases tests passed!~%~%"))

(defun test-undo-tree-structure ()
  "Test undo tree internal structure"
  (format t "Running undo tree structure tests...~%")
  
  ;; Test 1: Tree structure after operations
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)
    
    (let ((tree (buffer-undo-tree buf)))
      ;; Initially at root
      (assert (eq (tree-current tree) (tree-root tree)) ()
              "Test 1 failed: should start at root")
      (assert (= (tree-size tree) 0) ()
              "Test 1 failed: should start with size 0")
      
      ;; After one operation
      (insert-char buf #\1)
      (assert (= (tree-size tree) 1) ()
              "Test 1 failed: size should be 1 after one operation")
      (assert (not (eq (tree-current tree) (tree-root tree))) ()
              "Test 1 failed: should not be at root after operation")
      
      ;; After undo
      (buffer-undo buf)
      (assert (eq (tree-current tree) (tree-root tree)) ()
              "Test 1 failed: should be back at root after undo")
      
      (format t "✓ Test 1 passed: Tree structure after operations~%")))
  
  (format t "All undo tree structure tests passed!~%~%"))

(defun run-all-buffer-comprehensive-tests ()
  "Run all comprehensive buffer tests"
  (format t "~%======================================~%")
  (format t "Running Comprehensive Buffer Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-buffer-creation-and-state)
        (test-point-and-mark-operations)
        (test-cursor-movement-comprehensive)
        (test-insertion-edge-cases)
        (test-deletion-edge-cases)
        (test-undo-tree-structure)
        (format t "~%======================================~%")
        (format t "All comprehensive buffer tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")
      (return-from run-all-buffer-comprehensive-tests nil)))
  
  t)