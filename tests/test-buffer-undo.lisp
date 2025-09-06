(in-package :tle)

(defun test-undo-insert-char ()
  "Test undo functionality for character insertion"
  (format t "Running undo insert-char tests...~%")
  
  ;; Test 1: Insert char and undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 2)  ; Position at 'l' in "hello"
    (insert-char buf #\X)
    (assert (string= (buffer-line buf 0) "heXllo") () 
            "Test 1 setup failed: expected 'heXllo', got '~A'" (buffer-line buf 0))
    (assert (equal (buffer-get-point buf) '(0 3)) ()
            "Test 1 setup failed: point should be at (0 3)")
    
    ;; Undo the insertion
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "hello") () 
            "Test 1 failed: expected 'hello' after undo, got '~A'" (buffer-line buf 0))
    (assert (equal (buffer-get-point buf) '(0 2)) ()
            "Test 1 failed: point should be back at (0 2)")
    (format t "✓ Test 1 passed: Insert char and undo~%"))
  
  ;; Test 2: Multiple insertions and undos
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)  ; At end of "test"
    (insert-char buf #\1)
    (insert-char buf #\2)
    (insert-char buf #\3)
    (assert (string= (buffer-line buf 0) "test123") () 
            "Test 2 setup failed: expected 'test123'")
    
    ;; Undo insertions one by one
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test12") () 
            "Test 2 failed: first undo should give 'test12'")
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test1") () 
            "Test 2 failed: second undo should give 'test1'")
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test") () 
            "Test 2 failed: third undo should give 'test'")
    (format t "✓ Test 2 passed: Multiple insertions and undos~%"))
  
  (format t "All undo insert-char tests passed!~%~%"))

(defun test-undo-delete-char ()
  "Test undo functionality for character deletion"
  (format t "Running undo delete-char tests...~%")
  
  ;; Test 1: Delete char and undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 2)  ; Position at third 'l' in "hello"
    (delete-char buf)
    (assert (string= (buffer-line buf 0) "helo") () 
            "Test 1 setup failed: expected 'helo' after delete")
    
    ;; Undo the deletion
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "hello") () 
            "Test 1 failed: expected 'hello' after undo, got '~A'" (buffer-line buf 0))
    (assert (equal (buffer-get-point buf) '(0 2)) ()
            "Test 1 failed: point should be back at (0 2)")
    (format t "✓ Test 1 passed: Delete char and undo~%"))
  
  ;; Test 2: Delete at end of line (joining lines) and undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello" "world"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (delete-char buf)
    (assert (string= (buffer-line buf 0) "helloworld") () 
            "Test 2 setup failed: expected 'helloworld' after delete")
    (assert (= (buffer-line-count buf) 1) ()
            "Test 2 setup failed: should have 1 line after join")
    
    ;; Undo the deletion (should split lines back)
    (buffer-undo buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2 failed: should have 2 lines after undo")
    (assert (string= (buffer-line buf 0) "hello") () 
            "Test 2 failed: first line should be 'hello'")
    (assert (string= (buffer-line buf 1) "world") () 
            "Test 2 failed: second line should be 'world'")
    (assert (equal (buffer-get-point buf) '(0 5)) ()
            "Test 2 failed: point should be back at (0 5)")
    (format t "✓ Test 2 passed: Delete at end of line and undo~%"))
  
  (format t "All undo delete-char tests passed!~%~%"))

(defun test-undo-insert-newline ()
  "Test undo functionality for newline insertion"
  (format t "Running undo insert-newline tests...~%")
  
  ;; Test 1: Insert newline and undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)  ; Between "hello" and " world"
    (insert-newline buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 1 setup failed: should have 2 lines after newline")
    (assert (string= (buffer-line buf 0) "hello") () 
            "Test 1 setup failed: first line should be 'hello'")
    (assert (string= (buffer-line buf 1) " world") () 
            "Test 1 setup failed: second line should be ' world'")
    
    ;; Undo the newline insertion
    (buffer-undo buf)
    (assert (= (buffer-line-count buf) 1) ()
            "Test 1 failed: should have 1 line after undo")
    (assert (string= (buffer-line buf 0) "hello world") () 
            "Test 1 failed: line should be 'hello world' after undo")
    (assert (equal (buffer-get-point buf) '(0 5)) ()
            "Test 1 failed: point should be back at (0 5)")
    (format t "✓ Test 1 passed: Insert newline and undo~%"))
  
  (format t "All undo insert-newline tests passed!~%~%"))

(defun test-redo-operations ()
  "Test redo functionality"
  (format t "Running redo tests...~%")
  
  ;; Test 1: Undo then redo character insertion
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 2)
    (insert-char buf #\X)
    (assert (string= (buffer-line buf 0) "teXst") ()
            "Test 1 setup failed")
    
    ;; Undo then redo
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test") ()
            "Test 1 undo failed")
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "teXst") ()
            "Test 1 failed: redo should restore 'teXst'")
    (assert (equal (buffer-get-point buf) '(0 3)) ()
            "Test 1 failed: point should be at (0 3) after redo")
    (format t "✓ Test 1 passed: Undo then redo character insertion~%"))
  
  ;; Test 2: Multiple operations with undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "abc"))
    (buffer-set-point buf 0 1)
    (insert-char buf #\1)  ; a1bc
    (insert-char buf #\2)  ; a12bc
    (delete-char buf)      ; a12c (deletes 'b')
    
    ;; Undo all operations
    (buffer-undo buf)      ; back to a12bc
    (assert (string= (buffer-line buf 0) "a12bc") ()
            "Test 2 failed: first undo should give 'a12bc'")
    (buffer-undo buf)      ; back to a1bc
    (assert (string= (buffer-line buf 0) "a1bc") ()
            "Test 2 failed: second undo should give 'a1bc'")
    (buffer-undo buf)      ; back to abc
    (assert (string= (buffer-line buf 0) "abc") ()
            "Test 2 failed: third undo should give 'abc'")
    
    ;; Redo operations
    (buffer-redo buf)      ; forward to a1bc
    (assert (string= (buffer-line buf 0) "a1bc") ()
            "Test 2 failed: first redo should give 'a1bc'")
    (buffer-redo buf)      ; forward to a12bc
    (assert (string= (buffer-line buf 0) "a12bc") ()
            "Test 2 failed: second redo should give 'a12bc'")
    (buffer-redo buf)      ; forward to a12c
    (assert (string= (buffer-line buf 0) "a12c") ()
            "Test 2 failed: third redo should give 'a12c'")
    
    (format t "✓ Test 2 passed: Multiple operations with undo/redo~%"))
  
  (format t "All redo tests passed!~%~%"))

(defun test-undo-tree-branching ()
  "Test undo tree branching behavior"
  (format t "Running undo tree branching tests...~%")
  
  ;; Test 1: Create branch in undo tree
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)
    
    ;; Create initial sequence
    (insert-char buf #\1)  ; test1
    (insert-char buf #\2)  ; test12
    
    ;; Undo back to test1
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test1") ()
            "Test 1 setup failed: should be 'test1' after undo")
    
    ;; Create a branch by inserting different character
    (insert-char buf #\A)  ; test1A
    (assert (string= (buffer-line buf 0) "test1A") ()
            "Test 1 failed: should be 'test1A' after branching")
    
    ;; The undo tree now has a branch:
    ;; test -> test1 -> test12 (original branch)
    ;;              \-> test1A (new branch)
    
    (format t "✓ Test 1 passed: Create branch in undo tree~%"))
  
  (format t "All undo tree branching tests passed!~%~%"))

(defun test-undo-disabled ()
  "Test behavior when undo recording is disabled"
  (format t "Running undo disabled tests...~%")
  
  ;; Test 1: Operations with undo recording disabled
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)
    
    ;; Record one operation normally
    (insert-char buf #\1)  ; test1
    
    ;; Disable undo recording
    (setf (buffer-recording-undo-p buf) nil)
    (insert-char buf #\2)  ; test12
    (insert-char buf #\3)  ; test123
    
    ;; Re-enable undo recording
    (setf (buffer-recording-undo-p buf) t)
    (insert-char buf #\4)  ; test1234
    
    ;; Undo should only undo the last operation (insert '4')
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test123") ()
            "Test 1 failed: first undo should give 'test123'")
    
    ;; Next undo attempts to undo the '1' insertion, but buffer state has changed
    ;; This will delete character at the original position, which is now '2'
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test23") ()
            "Test 1 failed: second undo gives 'test23' due to state mismatch")
    
    (format t "✓ Test 1 passed: Operations with undo recording disabled show expected behavior~%"))
  
  (format t "All undo disabled tests passed!~%~%"))

(defun run-all-buffer-undo-tests ()
  "Run all buffer undo/redo tests"
  (format t "~%======================================~%")
  (format t "Running Buffer Undo/Redo Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-undo-insert-char)
        (test-undo-delete-char)
        (test-undo-insert-newline)
        (test-redo-operations)
        (test-undo-tree-branching)
        (test-undo-disabled)
        (format t "~%======================================~%")
        (format t "All undo/redo tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")
      (return-from run-all-buffer-undo-tests nil)))
  
  t)