(in-package :tle)

(defun test-dirty-bit-basic ()
  "Test basic dirty bit functionality"
  (format t "Running basic dirty bit tests...~%")
  
  ;; Test 1: New buffer starts clean
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (assert (not (buffer-dirty-p buf)) ()
            "Test 1 failed: new buffer should not be dirty")
    (format t "✓ Test 1 passed: New buffer starts clean~%"))
  
  ;; Test 2: Buffer becomes dirty after modification
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)
    (insert-char buf #\X)
    (assert (buffer-dirty-p buf) ()
            "Test 2 failed: buffer should be dirty after modification")
    (format t "✓ Test 2 passed: Buffer becomes dirty after modification~%"))
  
  ;; Test 3: Buffer becomes clean after save
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)
    (insert-char buf #\X)
    (assert (buffer-dirty-p buf) ()
            "Test 3 setup failed: buffer should be dirty")
    ;; Manually mark as clean (simulating successful save)
    (mark-buffer-clean buf)
    (assert (not (buffer-dirty-p buf)) ()
            "Test 3 failed: buffer should be clean after save")
    (format t "✓ Test 3 passed: Buffer becomes clean after save~%"))
  
  (format t "All basic dirty bit tests passed!~%~%"))

(defun test-dirty-bit-undo-bug ()
  "Test dirty bit behavior with save/modify/undo - this should reveal the bug"
  (format t "Running dirty bit undo bug tests...~%")
  
  ;; Test the problematic scenario: save, modify, undo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)
    
    ;; Start clean (as if just saved)
    (mark-buffer-clean buf)
    (assert (not (buffer-dirty-p buf)) ()
            "Setup failed: buffer should start clean")
    
    ;; Make a modification - should become dirty
    (insert-char buf #\X)
    (assert (buffer-dirty-p buf) ()
            "Test failed: buffer should be dirty after modification")
    
    ;; Undo the modification - should become clean again since we're back to saved state
    (buffer-undo buf)
    (assert (not (buffer-dirty-p buf)) ()
            "BUG: buffer should be clean after undoing to saved state, but it's still dirty")
    
    (format t "✓ Test passed: Save/modify/undo correctly manages dirty bit~%"))
  
  (format t "All dirty bit undo bug tests passed!~%~%"))

(defun test-dirty-bit-complex-undo ()
  "Test dirty bit behavior with complex undo scenarios"
  (format t "Running complex dirty bit undo tests...~%")
  
  ;; Test 1: Multiple modifications and undos
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)
    
    ;; Start clean (saved state)
    (mark-buffer-clean buf)
    
    ;; Make multiple modifications
    (insert-char buf #\1)  ; test1
    (insert-char buf #\2)  ; test12
    (insert-char buf #\3)  ; test123
    (assert (buffer-dirty-p buf) ()
            "Buffer should be dirty after modifications")
    
    ;; Undo back to saved state
    (buffer-undo buf)  ; test12
    (assert (buffer-dirty-p buf) ()
            "Buffer should still be dirty (not at saved state yet)")
    (buffer-undo buf)  ; test1
    (assert (buffer-dirty-p buf) ()
            "Buffer should still be dirty (not at saved state yet)")
    (buffer-undo buf)  ; test (back to saved state)
    (assert (not (buffer-dirty-p buf)) ()
            "Buffer should be clean after undoing back to saved state")
    
    (format t "✓ Test 1 passed: Multiple modifications and undos~%"))
  
  ;; Test 2: Undo past saved state, then redo to saved state
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 5)
    
    ;; Make a change, then mark as saved
    (insert-char buf #\X)  ; helloX
    (mark-buffer-clean buf)  ; This is now our "saved" state
    
    ;; Make another change
    (insert-char buf #\Y)  ; helloXY
    (assert (buffer-dirty-p buf) ()
            "Buffer should be dirty after new modification")
    
    ;; Undo back to saved state
    (buffer-undo buf)  ; helloX
    (assert (not (buffer-dirty-p buf)) ()
            "Buffer should be clean at saved state")
    
    ;; Undo past saved state
    (buffer-undo buf)  ; hello
    (assert (buffer-dirty-p buf) ()
            "Buffer should be dirty when past saved state")
    
    ;; Redo back to saved state
    (buffer-redo buf)  ; helloX
    (assert (not (buffer-dirty-p buf)) ()
            "Buffer should be clean when back at saved state via redo")
    
    (format t "✓ Test 2 passed: Undo past saved state, then redo to saved state~%"))
  
  (format t "All complex dirty bit undo tests passed!~%~%"))

(defun run-all-dirty-bit-tests ()
  "Run all dirty bit tests"
  (format t "~%======================================~%")
  (format t "Running Dirty Bit Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-dirty-bit-basic)
        (test-dirty-bit-undo-bug)
        (test-dirty-bit-complex-undo)
        (format t "~%======================================~%")
        (format t "All dirty bit tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")
      (return-from run-all-dirty-bit-tests nil)))
  
  t)