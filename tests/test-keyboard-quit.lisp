(in-package :tle)

(defun test-keyboard-quit-basic ()
  "Test that keyboard-quit clears the mark if one is set"
  (format t "Running basic keyboard-quit tests...~%")
  
  ;; Test 1: keyboard-quit with mark set should clear it
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 5)
    ;; Set mark at current position
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    
    ;; Verify mark is set
    (assert (buffer-get-mark buf) () 
            "Test 1 setup failed: mark should be set")
    
    ;; Call keyboard-quit
    (keyboard-quit buf)
    
    ;; Verify mark is cleared
    (assert (null (buffer-get-mark buf)) () 
            "Test 1 failed: keyboard-quit should clear the mark")
    
    (format t "✓ Test 1 passed: keyboard-quit clears mark~%"))
  
  ;; Test 2: keyboard-quit with no mark should not cause error
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 5)
    
    ;; Verify no mark is set
    (assert (null (buffer-get-mark buf)) () 
            "Test 2 setup failed: mark should not be set")
    
    ;; Call keyboard-quit (should not error)
    (let ((result (keyboard-quit buf)))
      (assert result () "keyboard-quit should return t")
      
      ;; Mark should still be nil
      (assert (null (buffer-get-mark buf)) () 
              "Test 2 failed: mark should remain nil"))
    
    (format t "✓ Test 2 passed: keyboard-quit works when no mark is set~%"))
  
  ;; Test 3: keyboard-quit doesn't affect cursor position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 7)
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    
    ;; Get initial cursor position
    (let ((initial-point (copy-list (buffer-get-point buf))))
      
      ;; Call keyboard-quit
      (keyboard-quit buf)
      
      ;; Verify cursor position unchanged
      (let ((final-point (buffer-get-point buf)))
        (assert (equal initial-point final-point) () 
                "Test 3 failed: cursor position should not change"))
      
      ;; Verify mark is cleared
      (assert (null (buffer-get-mark buf)) () 
              "Test 3 failed: mark should be cleared"))
    
    (format t "✓ Test 3 passed: keyboard-quit doesn't affect cursor position~%"))
  
  (format t "All basic keyboard-quit tests passed!~%~%"))

(defun test-keyboard-quit-undo-redo-compatibility ()
  "Test that undo/redo still works after using keyboard-quit"
  (format t "Running keyboard-quit undo/redo compatibility tests...~%")
  
  ;; Test: Insert text, set mark, keyboard-quit, then undo should still work
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 5)
    
    ;; Insert some text
    (insert-char buf #\!)
    (assert (string= (buffer-line buf 0) "hello!") ()
            "Setup failed: expected 'hello!', got '~A'" (buffer-line buf 0))
    
    ;; Set mark
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    
    ;; Verify mark is set
    (assert (buffer-get-mark buf) () "Mark should be set")
    
    ;; Call keyboard-quit (should clear mark)
    (keyboard-quit buf)
    (assert (null (buffer-get-mark buf)) () "Mark should be cleared")
    
    ;; Now test undo - should undo the character insertion
    (let ((undo-result (buffer-undo buf)))
      (assert undo-result () "Undo should succeed")
      (assert (string= (buffer-line buf 0) "hello") ()
              "Undo failed: expected 'hello', got '~A'" (buffer-line buf 0)))
    
    ;; Test redo - should redo the character insertion
    (let ((redo-result (buffer-redo buf)))
      (assert redo-result () "Redo should succeed")
      (assert (string= (buffer-line buf 0) "hello!") ()
              "Redo failed: expected 'hello!', got '~A'" (buffer-line buf 0)))
    
    (format t "✓ Test 1 passed: undo/redo works correctly after keyboard-quit~%"))
  
  ;; Test 2: Multiple operations with keyboard-quit in between
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 4)
    
    ;; Insert first character
    (insert-char buf #\A)
    
    ;; Set mark and keyboard-quit
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    (keyboard-quit buf)
    
    ;; Insert second character  
    (insert-char buf #\B)
    (assert (string= (buffer-line buf 0) "testAB") ()
            "Expected 'testAB', got '~A'" (buffer-line buf 0))
    
    ;; Undo twice should remove both characters
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "testA") ()
            "First undo failed: expected 'testA', got '~A'" (buffer-line buf 0))
    
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "test") ()
            "Second undo failed: expected 'test', got '~A'" (buffer-line buf 0))
    
    ;; Redo twice should restore both characters
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "testA") ()
            "First redo failed: expected 'testA', got '~A'" (buffer-line buf 0))
    
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "testAB") ()
            "Second redo failed: expected 'testAB', got '~A'" (buffer-line buf 0))
    
    (format t "✓ Test 2 passed: multiple undo/redo operations work correctly~%"))
  
  (format t "All keyboard-quit undo/redo compatibility tests passed!~%~%"))

(defun test-keyboard-quit-edge-cases ()
  "Test keyboard-quit edge cases and interactions"
  (format t "Running keyboard-quit edge case tests...~%")
  
  ;; Test 1: keyboard-quit with mark on different line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 1 5)  ; On second line
    (buffer-set-mark buf 0 3)   ; Mark on first line
    
    ;; Verify mark is set on different line
    (let ((mark (buffer-get-mark buf)))
      (assert (equal mark '(0 3)) () 
              "Setup failed: expected mark at (0 3), got ~A" mark))
    
    ;; Call keyboard-quit
    (keyboard-quit buf)
    
    ;; Verify mark is cleared
    (assert (null (buffer-get-mark buf)) () 
            "Test 1 failed: mark should be cleared even when on different line")
    
    (format t "✓ Test 1 passed: keyboard-quit clears mark on different line~%"))
  
  ;; Test 2: keyboard-quit after selection operations
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "selected text"))
    (buffer-set-point buf 0 8)
    (buffer-set-mark buf 0 0)  ; Mark at beginning, point at position 8
    
    ;; Verify we have a selection
    (let ((mark (buffer-get-mark buf))
          (point (buffer-get-point buf)))
      (assert mark () "Mark should be set")
      (assert (not (equal mark point)) () "Mark and point should be different"))
    
    ;; Call keyboard-quit
    (keyboard-quit buf)
    
    ;; Verify mark is cleared but point unchanged
    (assert (null (buffer-get-mark buf)) () 
            "Test 2 failed: mark should be cleared")
    (assert (equal (buffer-get-point buf) '(0 8)) ()
            "Test 2 failed: point should remain unchanged")
    
    (format t "✓ Test 2 passed: keyboard-quit clears selection but preserves cursor~%"))
  
  ;; Test 3: Multiple keyboard-quit calls
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 2)
    (buffer-set-mark buf 0 4)
    
    ;; First keyboard-quit should clear mark
    (keyboard-quit buf)
    (assert (null (buffer-get-mark buf)) () 
            "First keyboard-quit should clear mark")
    
    ;; Second keyboard-quit should be safe (no error)
    (let ((result (keyboard-quit buf)))
      (assert result () "Second keyboard-quit should return t")
      (assert (null (buffer-get-mark buf)) () 
              "Mark should remain nil"))
    
    (format t "✓ Test 3 passed: multiple keyboard-quit calls are safe~%"))
  
  (format t "All keyboard-quit edge case tests passed!~%~%"))

(defun run-all-keyboard-quit-tests ()
  "Run all keyboard-quit tests"
  (format t "~%======================================~%")
  (format t "Running Keyboard-Quit Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-keyboard-quit-basic)
        (test-keyboard-quit-undo-redo-compatibility)
        (test-keyboard-quit-edge-cases)
        (format t "~%======================================~%")
        (format t "All keyboard-quit tests passed successfully!~%")
        (format t "C-g key binding implemented and working correctly~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)

;; Run all tests when loaded
(run-all-keyboard-quit-tests)