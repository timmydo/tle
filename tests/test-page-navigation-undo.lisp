(in-package :tle)

(defun test-page-navigation-undo-behavior ()
  "Test that page navigation operations do NOT create undo records (which is correct behavior)"
  (format t "Running page navigation undo behavior tests...~%")
  
  ;; Test 1: Page navigation should not create undo records
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 50 :fill-pointer 50)))
    ;; Create 50 lines of test content
    (loop for i from 0 to 49
          do (setf (aref lines i) (format nil "Line ~A content" i)))
    (setf (lines buf) lines)
    
    ;; Start at beginning and insert some text (this SHOULD be undoable)
    (buffer-set-point buf 0 0)
    (insert-char buf #\X)
    (insert-char buf #\Y)
    (assert (string= (buffer-line buf 0) "XYLine 0 content") () 
            "Test setup failed")
    
    ;; Move around with page navigation (this should NOT be undoable)
    (page-down buf)
    (page-down buf)
    (page-up buf)
    (let ((current-point (buffer-get-point buf)))
      (assert (equal current-point '(20 2)) () "Page navigation setup failed"))
    
    ;; Now undo - should only undo the text insertions, not cursor movements
    (buffer-undo buf)  ; Should undo the 'Y' insertion
    (assert (string= (buffer-line buf 0) "XLine 0 content") () 
            "First undo should remove 'Y'")
    (assert (equal (buffer-get-point buf) '(0 1)) () 
            "First undo should restore point to after 'X'")
    
    (buffer-undo buf)  ; Should undo the 'X' insertion
    (assert (string= (buffer-line buf 0) "Line 0 content") () 
            "Second undo should remove 'X'")
    (assert (equal (buffer-get-point buf) '(0 0)) () 
            "Second undo should restore point to beginning")
    
    ;; Try to undo again - should do nothing (no more undo records)
    (let ((undo-result (buffer-undo buf)))
      (assert (not undo-result) () 
              "Third undo should return nil (no more undos available)"))
    
    (format t "✓ Test 1 passed: Page navigation does not create undo records~%"))
  
  ;; Test 2: Verify redo also only affects content operations
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 30 :fill-pointer 30)))
    (loop for i from 0 to 29
          do (setf (aref lines i) (format nil "Line ~A" i)))
    (setf (lines buf) lines)
    
    ;; Insert text, do page navigation, then undo/redo
    (buffer-set-point buf 0 0)
    (insert-char buf #\A)
    (let ((point-after-a (buffer-get-point buf)))
      (format t "Point after inserting A: ~A~%" point-after-a))
    (page-down buf)  ; Should not be recorded in undo history
    (let ((point-after-page-down (buffer-get-point buf)))
      (format t "Point after page-down: ~A~%" point-after-page-down))
    (insert-char buf #\B)  ; Insert at new location
    (let ((point-after-b (buffer-get-point buf)))
      (format t "Point after inserting B: ~A~%" point-after-b))
    
    ;; Verify state after operations
    (assert (string= (buffer-line buf 0) "ALine 0") () "First line should have 'A', got ~A" (buffer-line buf 0))
    (let ((line-20 (buffer-line buf 20)))
      (format t "Line 20 content: '~A'~%" line-20)
      ;; Note: page-down preserves column position, so cursor is at column 1 when B is inserted
      (assert (string= line-20 "LBine 20") () "Line 20 should have 'B' at column 1, got '~A'" line-20))
    
    ;; Undo operations
    (buffer-undo buf)  ; Should undo 'B' insertion and restore point
    (let ((point (buffer-get-point buf)))
      (format t "Point after undoing B: ~A~%" point)
      (assert (equal point '(20 1)) () "Point should be at column 1 of line 20, got ~A" point))
    (let ((line-20-after-undo (buffer-line buf 20)))
      (format t "Line 20 after undoing B: '~A'~%" line-20-after-undo)
      (assert (string= line-20-after-undo "Line 20") () "Line 20 should be restored, got '~A'" line-20-after-undo))
    
    (buffer-undo buf)  ; Should undo 'A' insertion and restore point
    (let ((point (buffer-get-point buf)))
      (format t "Point after undoing A: ~A~%" point)
      (assert (equal point '(0 0)) () "Point should be at start of line 0, got ~A" point))
    (let ((line-0-after-undo (buffer-line buf 0)))
      (format t "Line 0 after undoing A: '~A'~%" line-0-after-undo)
      (assert (string= line-0-after-undo "Line 0") () "Line 0 should be restored, got '~A'" line-0-after-undo))
    
    ;; Redo operations
    (buffer-redo buf)  ; Should redo 'A' insertion
    (let ((line-0-after-redo-a (buffer-line buf 0)))
      (format t "Line 0 after redoing A: '~A'~%" line-0-after-redo-a)
      (assert (string= line-0-after-redo-a "ALine 0") () "Line 0 should have 'A' again, got '~A'" line-0-after-redo-a))
    (let ((point (buffer-get-point buf)))
      (format t "Point after redoing A: ~A~%" point)
      (assert (equal point '(0 1)) () "Point should be after 'A', got ~A" point))
    
    (buffer-redo buf)  ; Should redo 'B' insertion at line 20
    (let ((line-20-after-redo-b (buffer-line buf 20)))
      (format t "Line 20 after redoing B: '~A'~%" line-20-after-redo-b)
      (assert (string= line-20-after-redo-b "LBine 20") () "Line 20 should have 'B' at column 1 again, got '~A'" line-20-after-redo-b))
    (let ((point (buffer-get-point buf)))
      (format t "Point after redoing B: ~A~%" point)
      (assert (equal point '(20 2)) () "Point should be after 'B' at column 2, got ~A" point))
    
    (format t "✓ Test 2 passed: Redo also only affects content operations~%"))
  
  (format t "All page navigation undo behavior tests passed!~%~%"))

(defun run-all-page-navigation-undo-tests ()
  "Run all page navigation undo behavior tests"
  (format t "~%======================================~%")
  (format t "Running Page Navigation Undo Behavior Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-page-navigation-undo-behavior)
        (format t "~%======================================~%")
        (format t "All page navigation undo tests passed successfully!~%")
        (format t "Note: Page navigation correctly does NOT create undo records~%")
        (format t "======================================~%")
        t)
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")
      nil)))