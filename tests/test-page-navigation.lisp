(in-package :tle)

(defun test-page-down ()
  "Test page-down method with various scenarios"
  (format t "Running page-down tests...~%")
  
  ;; Test 1: Normal page-down movement
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 50 :fill-pointer 50)))
    ;; Create 50 lines of test content
    (loop for i from 0 to 49
          do (setf (aref lines i) (format nil "Line ~A content" i)))
    (setf (lines buf) lines)
    (buffer-set-point buf 0 5)  ; Start at line 0, column 5
    (page-down buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(20 5)) () "Test 1 failed: expected (20 5), got ~A" point))
    (format t "✓ Test 1 passed: Normal page-down movement preserves column~%"))
  
  ;; Test 2: Page-down near end of buffer
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 30 :fill-pointer 30)))
    ;; Create 30 lines of test content
    (loop for i from 0 to 29
          do (setf (aref lines i) (format nil "Line ~A" i)))
    (setf (lines buf) lines)
    (buffer-set-point buf 15 3)  ; Start at line 15
    (page-down buf)  ; Should move to line 29 (last line), not beyond
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(29 3)) () "Test 2 failed: expected (29 3), got ~A" point))
    (format t "✓ Test 2 passed: Page-down stops at end of buffer~%"))
  
  ;; Test 3: Page-down from last line (should stay in place)
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 10 :fill-pointer 10)))
    (loop for i from 0 to 9
          do (setf (aref lines i) (format nil "Line ~A" i)))
    (setf (lines buf) lines)
    (buffer-set-point buf 9 2)  ; Last line
    (page-down buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(9 2)) () "Test 3 failed: expected (9 2), got ~A" point))
    (format t "✓ Test 3 passed: Page-down from last line stays in place~%"))
  
  ;; Test 4: Column preservation with shorter target line
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 30 :fill-pointer 30)))
    ;; Create lines with varying lengths
    (loop for i from 0 to 29
          do (setf (aref lines i) (if (= i 20) "short" (format nil "This is a much longer line ~A with more content" i))))
    (setf (lines buf) lines)
    (buffer-set-point buf 0 25)  ; Start at column 25 in a long line
    (page-down buf)  ; Should move to line 20 but adjust column to fit "short"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(20 5)) () "Test 4 failed: expected (20 5), got ~A" point))
    (format t "✓ Test 4 passed: Column adjusted for shorter target line~%"))
  
  ;; Test 5: Empty buffer handling
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (page-down buf)  ; Should not crash
    (format t "✓ Test 5 passed: Page-down with empty buffer doesn't crash~%"))
  
  (format t "All page-down tests passed!~%~%"))

(defun test-page-up ()
  "Test page-up method with various scenarios"
  (format t "Running page-up tests...~%")
  
  ;; Test 1: Normal page-up movement
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 50 :fill-pointer 50)))
    ;; Create 50 lines of test content
    (loop for i from 0 to 49
          do (setf (aref lines i) (format nil "Line ~A content" i)))
    (setf (lines buf) lines)
    (buffer-set-point buf 25 7)  ; Start at line 25, column 7
    (page-up buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(5 7)) () "Test 1 failed: expected (5 7), got ~A" point))
    (format t "✓ Test 1 passed: Normal page-up movement preserves column~%"))
  
  ;; Test 2: Page-up near beginning of buffer
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 30 :fill-pointer 30)))
    ;; Create 30 lines of test content
    (loop for i from 0 to 29
          do (setf (aref lines i) (format nil "Line ~A" i)))
    (setf (lines buf) lines)
    (buffer-set-point buf 10 3)  ; Start at line 10
    (page-up buf)  ; Should move to line 0 (first line), not negative
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 3)) () "Test 2 failed: expected (0 3), got ~A" point))
    (format t "✓ Test 2 passed: Page-up stops at beginning of buffer~%"))
  
  ;; Test 3: Page-up from first line (should stay in place)
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 10 :fill-pointer 10)))
    (loop for i from 0 to 9
          do (setf (aref lines i) (format nil "Line ~A" i)))
    (setf (lines buf) lines)
    (buffer-set-point buf 0 4)  ; First line
    (page-up buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "Test 3 failed: expected (0 4), got ~A" point))
    (format t "✓ Test 3 passed: Page-up from first line stays in place~%"))
  
  ;; Test 4: Column preservation with shorter target line
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 30 :fill-pointer 30)))
    ;; Create lines with varying lengths
    (loop for i from 0 to 29
          do (setf (aref lines i) (if (= i 5) "short" (format nil "This is a much longer line ~A with more content" i))))
    (setf (lines buf) lines)
    (buffer-set-point buf 25 30)  ; Start at column 30 in a long line
    (page-up buf)  ; Should move to line 5 but adjust column to fit "short"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(5 5)) () "Test 4 failed: expected (5 5), got ~A" point))
    (format t "✓ Test 4 passed: Column adjusted for shorter target line~%"))
  
  ;; Test 5: Empty buffer handling
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector ))
    (buffer-set-point buf 0 0)
    (page-up buf)  ; Should not crash
    (format t "✓ Test 5 passed: Page-up with empty buffer doesn't crash~%"))
  
  (format t "All page-up tests passed!~%~%"))

(defun test-combined-page-navigation ()
  "Test combinations of page-up and page-down movements"
  (format t "Running combined page navigation tests...~%")
  
  ;; Test 1: Page-down then page-up should return close to original position
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 50 :fill-pointer 50)))
    ;; Create 50 lines of test content
    (loop for i from 0 to 49
          do (setf (aref lines i) (format nil "Line ~A content" i)))
    (setf (lines buf) lines)
    (buffer-set-point buf 25 5)
    (let ((original-point (buffer-get-point buf)))
      (page-down buf)
      (page-up buf)
      (let ((final-point (buffer-get-point buf)))
        ;; Should be back to original position
        (assert (equal original-point final-point) () 
                "Test 1 failed: expected ~A, got ~A" original-point final-point)))
    (format t "✓ Test 1 passed: Page-down then page-up returns to original position~%"))
  
  ;; Test 2: Multiple page movements
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 100 :fill-pointer 100)))
    ;; Create 100 lines of test content
    (loop for i from 0 to 99
          do (setf (aref lines i) (format nil "Line ~A" i)))
    (setf (lines buf) lines)
    (buffer-set-point buf 0 0)  ; Start at beginning
    
    ;; Move down several pages
    (page-down buf)  ; Line 20
    (page-down buf)  ; Line 40
    (page-down buf)  ; Line 60
    (let ((point-after-down (buffer-get-point buf)))
      (assert (equal point-after-down '(60 0)) () "Multiple page-down failed"))
    
    ;; Move up several pages
    (page-up buf)    ; Line 40
    (page-up buf)    ; Line 20
    (page-up buf)    ; Line 0
    (let ((point-after-up (buffer-get-point buf)))
      (assert (equal point-after-up '(0 0)) () "Multiple page-up failed"))
    
    (format t "✓ Test 2 passed: Multiple page movements~%"))
  
  (format t "All combined page navigation tests passed!~%~%"))

(defun test-page-navigation-edge-cases ()
  "Test edge cases for page navigation"
  (format t "Running page navigation edge case tests...~%")
  
  ;; Test 1: Small buffer (less than one page)
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 5 :fill-pointer 5)))
    (loop for i from 0 to 4
          do (setf (aref lines i) (format nil "Line ~A" i)))
    (setf (lines buf) lines)
    
    ;; Page-down from beginning should go to end
    (buffer-set-point buf 0 0)
    (page-down buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(4 0)) () "Small buffer page-down failed"))
    
    ;; Page-up from end should go to beginning
    (page-up buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Small buffer page-up failed"))
    
    (format t "✓ Test 1 passed: Small buffer handling~%"))
  
  ;; Test 2: Single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "Only line"))
    (buffer-set-point buf 0 5)
    
    (page-down buf)  ; Should stay in place
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Single line page-down failed"))
    
    (page-up buf)    ; Should stay in place
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Single line page-up failed"))
    
    (format t "✓ Test 2 passed: Single line buffer~%"))
  
  ;; Test 3: Buffer with exactly one page size (20 lines)
  (let ((buf (make-instance 'standard-buffer))
        (lines (make-array 20 :fill-pointer 20)))
    (loop for i from 0 to 19
          do (setf (aref lines i) (format nil "Line ~A" i)))
    (setf (lines buf) lines)
    
    (buffer-set-point buf 0 3)
    (page-down buf)  ; Should go to last line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(19 3)) () "Exact page size page-down failed"))
    
    (page-up buf)    ; Should go back to first line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 3)) () "Exact page size page-up failed"))
    
    (format t "✓ Test 3 passed: Buffer with exactly one page size~%"))
  
  (format t "All edge case tests passed!~%~%"))

(defun run-all-page-navigation-tests ()
  "Run all page navigation tests"
  (format t "~%======================================~%")
  (format t "Running Page Navigation Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-page-down)
        (test-page-up)
        (test-combined-page-navigation)
        (test-page-navigation-edge-cases)
        (format t "~%======================================~%")
        (format t "All page navigation tests passed successfully!~%")
        (format t "======================================~%")
        t)
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")
      nil)))