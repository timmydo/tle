(in-package :tle)

(defun test-live-isearch-basic ()
  "Test basic live incremental search functionality"
  (format t "Running live isearch basic tests...~%")
  
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup test buffer
    (setf (lines buffer) #("hello world test" "another world line"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Start isearch
    (start-isearch-forward editor)
    (assert (minibuffer-active-p editor) () "Minibuffer should be active")
    (assert (isearch-original-position editor) () "Original position should be stored")
    (assert (equal (isearch-original-position editor) '(0 0)) () "Original position should be (0 0)")
    
    ;; Type 'w' - should find 'world' at position (0 6)
    (handle-minibuffer-input editor "w" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 6)) () "Should find 'w' in 'world' at (0 6)")
    (assert (string= (get-minibuffer-contents editor) "w") () "Minibuffer should contain 'w'")
    
    ;; Type 'o' - should still be at 'world' 
    (handle-minibuffer-input editor "o" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 6)) () "Should find 'wo' in 'world' at (0 6)")
    (assert (string= (get-minibuffer-contents editor) "wo") () "Minibuffer should contain 'wo'")
    
    ;; Complete the search
    (handle-minibuffer-input editor "Enter" nil nil nil nil)
    (assert (not (minibuffer-active-p editor)) () "Minibuffer should be deactivated")
    (assert (equal (buffer-get-point buffer) '(0 6)) () "Cursor should stay at found position")
    (assert (not (isearch-original-position editor)) () "Original position should be cleared")
    
    (format t "✓ Test 1 passed: Basic live isearch functionality~%"))
  
  (format t "All live isearch basic tests passed!~%~%"))

(defun test-live-isearch-cancel ()
  "Test canceling incremental search restores position"
  (format t "Running live isearch cancel tests...~%")
  
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup test buffer
    (setf (lines buffer) #("hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Start isearch and move cursor
    (start-isearch-forward editor)
    (handle-minibuffer-input editor "w" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 6)) () "Should move to 'world'")
    
    ;; Cancel with escape
    (handle-minibuffer-input editor "Escape" nil nil nil nil)
    (assert (not (minibuffer-active-p editor)) () "Minibuffer should be deactivated")
    (assert (equal (buffer-get-point buffer) '(0 0)) () "Should restore original position")
    (assert (not (isearch-original-position editor)) () "Original position should be cleared")
    
    (format t "✓ Test 1 passed: Cancel restores position~%"))
  
  (format t "All live isearch cancel tests passed!~%~%"))

(defun test-live-isearch-not-found ()
  "Test behavior when search string is not found"
  (format t "Running live isearch not-found tests...~%")
  
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup test buffer
    (setf (lines buffer) #("hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Start isearch
    (start-isearch-forward editor)
    
    ;; Type something that exists
    (handle-minibuffer-input editor "w" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 6)) () "Should find 'w'")
    
    ;; Type something that makes it not found
    (handle-minibuffer-input editor "x" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 0)) () "Should return to original position when not found")
    (assert (string= (get-minibuffer-contents editor) "wx") () "Minibuffer should contain 'wx'")
    
    ;; Backspace to make it found again
    (handle-minibuffer-input editor "Backspace" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 6)) () "Should find 'w' again after backspace")
    (assert (string= (get-minibuffer-contents editor) "w") () "Minibuffer should contain 'w'")
    
    ;; Cancel
    (handle-minibuffer-input editor "Escape" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 0)) () "Should restore original position")
    
    (format t "✓ Test 1 passed: Not found behavior~%"))
  
  (format t "All live isearch not-found tests passed!~%~%"))

(defun test-live-isearch-multiline ()
  "Test incremental search across multiple lines"
  (format t "Running live isearch multiline tests...~%")
  
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup test buffer
    (setf (lines buffer) #("first line" "second target line" "third line"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Start isearch
    (start-isearch-forward editor)
    
    ;; Search for 'target' which is on line 2
    (handle-minibuffer-input editor "t" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 4)) () "Should find first 't' in 'first' at (0 4)")
    
    (handle-minibuffer-input editor "a" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(1 7)) () "Should find 'ta' in 'target' at (1 7)")
    
    (handle-minibuffer-input editor "r" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(1 7)) () "Should find 'tar' in 'target' at (1 7)")
    
    ;; Complete the search
    (handle-minibuffer-input editor "Enter" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(1 7)) () "Should stay at target position")
    
    (format t "✓ Test 1 passed: Multiline search~%"))
  
  (format t "All live isearch multiline tests passed!~%~%"))

(defun test-live-isearch-empty-string ()
  "Test behavior with empty search string"
  (format t "Running live isearch empty string tests...~%")
  
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup test buffer
    (setf (lines buffer) #("hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 5)  ; Start from middle
    
    ;; Start isearch
    (start-isearch-forward editor)
    (assert (equal (isearch-original-position editor) '(0 5)) () "Should store original position (0 5)")
    
    ;; Type and then delete to empty string
    (handle-minibuffer-input editor "w" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 6)) () "Should find 'w'")
    
    (handle-minibuffer-input editor "Backspace" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 5)) () "Should restore to original position with empty search")
    (assert (string= (get-minibuffer-contents editor) "") () "Minibuffer should be empty")
    
    ;; Cancel
    (handle-minibuffer-input editor "Escape" nil nil nil nil)
    (assert (equal (buffer-get-point buffer) '(0 5)) () "Should stay at original position")
    
    (format t "✓ Test 1 passed: Empty string behavior~%"))
  
  (format t "All live isearch empty string tests passed!~%~%"))

(defun run-all-live-isearch-tests ()
  "Run all live incremental search tests"
  (format t "~%======================================~%")
  (format t "Running Live Incremental Search Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-live-isearch-basic)
        (test-live-isearch-cancel)
        (test-live-isearch-not-found)
        (test-live-isearch-multiline)
        (test-live-isearch-empty-string)
        (format t "~%======================================~%")
        (format t "All live incremental search tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: live isearch test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)