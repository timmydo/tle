;;;; test-query-replace-enter-bug.lisp
;;;; Test to reproduce the Enter key bug in query-replace

(asdf:load-system :tle)

(use-package :tle)

(defun test-query-replace-enter-action ()
  "Test that Enter key performs replacement instead of quitting"
  (format t "Testing query-replace Enter key behavior...~%")
  
  (let* ((editor (make-standard-editor))
         (buffer (current-buffer editor)))
    
    ;; Setup buffer with test content
    (insert-text buffer "hello world hello universe")
    (buffer-set-point buffer 0 0)
    
    ;; Start interactive query-replace
    (start-interactive-query-replace editor "hello" "hi")
    
    ;; Verify query-replace is active and positioned at first match
    (assert (query-replace-active-p editor) () 
            "Query-replace should be active")
    (assert (equal (buffer-get-point buffer) '(0 0)) ()
            "Should be positioned at first match")
    
    ;; Test that Enter key performs replacement (should act like 'y')
    ;; First simulate what happens when the user types Enter
    (let ((action "Enter"))  ; This simulates the Enter key input
      ;; Convert to character for the action callback
      (let ((action-char (if (string= action "Enter") #\Return #\q)))
        (format t "Testing action character: ~A (from input: ~A)~%" action-char action)
        
        ;; This should perform replacement, not quit
        (query-replace-action-callback (string action-char) editor)))
    
    ;; Check if replacement occurred
    (let ((line-content (buffer-line buffer 0)))
      (format t "Line content after Enter: '~A'~%" line-content)
      
      ;; The bug: Enter quits instead of replacing, so content stays unchanged
      (if (string= line-content "hello world hello universe")
          (format t "❌ BUG CONFIRMED: Enter key quit query-replace instead of replacing~%")
          (format t "✓ Enter key worked correctly, performed replacement~%")))
    
    ;; Check if query-replace is still active (should be if replacement occurred)
    (if (query-replace-active-p editor)
        (format t "✓ Query-replace still active (expected after replacement)~%")
        (format t "❌ Query-replace inactive (indicates it quit instead of replacing)~%")))
  
  (format t "~%"))

(defun test-expected-enter-behavior ()
  "Test what the correct behavior should be for Enter key"
  (format t "Testing expected Enter key behavior (same as 'y')...~%")
  
  (let* ((editor (make-standard-editor))
         (buffer (current-buffer editor)))
    
    ;; Setup buffer with test content
    (insert-text buffer "hello world hello universe")
    (buffer-set-point buffer 0 0)
    
    ;; Start interactive query-replace
    (start-interactive-query-replace editor "hello" "hi")
    
    ;; Test 'y' key (which should work correctly)
    (query-replace-action-callback "y" editor)
    
    ;; Check if replacement occurred
    (let ((line-content (buffer-line buffer 0)))
      (format t "Line content after 'y': '~A'~%" line-content)
      
      (if (string= line-content "hi world hello universe")
          (format t "✓ 'y' key worked correctly, performed first replacement~%")
          (format t "❌ 'y' key failed to perform replacement~%")))
    
    ;; Check if still at next match
    (if (query-replace-active-p editor)
        (format t "✓ Query-replace still active (ready for next match)~%")
        (format t "❌ Query-replace ended unexpectedly~%")))
  
  (format t "~%"))

(defun run-all-tests ()
  "Run all query-replace Enter key tests"
  (format t "=== Query-Replace Enter Key Bug Tests ===~%~%")
  
  (test-query-replace-enter-action)
  (test-expected-enter-behavior)
  
  (format t "=== Test Summary ===~%")
  (format t "The bug is that Enter key in query-replace acts like 'q' (quit) instead of 'y' (replace).~%")
  (format t "This is because in query-replace-action-callback, both #\\q and #\\Return are mapped to quit.~%")
  (format t "The fix is to make Enter act like 'y' or Space (perform replacement).~%~%"))

;; Run tests if executed directly
(when (or (null *load-pathname*) (string= (file-namestring *load-pathname*) "test-query-replace-enter-bug.lisp"))
  (run-all-tests))