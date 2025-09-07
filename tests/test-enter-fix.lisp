;;;; test-enter-fix.lisp
;;;; Simple test to verify Enter key fix works

(asdf:load-system :tle)

(in-package :tle)

(defun test-query-replace-enter-fix ()
  "Test that Enter key performs replacement instead of quitting"
  (format t "Testing query-replace Enter key fix...~%")
  
  ;; Test the action callback directly
  (let* ((editor (make-standard-editor))
         (buffer (current-buffer editor)))
    
    ;; Setup test buffer
    (insert-text-without-undo buffer "hello world hello universe")
    (buffer-set-point buffer 0 0)
    
    ;; Initialize query-replace state manually
    (setf (query-replace-active-p editor) t)
    (setf (query-replace-from-string editor) "hello")
    (setf (query-replace-to-string editor) "hi")
    (setf (query-replace-matches editor) '((0 0) (0 12)))
    (setf (query-replace-current-match editor) 0)
    (setf (query-replace-replacements editor) '())
    
    ;; Set up the buffer correctly
    (setf (buffers editor) (list buffer))
    
    ;; Test Enter key action (simulating Return character)
    (format t "Before replacement: '~A'~%" (buffer-line buffer 0))
    
    ;; Simulate Enter key press (creates Return character)
    (query-replace-action-callback (string #\Return) editor)
    
    ;; Check result
    (let ((line-content (buffer-line buffer 0)))
      (format t "After Enter: '~A'~%" line-content)
      
      (if (string= line-content "hi world hello universe")
          (format t "✅ SUCCESS: Enter key performed replacement correctly!~%")
          (format t "❌ FAILED: Enter key did not perform replacement. Content: '~A'~%" line-content)))
    
    ;; Check if still active (should be, ready for next match)
    (if (query-replace-active-p editor)
        (format t "✅ Query-replace still active (correct)~%")
        (format t "❌ Query-replace inactive (incorrect - should continue to next match)~%")))
  
  (format t "~%Test completed.~%"))

;; Run test
(test-query-replace-enter-fix)