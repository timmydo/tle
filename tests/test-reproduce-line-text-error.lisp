;;;; test-reproduce-line-text-error.lisp
;;;; Try to reproduce the exact LINE-TEXT error

(ql:quickload :tle)

(in-package :tle)

(defun test-reproduce-line-text-error ()
  "Reproduce the LINE-TEXT unbound variable error"
  (format t "=== Reproducing LINE-TEXT Error ===~%")
  
  (let* ((editor (make-standard-editor))
         (buffer (current-buffer editor)))
    
    ;; Setup buffer with some content
    (insert-text-without-undo buffer "hello world hello universe")
    (buffer-set-point buffer 0 0)
    
    (format t "Buffer setup complete: '~A'~%" (buffer-line buffer 0))
    
    ;; Step 1: Simulate Alt+% (start-query-replace)
    (format t "Step 1: Starting query-replace...~%")
    (start-query-replace editor)
    
    ;; Step 2: Simulate typing "hello" and pressing Enter (query-replace-from-command)
    (format t "Step 2: Entering 'hello' as search term...~%")
    (handler-case
        (query-replace-from-command "hello" editor)
      (error (e)
        (format t "ERROR in step 2: ~A~%" e)
        (return-from test-reproduce-line-text-error nil)))
    
    ;; Step 3: Simulate typing "hi" and pressing Enter (query-replace-to-command)
    (format t "Step 3: Entering 'hi' as replacement term...~%")
    (handler-case
        (query-replace-to-command "hi" editor)
      (error (e)
        (format t "ERROR in step 3 (this is the suspected LINE-TEXT error): ~A~%" e)
        (format t "Error type: ~A~%" (type-of e))
        (return-from test-reproduce-line-text-error nil)))
    
    (format t "Step 4: Checking if query-replace is now active...~%")
    (if (query-replace-active-p editor)
        (format t "✓ Query-replace is active - no error occurred~%")
        (format t "❌ Query-replace is not active - something went wrong~%"))
    
    (format t "Test completed without reproducing the error.~%")))

(defun run-all-reproduce-line-text-error-tests ()
  "Run all line text error reproduction tests"
  (test-reproduce-line-text-error))