;;;; test-query-replace-full-flow.lisp
;;;; Test to replicate the full query-replace flow and the LINE-TEXT error

(asdf:load-system :tle)

(in-package :tle)

(defun test-query-replace-full-flow ()
  "Test the complete query-replace flow to reproduce the LINE-TEXT error"
  (format t "=== Testing Full Query-Replace Flow ===~%")
  
  (let* ((editor (make-standard-editor))
         (buffer (current-buffer editor)))
    
    ;; Setup buffer with test content
    (insert-text-without-undo buffer "hello world hello universe")
    (buffer-set-point buffer 0 0)
    
    (format t "Buffer content: '~A'~%" (buffer-line buffer 0))
    (format t "Starting query-replace simulation...~%~%")
    
    ;; Step 1: Start query-replace (this should work)
    (format t "Step 1: Starting query-replace...~%")
    (start-query-replace editor)
    
    ;; Check if minibuffer is active
    (if (minibuffer-active-p editor)
        (format t "✓ Minibuffer activated for search term~%")
        (format t "❌ Minibuffer not activated~%"))
    
    ;; Step 2: Simulate entering "hello" in minibuffer and pressing Enter
    (format t "~%Step 2: Entering search term 'hello'...~%")
    (let ((minibuf (minibuffer editor)))
      (when minibuf
        (insert-text-without-undo minibuf "hello")
        (format t "Minibuffer content: '~A'~%" (buffer-line minibuf 0))
        
        ;; This should trigger query-replace-from-command
        (format t "Simulating Enter press for search term...~%")
        (let ((callback (minibuffer-callback editor)))
          (when callback
            (format t "Callback function: ~A~%" callback)
            (handler-case
                (funcall callback "hello" editor)
              (error (e)
                (format t "❌ ERROR in query-replace-from-command: ~A~%" e)))))))
    
    ;; Step 3: Check if we're now prompted for replacement string
    (format t "~%Step 3: Checking replacement prompt...~%")
    (if (minibuffer-active-p editor)
        (format t "✓ Minibuffer active for replacement term~%")
        (format t "❌ Minibuffer not active for replacement term~%"))
    
    ;; Step 4: Simulate entering "hi" and pressing Enter (this might cause the error)
    (format t "~%Step 4: Entering replacement term 'hi'...~%")
    (let ((minibuf (minibuffer editor)))
      (when minibuf
        ;; Clear minibuffer first
        (setf (lines minibuf) (list ""))
        (insert-text-without-undo minibuf "hi")
        (format t "Minibuffer content: '~A'~%" (buffer-line minibuf 0))
        
        ;; This should trigger query-replace-to-command
        (format t "Simulating Enter press for replacement term...~%")
        (let ((callback (minibuffer-callback editor)))
          (when callback
            (format t "Callback function: ~A~%" callback)
            (handler-case
                (funcall callback "hi" editor)
              (error (e)
                (format t "❌ ERROR in query-replace-to-command: ~A~%" e)
                (format t "This might be the LINE-TEXT unbound variable error!~%")
                (return-from test-query-replace-full-flow nil)))))))
    
    ;; Step 5: Check if interactive replacement started
    (format t "~%Step 5: Checking if interactive replacement started...~%")
    (if (query-replace-active-p editor)
        (format t "✓ Query-replace active~%")
        (format t "❌ Query-replace not active~%"))
    
    (format t "~%Test completed successfully (no errors encountered)~%")))

;; Test just the problematic functions directly
(defun test-query-replace-callbacks ()
  "Test the query-replace callback functions directly"
  (format t "=== Testing Query-Replace Callbacks ===~%")
  
  (let ((editor (make-standard-editor)))
    (format t "Testing query-replace-from-command...~%")
    (handler-case
        (query-replace-from-command "hello" editor)
      (error (e)
        (format t "❌ ERROR in query-replace-from-command: ~A~%" e)))
    
    (when (minibuffer-active-p editor)
      (format t "Testing query-replace-to-command...~%")
      (handler-case
          (query-replace-to-command "hi" editor)
        (error (e)
          (format t "❌ ERROR in query-replace-to-command: ~A~%" e)
          (format t "This is likely the LINE-TEXT error!~%"))))))

;; Run the tests
(format t "Running query-replace full flow test...~%~%")
(test-query-replace-full-flow)

(format t "~%~%Running callback tests...~%~%") 
(test-query-replace-callbacks)