;;;; test-minibuffer-path.lisp
;;;; Test the exact minibuffer path that web UI uses

(ql:quickload :tle)

(in-package :tle)

(defun test-minibuffer-path ()
  "Test the exact path that web UI uses for minibuffer handling"
  (format t "=== Testing Minibuffer Path ===~%")
  
  (let* ((editor (make-standard-editor))
         (buffer (current-buffer editor)))
    
    ;; Setup buffer with content
    (insert-text-without-undo buffer "hello world hello universe")
    (buffer-set-point buffer 0 0)
    
    (format t "Buffer content: '~A'~%" (buffer-line buffer 0))
    
    ;; Step 1: Simulate Alt+% - start query replace
    (format t "Step 1: Alt+%% pressed...~%")
    (start-query-replace editor)
    
    ;; Check minibuffer state
    (if (minibuffer-active-p editor)
        (format t "✓ Minibuffer activated for search term~%")
        (format t "❌ Minibuffer not activated~%"))
    
    ;; Step 2: Simulate typing "hello" in minibuffer
    (format t "Step 2: Typing 'hello' in minibuffer...~%")
    (let ((minibuf (minibuffer editor)))
      (when minibuf
        ;; Clear minibuffer and add content
        (setf (lines minibuf) (make-array 1 :initial-element "hello"))
        (buffer-set-point minibuf 0 5)))
    
    ;; Step 3: Simulate pressing Enter (this calls get-minibuffer-contents and the callback)
    (format t "Step 3: Pressing Enter to submit search term...~%")
    (handler-case
        ;; This is what happens when Enter is pressed in minibuffer
        (let ((contents (get-minibuffer-contents editor))
              (callback (minibuffer-callback editor)))
          (format t "Minibuffer contents: '~A'~%" contents)
          (format t "Callback: ~A~%" callback)
          (deactivate-minibuffer editor)
          (if callback
              (funcall callback contents editor)
              (error "No callback found")))
      (error (e)
        (format t "❌ ERROR in step 3: ~A~%" e)))
    
    ;; Step 4: Check if we're now asking for replacement
    (if (minibuffer-active-p editor)
        (format t "✓ Now asking for replacement term~%")
        (format t "❌ Should be asking for replacement term~%"))
    
    ;; Step 5: Simulate typing "hi" in minibuffer
    (format t "Step 5: Typing 'hi' in minibuffer...~%")
    (let ((minibuf (minibuffer editor)))
      (when minibuf
        (setf (lines minibuf) (make-array 1 :initial-element "hi"))
        (buffer-set-point minibuf 0 2)))
    
    ;; Step 6: Simulate pressing Enter again (this should trigger the error)
    (format t "Step 6: Pressing Enter to submit replacement term...~%")
    (handler-case
        (let ((contents (get-minibuffer-contents editor))
              (callback (minibuffer-callback editor)))
          (format t "Minibuffer contents: '~A'~%" contents)
          (format t "Callback: ~A~%" callback)
          (deactivate-minibuffer editor)
          (if callback
              (funcall callback contents editor)
              (error "No callback found")))
      (error (e)
        (format t "❌ ERROR in step 6 (this might be the LINE-TEXT error): ~A~%" e)))
    
    ;; Check final state
    (if (query-replace-active-p editor)
        (format t "✓ Query-replace is now active~%")
        (format t "❌ Query-replace should be active~%"))
    
    (format t "Test completed.~%")))

;; Run the test
(test-minibuffer-path)