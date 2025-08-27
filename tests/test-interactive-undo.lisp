(in-package :tle)

(defun test-interactive-query-replace-undo ()
  "Test that interactive query-replace creates proper undo record"
  (format t "Testing interactive query-replace undo...~%")
  
  (let ((editor (make-standard-editor))
        (buf (make-standard-buffer "*test*")))
    ;; Setup
    (setf (lines buf) #("hello world hello"))
    (buffer-set-point buf 0 0)
    (setf (buffers editor) (list buf))
    
    ;; Store original state
    (let ((original-content (copy-seq (lines buf)))
          (original-point (buffer-get-point buf)))
      
      ;; Start interactive query-replace
      (start-interactive-query-replace editor "hello" "hi")
      
      ;; Perform all replacements
      (let ((matches (query-replace-matches editor)))
        (loop for i from 0 below (length matches) do
              (perform-current-replacement editor)
              (setf (query-replace-current-match editor) (1+ i))))
      
      ;; Finish session
      (finish-query-replace editor)
      
      ;; Verify replacements were made
      (assert (string= (buffer-line buf 0) "hi world hi"))
      (format t "Replacements made: ~A~%" (coerce (lines buf) 'list))
      
      ;; Test undo
      (let ((undo-result (buffer-undo buf)))
        (format t "Undo result: ~A~%" undo-result)
        (format t "After undo: ~A~%" (coerce (lines buf) 'list))
        
        ;; Verify undo worked
        (assert undo-result)
        (assert (string= (buffer-line buf 0) "hello world hello"))
        (assert (equal (buffer-get-point buf) original-point))
        
        (format t "✓ Interactive query-replace undo works correctly~%")))))

(defun run-interactive-undo-test ()
  "Run the interactive undo test"
  (handler-case
      (test-interactive-query-replace-undo)
    (error (e)
      (format t "ERROR: ~A~%" e)
      (return-from run-interactive-undo-test nil)))
  t)