(in-package :tle)

(defclass repl-mode (mode)
  ()
  (:documentation "REPL mode with special handling for Enter key evaluation."))

(defmethod handle-key ((mode repl-mode) editor key ctrl alt shift meta)
  "Handle key input for REPL mode with special Enter key behavior."
  (let ((buffer (current-buffer editor)))
    (when buffer
      (cond
        ;; Enter for REPL evaluation (without shift)
        ((and (string= key "Enter") (not shift))
         (when (and (typep editor 'repl-editor) 
                    (repl-editor-rich-object-view editor))
           (evaluate-repl-buffer editor (repl-editor-rich-object-view editor)))
         (format t "Enter: Evaluated REPL buffer~%")
         t)
        
        ;; Shift+Enter for newline insertion in REPL editors
        ((and shift (string= key "Enter"))
         (insert-newline buffer)
         (format t "Shift+Enter: Inserted newline in REPL~%")
         t)
        
        ;; Return NIL if key was not handled (will be handled by other modes)
        (t nil)))))

(defun make-repl-mode ()
  "Create a new repl mode instance."
  (make-instance 'repl-mode))