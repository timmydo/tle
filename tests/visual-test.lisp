(ql:quickload :tle)

(defun test-visual-output ()
  "Test visual output of modeline and minibuffer HTML."
  (let ((editor (tle::make-standard-editor))
        (ui (tle::make-web-ui)))
    (format t "==== EDITOR HTML WITH ALWAYS-VISIBLE MINIBUFFER ====~%")
    (format t "~A~%~%" (tle::render editor ui))
    
    (format t "==== MODELINE ONLY ====~%")
    (format t "~A~%~%" (tle::render-modeline (tle::get-modeline-info editor)))
    
    (format t "==== MINIBUFFER (INACTIVE) ====~%")
    (format t "~A~%~%" (tle::render-minibuffer editor))
    
    (format t "==== MINIBUFFER (ACTIVE WITH M-x) ====~%")
    (tle::activate-minibuffer editor "M-x ")
    (format t "~A~%~%" (tle::render-minibuffer editor))
    
    ;; Add some content to minibuffer
    (tle::handle-minibuffer-input editor "h" nil nil nil nil)
    (tle::handle-minibuffer-input editor "e" nil nil nil nil)
    (tle::handle-minibuffer-input editor "l" nil nil nil nil)
    (tle::handle-minibuffer-input editor "l" nil nil nil nil)
    (tle::handle-minibuffer-input editor "o" nil nil nil nil)
    
    (format t "==== MINIBUFFER (ACTIVE WITH CONTENT) ====~%")
    (format t "~A~%~%" (tle::render-minibuffer editor))
    
    (format t "==== COMPLETE EDITOR WITH ACTIVE MINIBUFFER ====~%")
    (format t "~A~%~%" (tle::render editor ui))))

(test-visual-output)