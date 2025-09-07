(asdf:load-system :tle)

(defun test-modeline-info ()
  "Test that modeline info is correctly generated."
  (let ((editor (tle::make-standard-editor)))
    (let* ((buffer (tle::current-buffer editor))
           (modeline-info (tle::get-modeline-info editor)))
      ;; Test initial state
      (assert (string= (getf modeline-info :filename) "*scratch*"))
      (assert (= (getf modeline-info :line) 3))
      (assert (= (getf modeline-info :column) 6))
      
      ;; Move cursor and test again
      (tle::forward-char buffer)
      (let ((modeline-info-2 (tle::get-modeline-info editor)))
        (assert (= (getf modeline-info-2 :line) 3))
        (assert (= (getf modeline-info-2 :column) 7)))
      
      ;; Move to next line and test
      (tle::next-line buffer)
      (let ((modeline-info-3 (tle::get-modeline-info editor)))
        (assert (= (getf modeline-info-3 :line) 3))))))

(defun test-minibuffer-activation ()
  "Test minibuffer activation and deactivation."
  (let ((editor (tle::make-standard-editor)))
    ;; Initially minibuffer should not be active
    (assert (not (tle::minibuffer-active-p editor)))
    (assert (null (tle::minibuffer editor)))
    
    ;; Activate minibuffer
    (tle::activate-minibuffer editor "Test prompt: ")
    (assert (tle::minibuffer-active-p editor))
    (assert (not (null (tle::minibuffer editor))))
    (assert (string= (tle::minibuffer-prompt editor) "Test prompt: "))
    
    ;; Deactivate minibuffer
    (let ((contents (tle::deactivate-minibuffer editor)))
      (assert (not (tle::minibuffer-active-p editor)))
      (assert (null (tle::minibuffer editor)))
      (assert (string= (tle::minibuffer-prompt editor) ""))
      (assert (stringp contents)))))

(defun test-command-registration ()
  "Test command registration and execution."
  (let ((test-executed nil))
    ;; Register a test command
    (tle::register-command "test-command" 
                          (lambda (editor) 
                            (declare (ignore editor))
                            (setf test-executed t)))
    
    ;; Test that command is registered
    (assert (not (null (gethash "test-command" tle::*command-table*))))
    
    ;; Execute the command
    (let ((editor (tle::make-standard-editor)))
      (tle::execute-named-command "test-command" editor)
      (assert test-executed))))

(defun test-minibuffer-input-handling ()
  "Test minibuffer input handling."
  (let ((editor (tle::make-standard-editor)))
    ;; Activate minibuffer
    (tle::activate-minibuffer editor "Command: ")
    
    ;; Test character insertion
    (assert (tle::handle-minibuffer-input editor "t" nil nil nil nil))
    (assert (tle::handle-minibuffer-input editor "e" nil nil nil nil))
    (assert (tle::handle-minibuffer-input editor "s" nil nil nil nil))
    (assert (tle::handle-minibuffer-input editor "t" nil nil nil nil))
    
    ;; Check contents
    (let ((contents (tle::get-minibuffer-contents editor)))
      (assert (string= contents "test")))
    
    ;; Test backspace
    (assert (tle::handle-minibuffer-input editor "Backspace" nil nil nil nil))
    (let ((contents-after-backspace (tle::get-minibuffer-contents editor)))
      (assert (string= contents-after-backspace "tes")))
    
    ;; Test escape (should deactivate minibuffer)
    (assert (tle::handle-minibuffer-input editor "Escape" nil nil nil nil))
    (assert (not (tle::minibuffer-active-p editor)))))

(defun test-execute-command ()
  "Test the execute-command function."
  (let ((editor (tle::make-standard-editor)))
    ;; Execute command should activate minibuffer
    (tle::execute-command editor)
    (assert (tle::minibuffer-active-p editor))
    (assert (string= (tle::minibuffer-prompt editor) "M-x "))))

(defun test-minibuffer-clears-on-mx ()
  "Test that M-x clears existing minibuffer text."
  (let ((editor (tle::make-standard-editor)))
    ;; Start fresh - ensure no minibuffer is active
    (assert (not (tle::minibuffer-active-p editor)))
    
    ;; First activate minibuffer with some content
    (tle::activate-minibuffer editor "Command: ")
    (assert (tle::minibuffer-active-p editor))
    (assert (string= (tle::get-minibuffer-contents editor) "")) ; Should start empty
    
    ;; Add some content
    (tle::handle-minibuffer-input editor "o" nil nil nil nil)
    (tle::handle-minibuffer-input editor "l" nil nil nil nil)
    (tle::handle-minibuffer-input editor "d" nil nil nil nil)
    
    ;; Verify we have the expected content only
    (let ((contents-before (tle::get-minibuffer-contents editor)))
      (assert (string= contents-before "old")))
    
    ;; Now press M-x (execute-command) which should clear the text
    (tle::execute-command editor)
    
    ;; Check that minibuffer is now active with M-x prompt and no content
    (assert (tle::minibuffer-active-p editor))
    (assert (string= (tle::minibuffer-prompt editor) "M-x "))
    (let ((contents-after (tle::get-minibuffer-contents editor)))
      (assert (string= contents-after "")))))

(defun test-modeline-rendering ()
  "Test modeline HTML rendering."
  (let ((modeline-info (list :filename "test.lisp" :line 42 :column 10)))
    (let ((html (tle::render-modeline modeline-info)))
      (assert (search "test.lisp" html))
      (assert (search "Line:42" html))
      (assert (search "Col:10" html))
      (assert (search "modeline" html)))))

(defun test-minibuffer-rendering ()
  "Test minibuffer HTML rendering."
  (let ((editor (tle::make-standard-editor)))
    ;; Test inactive minibuffer (should still render but be empty)
    (let ((html-inactive (tle::render-minibuffer editor)))
      (assert (search "minibuffer" html-inactive)) ; Always has minibuffer div
      (assert (not (search "M-x" html-inactive)))) ; But no prompt when inactive
    
    ;; Test active minibuffer
    (tle::activate-minibuffer editor "M-x ")
    (let ((html-active (tle::render-minibuffer editor)))
      (assert (search "M-x" html-active))
      (assert (search "minibuffer" html-active)))))

(defun run-all-tests ()
  "Run all modeline and minibuffer tests."
  (format t "Running modeline and minibuffer tests...~%")
  
  (test-modeline-info)
  (format t "✓ Modeline info test passed~%")
  
  (test-minibuffer-activation)
  (format t "✓ Minibuffer activation test passed~%")
  
  (test-command-registration)
  (format t "✓ Command registration test passed~%")
  
  (test-minibuffer-input-handling)
  (format t "✓ Minibuffer input handling test passed~%")
  
  (test-execute-command)
  (format t "✓ Execute command test passed~%")
  
  (test-minibuffer-clears-on-mx)
  (format t "✓ Minibuffer clears on M-x test passed~%")
  
  (test-modeline-rendering)
  (format t "✓ Modeline rendering test passed~%")
  
  (test-minibuffer-rendering)
  (format t "✓ Minibuffer rendering test passed~%")
  
  (format t "All tests passed!~%"))

;; Run the tests
(run-all-tests)