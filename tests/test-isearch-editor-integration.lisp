;; Test isearch editor integration
(ql:quickload :tle)

(defpackage #:test-isearch-editor-integration
  (:use #:cl)
  (:export #:run-integration-tests))

(in-package #:test-isearch-editor-integration)

(defun test-live-isearch-forward ()
  "Test the live isearch forward functionality."
  (format t "Testing live isearch forward...~%")
  (let ((editor (tle::make-standard-editor)))
    (let ((buffer (tle::current-buffer editor)))
      ;; Set up buffer with test content
      (setf (tle::lines buffer) (vector "hello world" "test hello world test" "another line with hello"))
      (tle::buffer-set-point buffer 0 0)
      
      ;; Store original position
      (setf (tle::isearch-original-position editor) (copy-list (tle::buffer-get-point buffer)))
      
      ;; Test live isearch with "hello"
      (tle::live-isearch-forward "hello" editor)
      
      ;; Check that isearch state is set
      (assert (tle::buffer-isearch-active-p buffer))
      (assert (string= "hello" (tle::buffer-isearch-term buffer)))
      
      ;; Check that cursor moved to first occurrence
      (let ((point (tle::buffer-get-point buffer)))
        (assert (= 0 (first point)))  ; Line 0
        (assert (= 0 (second point))))  ; Column 0 (start of "hello")
      
      (format t "  ✓ Live isearch sets state and moves cursor~%")
      
      ;; Test with empty string (should restore original position)
      (tle::live-isearch-forward "" editor)
      
      ;; Check that isearch state is cleared
      (assert (not (tle::buffer-isearch-active-p buffer)))
      (assert (not (tle::buffer-isearch-term buffer)))
      
      ;; Check that cursor is back to original position
      (let ((point (tle::buffer-get-point buffer)))
        (assert (= 0 (first point)))
        (assert (= 0 (second point))))
      
      (format t "  ✓ Empty search restores position and clears state~%")))
  
  (format t "✓ Live isearch forward test passed~%"))

(defun test-start-isearch-forward ()
  "Test the start isearch forward functionality."
  (format t "Testing start isearch forward...~%")
  (let ((editor (tle::make-standard-editor)))
    (let ((buffer (tle::current-buffer editor)))
      ;; Set up buffer with test content
      (setf (tle::lines buffer) (vector "hello world" "test hello world test"))
      (tle::buffer-set-point buffer 1 5)  ; Position at line 1, column 5
      
      ;; Start isearch
      (tle::start-isearch-forward editor)
      
      ;; Check that original position is stored
      (let ((orig-pos (tle::isearch-original-position editor)))
        (assert (and orig-pos 
                     (= 1 (first orig-pos))
                     (= 5 (second orig-pos))))
        (format t "  ✓ Original position stored correctly~%"))
      
      ;; Check that minibuffer is activated
      (assert (tle::minibuffer-active-p editor))
      (assert (string= "I-search: " (tle::minibuffer-prompt editor)))
      (format t "  ✓ Minibuffer activated with correct prompt~%")
      
      ;; Test final callback (simulating pressing Enter)
      (let ((callback (tle::minibuffer-callback editor)))
        (when callback
          (funcall callback "test" editor)))
      
      ;; Check that isearch state is cleared and original position reset
      (assert (not (tle::buffer-isearch-active-p buffer)))
      (assert (not (tle::isearch-original-position editor)))
      (format t "  ✓ Final callback clears state correctly~%")))
  
  (format t "✓ Start isearch forward test passed~%"))

(defun run-integration-tests ()
  "Run all isearch editor integration tests."
  (format t "~%======================================~%")
  (format t "Running Isearch Editor Integration Tests~%")
  (format t "======================================~%~%")
  
  (test-live-isearch-forward)
  (test-start-isearch-forward)
  
  (format t "~%======================================~%")
  (format t "All isearch editor integration tests passed successfully!~%")
  (format t "======================================~%~%"))

;; Run tests when file is loaded
(run-integration-tests)