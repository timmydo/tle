;;;; test-interactive-query-replace.lisp
;;;; Test suite for interactive query-replace functionality

(asdf:load-system :tle)

(use-package :tle)

(defun test-find-matches-from-point ()
  "Test that find-matches-from-point works correctly."
  (format t "Running find-matches-from-point tests...~%")
  
  ;; Test 1: Basic matches from beginning
  (let ((buffer (make-standard-buffer "test-buffer")))
    (insert-text buffer "hello world hello universe hello")
    (buffer-set-point buffer 0 0)
    
    (let ((matches (find-matches-from-point buffer "hello")))
      (assert (= (length matches) 3))
      (assert (equal (first matches) '(0 0)))
      (assert (equal (second matches) '(0 12)))
      (assert (equal (third matches) '(0 24)))
      (format t "✓ Test 1 passed: Basic matches from beginning~%")))
  
  ;; Test 2: Matches from middle position  
  (let ((buffer (make-standard-buffer "test-buffer")))
    (insert-text buffer "hello world hello universe hello")
    (buffer-set-point buffer 0 6) ; Start after first "hello"
    
    (let ((matches (find-matches-from-point buffer "hello")))
      (assert (= (length matches) 2))
      (assert (equal (first matches) '(0 12)))
      (assert (equal (second matches) '(0 24)))
      (format t "✓ Test 2 passed: Matches from middle position~%")))
  
  ;; Test 3: Multi-line matches
  (let ((buffer (make-standard-buffer "test-buffer")))
    (insert-text buffer "hello world")
    (insert-text buffer (format nil "~%"))
    (insert-text buffer "hello universe")
    (insert-text buffer (format nil "~%"))  
    (insert-text buffer "hello again")
    (buffer-set-point buffer 0 6) ; Start on first line
    
    (let ((matches (find-matches-from-point buffer "hello")))
      (assert (= (length matches) 2))
      (assert (equal (first matches) '(1 0)))
      (assert (equal (second matches) '(2 0)))
      (format t "✓ Test 3 passed: Multi-line matches~%")))
  
  (format t "All find-matches-from-point tests passed!~%~%"))

(defun test-interactive-query-replace-state ()
  "Test query-replace state management."
  (format t "Running interactive query-replace state tests...~%")
  
  (let ((editor (make-standard-editor)))
    ;; Initialize state
    (setf (query-replace-active-p editor) t)
    (setf (query-replace-from-string editor) "test")
    (setf (query-replace-to-string editor) "replaced")
    (setf (query-replace-matches editor) '((0 5) (1 10)))
    (setf (query-replace-current-match editor) 0)
    (setf (query-replace-replacements editor) '())
    
    ;; Test state is set correctly
    (assert (query-replace-active-p editor))
    (assert (string= (query-replace-from-string editor) "test"))
    (assert (string= (query-replace-to-string editor) "replaced"))
    (assert (= (length (query-replace-matches editor)) 2))
    (assert (= (query-replace-current-match editor) 0))
    (format t "✓ Test 1 passed: State initialization~%")
    
    ;; Test clear state
    (clear-query-replace-state editor)
    (assert (not (query-replace-active-p editor)))
    (assert (null (query-replace-to-string editor)))
    (assert (null (query-replace-matches editor)))
    (assert (= (query-replace-current-match editor) 0))
    (format t "✓ Test 2 passed: State clearing~%"))
  
  (format t "All interactive query-replace state tests passed!~%~%"))

(defun test-replacement-functionality ()
  "Test that replacement functionality works correctly."
  (format t "Running replacement functionality tests...~%")
  
  (let ((editor (make-standard-editor))
        (buffer (make-standard-buffer "test-buffer")))
    ;; Set up buffer with test content
    (insert-text buffer "hello world hello universe")
    (setf (buffers editor) (list buffer))
    
    ;; Set up query-replace state
    (setf (query-replace-active-p editor) t)
    (setf (query-replace-from-string editor) "hello")
    (setf (query-replace-to-string editor) "hi")
    (setf (query-replace-matches editor) '((0 0) (0 12)))
    (setf (query-replace-current-match editor) 0)
    (setf (query-replace-replacements editor) '())
    
    ;; Perform first replacement
    (perform-current-replacement editor)
    (let ((line-content (buffer-line buffer 0)))
      (assert (string= line-content "hi world hello universe"))
      (assert (= (length (query-replace-replacements editor)) 1))
      (format t "✓ Test 1 passed: First replacement~%"))
    
    ;; Test undo
    (undo-last-replacement-without-prompt editor)
    (let ((line-content (buffer-line buffer 0)))
      (assert (string= line-content "hello world hello universe"))
      (assert (= (length (query-replace-replacements editor)) 0))
      (format t "✓ Test 2 passed: Undo replacement~%")))
  
  (format t "All replacement functionality tests passed!~%~%"))

(defun run-all-tests ()
  "Run all interactive query-replace tests."
  (format t "~%======================================~%")
  (format t "Running Interactive Query-Replace Tests~%")
  (format t "======================================~%~%")
  
  (test-find-matches-from-point)
  (test-interactive-query-replace-state)
  (test-replacement-functionality)
  
  (format t "~%======================================~%")
  (format t "All interactive query-replace tests passed successfully!~%")
  (format t "======================================~%"))

;; Run tests
(run-all-tests)