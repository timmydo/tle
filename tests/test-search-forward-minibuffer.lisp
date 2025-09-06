(in-package :tle)

(defun test-search-forward-minibuffer-integration ()
  "Test that search-forward works correctly through minibuffer interface"
  (format t "Running search-forward minibuffer integration tests...~%")
  
  ;; Test 1: Basic minibuffer search functionality
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer with test content
    (setf (lines buffer) (vector "hello world test" "second line"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Test that activating search creates proper minibuffer state
    (activate-minibuffer editor "Search: " nil 'search-forward-command)
    
    ;; Verify minibuffer is active
    (assert (minibuffer-active-p editor) () "Minibuffer should be active after activation")
    (assert (string= (minibuffer-prompt editor) "Search: ") () 
            "Minibuffer prompt should be 'Search: '")
    (assert (eq (minibuffer-callback editor) 'search-forward-command) ()
            "Minibuffer callback should be search-forward-command")
    (format t "✓ Test 1a passed: Minibuffer activation works correctly~%")
    
    ;; Simulate typing "world" in minibuffer
    (let ((minibuf (minibuffer editor)))
      (insert-char minibuf #\w)
      (insert-char minibuf #\o)
      (insert-char minibuf #\r)
      (insert-char minibuf #\l)
      (insert-char minibuf #\d))
    
    ;; Verify minibuffer contains the typed text
    (let ((contents (get-minibuffer-contents editor)))
      (assert (string= contents "world") () 
              "Minibuffer should contain 'world', got ~A" contents))
    (format t "✓ Test 1b passed: Text input into minibuffer works~%")
    
    ;; Store initial position
    (let ((initial-point (buffer-get-point buffer)))
      (assert (equal initial-point '(0 0)) () "Initial position should be (0 0)")
      
      ;; Simulate pressing Enter (this should execute the callback)
      (handle-minibuffer-input editor "Enter" nil nil nil nil)
      
      ;; Verify minibuffer is deactivated
      (assert (not (minibuffer-active-p editor)) () 
              "Minibuffer should be deactivated after Enter")
      
      ;; Verify cursor moved to the found position
      (let ((final-point (buffer-get-point buffer)))
        (assert (equal final-point '(0 6)) () 
                "Cursor should move to position (0 6) where 'world' starts, got ~A" final-point))
      (format t "✓ Test 1c passed: Enter executes search and moves cursor~%")))
  
  ;; Test 2: Search not found case
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer with test content
    (setf (lines buffer) (vector "hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Activate search and type non-existent text
    (activate-minibuffer editor "Search: " nil 'search-forward-command)
    (let ((minibuf (minibuffer editor)))
      ;; Type "missing"
      (dolist (char '(#\m #\i #\s #\s #\i #\n #\g))
        (insert-char minibuf char)))
    
    (let ((initial-point (buffer-get-point buffer)))
      ;; Execute search
      (handle-minibuffer-input editor "Enter" nil nil nil nil)
      
      ;; Verify cursor didn't move (search failed)
      (let ((final-point (buffer-get-point buffer)))
        (assert (equal initial-point final-point) () 
                "Cursor should not move when search fails: initial ~A, final ~A"
                initial-point final-point))
      (format t "✓ Test 2 passed: Failed search doesn't move cursor~%")))
  
  ;; Test 3: Escape cancellation
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer
    (setf (lines buffer) (vector "hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Activate search and type some text
    (activate-minibuffer editor "Search: " nil 'search-forward-command)
    (let ((minibuf (minibuffer editor)))
      (insert-char minibuf #\w)
      (insert-char minibuf #\o)
      (insert-char minibuf #\r))
    
    (let ((initial-point (buffer-get-point buffer)))
      ;; Press Escape to cancel
      (handle-minibuffer-input editor "Escape" nil nil nil nil)
      
      ;; Verify minibuffer is deactivated
      (assert (not (minibuffer-active-p editor)) () 
              "Minibuffer should be deactivated after Escape")
      
      ;; Verify cursor didn't move (search was cancelled)
      (let ((final-point (buffer-get-point buffer)))
        (assert (equal initial-point final-point) () 
                "Cursor should not move when search is cancelled"))
      (format t "✓ Test 3 passed: Escape cancels search without moving cursor~%")))
  
  ;; Test 4: Test undo after minibuffer search
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer
    (setf (lines buffer) (vector "hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    (let ((initial-point (buffer-get-point buffer)))
      ;; Perform search through minibuffer
      (activate-minibuffer editor "Search: " nil 'search-forward-command)
      (let ((minibuf (minibuffer editor)))
        (dolist (char '(#\t #\e #\s #\t))
          (insert-char minibuf char)))
      
      ;; Execute search
      (handle-minibuffer-input editor "Enter" nil nil nil nil)
      
      ;; Verify search worked
      (let ((search-point (buffer-get-point buffer)))
        (assert (equal search-point '(0 12)) () 
                "Search should find 'test' at position (0 12)"))
      
      ;; Test undo
      (buffer-undo buffer)
      (let ((undo-point (buffer-get-point buffer)))
        (assert (equal undo-point initial-point) ()
                "Undo should restore original position ~A, got ~A" 
                initial-point undo-point))
      (format t "✓ Test 4 passed: Undo works after minibuffer search~%")))
  
  (format t "All search-forward minibuffer integration tests passed!~%~%"))

(defun test-search-forward-command-function ()
  "Test the search-forward-command function directly"
  (format t "Running search-forward-command function tests...~%")
  
  ;; Test 1: Basic command execution
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer
    (setf (lines buffer) (vector "hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Execute command directly
    (search-forward-command "world" editor)
    
    ;; Verify cursor moved
    (let ((point (buffer-get-point buffer)))
      (assert (equal point '(0 6)) ()
              "search-forward-command should move cursor to (0 6), got ~A" point))
    (format t "✓ Test 1 passed: search-forward-command moves cursor correctly~%"))
  
  ;; Test 2: Command with whitespace trimming
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer
    (setf (lines buffer) (vector "hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Execute command with extra whitespace
    (search-forward-command "  world  " editor)
    
    ;; Verify cursor moved (whitespace should be trimmed)
    (let ((point (buffer-get-point buffer)))
      (assert (equal point '(0 6)) ()
              "search-forward-command should trim whitespace and find 'world'"))
    (format t "✓ Test 2 passed: search-forward-command trims whitespace~%"))
  
  ;; Test 3: Command with no current buffer
  (let ((editor (make-instance 'standard-editor)))
    ;; Don't set current buffer (empty buffer list)
    (setf (buffers editor) nil)
    
    ;; This should not crash
    (search-forward-command "test" editor)
    (format t "✓ Test 3 passed: search-forward-command handles nil buffer gracefully~%"))
  
  (format t "All search-forward-command function tests passed!~%~%"))

(defun run-all-search-forward-minibuffer-tests ()
  "Run all search-forward minibuffer integration tests"
  (format t "~%======================================~%")
  (format t "Running Search-Forward Minibuffer Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-search-forward-minibuffer-integration)
        (test-search-forward-command-function)
        (format t "~%======================================~%")
        (format t "All search-forward minibuffer tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: search-forward minibuffer test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)