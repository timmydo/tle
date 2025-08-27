(in-package :tle)

(defun test-search-forward-basic ()
  "Test basic search-forward functionality"
  (format t "Running basic search-forward tests...~%")
  
  ;; Test 1: Find text in same line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 0)  ; Beginning
    (let ((result (search-forward buf "world")))
      (assert result () "Test 1a failed: should find 'world'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 6)) () "Test 1a failed: expected (0 6), got ~A" point)))
    (format t "✓ Test 1a passed: Find text in same line~%"))
  
  ;; Test 2: Find from current position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world hello"))
    (buffer-set-point buf 0 7)  ; After first "hello"
    (let ((result (search-forward buf "hello")))
      (assert result () "Test 2 failed: should find second 'hello'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 12)) () "Test 2 failed: expected (0 12), got ~A" point)))
    (format t "✓ Test 2 passed: Find from current position~%"))
  
  ;; Test 3: Text not found
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "missing")))
      (assert (not result) () "Test 3 failed: should not find 'missing'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Test 3 failed: point should not move when not found")))
    (format t "✓ Test 3 passed: Text not found~%"))
  
  ;; Test 4: Find across lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world test"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "world")))
      (assert result () "Test 4 failed: should find 'world' on next line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(1 0)) () "Test 4 failed: expected (1 0), got ~A" point)))
    (format t "✓ Test 4 passed: Find across lines~%"))
  
  (format t "All basic search-forward tests passed!~%~%"))

(defun test-search-forward-edge-cases ()
  "Test edge cases for search-forward"
  (format t "Running search-forward edge case tests...~%")
  
  ;; Test 1: Empty search string
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "")))
      (assert (not result) () "Test 1 failed: empty search should return nil"))
    (format t "✓ Test 1 passed: Empty search string~%"))
  
  ;; Test 2: Nil search string
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf nil)))
      (assert (not result) () "Test 2 failed: nil search should return nil"))
    (format t "✓ Test 2 passed: Nil search string~%"))
  
  ;; Test 3: Search at end of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)  ; At end
    (let ((result (search-forward buf "hello")))
      (assert (not result) () "Test 3 failed: should not find text when at end")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 11)) () "Test 3 failed: point should not move")))
    (format t "✓ Test 3 passed: Search at end of buffer~%"))
  
  ;; Test 4: Search in empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #())  ; Empty buffer
    (let ((result (search-forward buf "test")))
      (assert (not result) () "Test 4 failed: should not find text in empty buffer"))
    (format t "✓ Test 4 passed: Search in empty buffer~%"))
  
  ;; Test 5: Case sensitive search
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("Hello World"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "hello")))
      (assert (not result) () "Test 5 failed: search should be case sensitive"))
    (format t "✓ Test 5 passed: Case sensitive search~%"))
  
  ;; Test 6: Find partial word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "ell")))
      (assert result () "Test 6 failed: should find partial word 'ell'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 1)) () "Test 6 failed: expected (0 1), got ~A" point)))
    (format t "✓ Test 6 passed: Find partial word~%"))
  
  ;; Test 7: Single character search
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "o")))
      (assert result () "Test 7 failed: should find single character 'o'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 4)) () "Test 7 failed: expected (0 4), got ~A" point)))
    (format t "✓ Test 7 passed: Single character search~%"))
  
  (format t "All search-forward edge case tests passed!~%~%"))

(defun test-search-forward-multiline ()
  "Test search-forward across multiple lines"
  (format t "Running search-forward multiline tests...~%")
  
  ;; Test 1: Search across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line with target" "third line"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "target")))
      (assert result () "Test 1 failed: should find 'target' in second line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(1 17)) () "Test 1 failed: expected (1 17), got ~A" point)))
    (format t "✓ Test 1 passed: Search across multiple lines~%"))
  
  ;; Test 2: Search starting from middle line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first target" "second line" "third target"))
    (buffer-set-point buf 1 5)  ; Middle of second line
    (let ((result (search-forward buf "target")))
      (assert result () "Test 2 failed: should find 'target' in third line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(2 6)) () "Test 2 failed: expected (2 6), got ~A" point)))
    (format t "✓ Test 2 passed: Search starting from middle line~%"))
  
  ;; Test 3: Search with empty lines in between
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "" "target here"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "target")))
      (assert result () "Test 3 failed: should find 'target' after empty line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(2 0)) () "Test 3 failed: expected (2 0), got ~A" point)))
    (format t "✓ Test 3 passed: Search with empty lines~%"))
  
  ;; Test 4: Text spans across lines - should not find
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("tar" "get"))
    (buffer-set-point buf 0 0)
    (let ((result (search-forward buf "target")))
      (assert (not result) () "Test 4 failed: should not find text spanning lines"))
    (format t "✓ Test 4 passed: Text spanning lines not found~%"))
  
  (format t "All search-forward multiline tests passed!~%~%"))

(defun test-search-forward-undo ()
  "Test that search-forward creates proper undo records"
  (format t "Running search-forward undo tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 0)
    
    ;; Check initial state
    (let ((initial-point (buffer-get-point buf))
          (initial-size (tree-size (buffer-undo-tree buf))))
      
      ;; Perform search
      (let ((result (search-forward buf "world")))
        (assert result () "Search should succeed")
        
        ;; Check that undo record was created
        (let ((final-size (tree-size (buffer-undo-tree buf))))
          (assert (> final-size initial-size) ()
                  "Search should create undo record: initial ~A, final ~A" 
                  initial-size final-size))
        
        ;; Check that point moved
        (let ((final-point (buffer-get-point buf)))
          (assert (equal final-point '(0 6)) () "Point should move to found position"))
        
        ;; Test undo
        (buffer-undo buf)
        (let ((undo-point (buffer-get-point buf)))
          (assert (equal undo-point initial-point) ()
                  "Undo should restore original point position: expected ~A, got ~A"
                  initial-point undo-point))
        (format t "✓ Test 1 passed: Undo restores point position~%")
        
        ;; Test redo
        (buffer-redo buf)
        (let ((redo-point (buffer-get-point buf)))
          (assert (equal redo-point '(0 6)) ()
                  "Redo should restore search result position: expected (0 6), got ~A"
                  redo-point))
        (format t "✓ Test 2 passed: Redo restores search position~%")))
    
    ;; Test multiple undos/redos
    (buffer-undo buf)  ; Back to start
    (buffer-undo buf)  ; Should have no effect (already at start)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Multiple undos should not go beyond start"))
    (format t "✓ Test 3 passed: Multiple undos work correctly~%")
    
    (buffer-redo buf)  ; Forward to search result
    (buffer-redo buf)  ; Should have no effect (already at end)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Multiple redos should not go beyond end"))
    (format t "✓ Test 4 passed: Multiple redos work correctly~%"))
  
  (format t "All search-forward undo tests passed!~%~%"))

(defun test-search-forward-mark-clearing ()
  "Test that search-forward properly clears the mark"
  (format t "Running search-forward mark clearing tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 0)
    
    ;; Set a mark
    (buffer-set-mark buf 0 5)
    (let ((initial-mark (buffer-get-mark buf)))
      (assert initial-mark () "Mark should be set initially"))
    
    ;; Perform search
    (let ((result (search-forward buf "test")))
      (assert result () "Search should succeed")
      
      ;; Check that mark is cleared
      (let ((final-mark (buffer-get-mark buf)))
        (assert (not final-mark) () "Mark should be cleared after search"))
      (format t "✓ Test 1 passed: Mark cleared after successful search~%"))
    
    ;; Test unsuccessful search
    (buffer-set-mark buf 0 5)  ; Set mark again
    (let ((result (search-forward buf "missing")))
      (assert (not result) () "Search should fail")
      
      ;; Mark should still be there since search failed
      (let ((final-mark (buffer-get-mark buf)))
        (assert final-mark () "Mark should remain after unsuccessful search"))
      (format t "✓ Test 2 passed: Mark preserved after unsuccessful search~%")))
  
  (format t "All search-forward mark clearing tests passed!~%~%"))

(defun test-search-forward-minibuffer-integration ()
  "Test that search-forward works correctly through minibuffer interface"
  (format t "Running search-forward minibuffer integration tests...~%")
  
  ;; Test 1: Basic minibuffer search functionality  
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer with test content
    (setf (lines buffer) #("hello world test" "second line"))
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
    (format t "✓ Minibuffer activation works correctly~%")
    
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
    (format t "✓ Text input into minibuffer works~%")
    
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
      (format t "✓ Enter executes search and moves cursor~%")))
  
  ;; Test 2: Escape cancellation
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer
    (setf (lines buffer) #("hello world test"))
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
      (format t "✓ Escape cancels search without moving cursor~%")))
  
  (format t "All search-forward minibuffer integration tests passed!~%~%"))

(defun test-search-forward-key-binding-regression ()
  "Regression test for correct activate-minibuffer parameter usage"
  (format t "Running search-forward key binding regression tests...~%")
  
  ;; Test that simulates the key binding bug where callback was passed as completion function
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer
    (setf (lines buffer) #("hello world test"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; This should mimic what happens when C-s is pressed with correct parameters
    ;; The correct call: (activate-minibuffer editor "Search: " nil 'search-forward-command)
    ;; The incorrect call was: (activate-minibuffer editor "Search: " 'search-forward-command)
    (activate-minibuffer editor "Search: " nil 'search-forward-command)
    
    ;; Verify the callback is set correctly (not as completion function)
    (assert (eq (minibuffer-callback editor) 'search-forward-command) ()
            "Callback should be search-forward-command")
    (assert (null (minibuffer-completion-function editor)) ()
            "Completion function should be nil for search")
    (format t "✓ activate-minibuffer called with correct parameters~%")
    
    ;; Type a search string
    (let ((minibuf (minibuffer editor)))
      (insert-char minibuf #\w)
      (insert-char minibuf #\o)
      (insert-char minibuf #\r)
      (insert-char minibuf #\l)
      (insert-char minibuf #\d))
    
    ;; Execute the callback by pressing Enter
    (handle-minibuffer-input editor "Enter" nil nil nil nil)
    
    ;; Verify the search worked (cursor moved)
    (let ((final-point (buffer-get-point buffer)))
      (assert (equal final-point '(0 6)) ()
              "Search should work with correct parameter order, cursor at ~A" final-point))
    (format t "✓ Callback executed successfully with correct parameters~%"))
  
  (format t "All search-forward key binding regression tests passed!~%~%"))

(defun run-all-search-forward-tests ()
  "Run all search-forward tests"
  (format t "~%======================================~%")
  (format t "Running Search-Forward Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-search-forward-basic)
        (test-search-forward-edge-cases)
        (test-search-forward-multiline)
        (test-search-forward-undo)
        (test-search-forward-mark-clearing)
        (test-search-forward-minibuffer-integration)
        (test-search-forward-key-binding-regression)
        (format t "~%======================================~%")
        (format t "All search-forward tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: search-forward test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)