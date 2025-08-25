(in-package :tle)

(defun test-search-backward-basic ()
  "Test basic search-backward functionality"
  (format t "Running basic search-backward tests...~%")
  
  ;; Test 1: Find text in same line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 16)  ; End of line
    (let ((result (search-backward buf "world")))
      (assert result () "Test 1a failed: should find 'world'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 6)) () "Test 1a failed: expected (0 6), got ~A" point)))
    (format t "✓ Test 1a passed: Find text in same line~%"))
  
  ;; Test 2: Find from current position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world hello"))
    (buffer-set-point buf 0 17)  ; After second "hello"
    (let ((result (search-backward buf "hello")))
      (assert result () "Test 2 failed: should find first 'hello'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 12)) () "Test 2 failed: expected (0 12), got ~A" point)))
    (format t "✓ Test 2 passed: Find from current position~%"))
  
  ;; Test 3: Text not found
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)
    (let ((result (search-backward buf "missing")))
      (assert (not result) () "Test 3 failed: should not find 'missing'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 11)) () "Test 3 failed: point should not move when not found")))
    (format t "✓ Test 3 passed: Text not found~%"))
  
  ;; Test 4: Find across lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world test"))
    (buffer-set-point buf 1 5)  ; End of second line
    (let ((result (search-backward buf "hello")))
      (assert result () "Test 4 failed: should find 'hello' on previous line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Test 4 failed: expected (0 0), got ~A" point)))
    (format t "✓ Test 4 passed: Find across lines~%"))
  
  (format t "All basic search-backward tests passed!~%~%"))

(defun test-search-backward-edge-cases ()
  "Test edge cases for search-backward"
  (format t "Running search-backward edge case tests...~%")
  
  ;; Test 1: Empty search string
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)
    (let ((result (search-backward buf "")))
      (assert (not result) () "Test 1 failed: empty search should return nil"))
    (format t "✓ Test 1 passed: Empty search string~%"))
  
  ;; Test 2: Nil search string
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)
    (let ((result (search-backward buf nil)))
      (assert (not result) () "Test 2 failed: nil search should return nil"))
    (format t "✓ Test 2 passed: Nil search string~%"))
  
  ;; Test 3: Search at beginning of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)  ; At beginning
    (let ((result (search-backward buf "hello")))
      (assert (not result) () "Test 3 failed: should not find text when at beginning")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Test 3 failed: point should not move")))
    (format t "✓ Test 3 passed: Search at beginning of buffer~%"))
  
  ;; Test 4: Search in empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #())  ; Empty buffer
    (let ((result (search-backward buf "test")))
      (assert (not result) () "Test 4 failed: should not find text in empty buffer"))
    (format t "✓ Test 4 passed: Search in empty buffer~%"))
  
  ;; Test 5: Case sensitive search
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("Hello World"))
    (buffer-set-point buf 0 11)
    (let ((result (search-backward buf "hello")))
      (assert (not result) () "Test 5 failed: search should be case sensitive"))
    (format t "✓ Test 5 passed: Case sensitive search~%"))
  
  ;; Test 6: Find partial word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)
    (let ((result (search-backward buf "ell")))
      (assert result () "Test 6 failed: should find partial word 'ell'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 1)) () "Test 6 failed: expected (0 1), got ~A" point)))
    (format t "✓ Test 6 passed: Find partial word~%"))
  
  ;; Test 7: Single character search
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)
    (let ((result (search-backward buf "o")))
      (assert result () "Test 7 failed: should find single character 'o'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 7)) () "Test 7 failed: expected (0 7), got ~A" point)))
    (format t "✓ Test 7 passed: Single character search~%"))
  
  (format t "All search-backward edge case tests passed!~%~%"))

(defun test-search-backward-multiline ()
  "Test search-backward across multiple lines"
  (format t "Running search-backward multiline tests...~%")
  
  ;; Test 1: Search backward across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line with target" "third line"))
    (buffer-set-point buf 2 10)  ; Middle of third line
    (let ((result (search-backward buf "target")))
      (assert result () "Test 1 failed: should find 'target' in second line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(1 17)) () "Test 1 failed: expected (1 17), got ~A" point)))
    (format t "✓ Test 1 passed: Search backward across multiple lines~%"))
  
  ;; Test 2: Search starting from middle line backward
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first target" "second line" "third target"))
    (buffer-set-point buf 1 5)  ; Middle of second line
    (let ((result (search-backward buf "target")))
      (assert result () "Test 2 failed: should find 'target' in first line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 6)) () "Test 2 failed: expected (0 6), got ~A" point)))
    (format t "✓ Test 2 passed: Search backward starting from middle line~%"))
  
  ;; Test 3: Search with empty lines in between
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("target here" "" "third line"))
    (buffer-set-point buf 2 5)
    (let ((result (search-backward buf "target")))
      (assert result () "Test 3 failed: should find 'target' before empty line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point)))
    (format t "✓ Test 3 passed: Search backward with empty lines~%"))
  
  ;; Test 4: Text spans across lines - should not find
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("tar" "get"))
    (buffer-set-point buf 1 3)
    (let ((result (search-backward buf "target")))
      (assert (not result) () "Test 4 failed: should not find text spanning lines"))
    (format t "✓ Test 4 passed: Text spanning lines not found~%"))
  
  (format t "All search-backward multiline tests passed!~%~%"))

(defun test-search-backward-undo ()
  "Test that search-backward creates proper undo records"
  (format t "Running search-backward undo tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 16)
    
    ;; Check initial state
    (let ((initial-point (buffer-get-point buf))
          (initial-size (tree-size (buffer-undo-tree buf))))
      
      ;; Perform search
      (let ((result (search-backward buf "world")))
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
      (assert (equal point '(0 16)) () "Multiple undos should not go beyond start"))
    (format t "✓ Test 3 passed: Multiple undos work correctly~%")
    
    (buffer-redo buf)  ; Forward to search result
    (buffer-redo buf)  ; Should have no effect (already at end)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Multiple redos should not go beyond end"))
    (format t "✓ Test 4 passed: Multiple redos work correctly~%"))
  
  (format t "All search-backward undo tests passed!~%~%"))

(defun test-search-backward-mark-clearing ()
  "Test that search-backward properly clears the mark"
  (format t "Running search-backward mark clearing tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 16)
    
    ;; Set a mark
    (buffer-set-mark buf 0 10)
    (let ((initial-mark (buffer-get-mark buf)))
      (assert initial-mark () "Mark should be set initially"))
    
    ;; Perform search
    (let ((result (search-backward buf "hello")))
      (assert result () "Search should succeed")
      
      ;; Check that mark is cleared
      (let ((final-mark (buffer-get-mark buf)))
        (assert (not final-mark) () "Mark should be cleared after search"))
      (format t "✓ Test 1 passed: Mark cleared after successful search~%"))
    
    ;; Test unsuccessful search
    (buffer-set-mark buf 0 5)  ; Set mark again
    (let ((result (search-backward buf "missing")))
      (assert (not result) () "Search should fail")
      
      ;; Mark should still be there since search failed
      (let ((final-mark (buffer-get-mark buf)))
        (assert final-mark () "Mark should remain after unsuccessful search"))
      (format t "✓ Test 2 passed: Mark preserved after unsuccessful search~%")))
  
  (format t "All search-backward mark clearing tests passed!~%~%"))

(defun test-search-backward-position-edge-cases ()
  "Test search-backward position handling edge cases"
  (format t "Running search-backward position edge case tests...~%")
  
  ;; Test 1: Search at current position should not find itself
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 6)  ; At beginning of "world"
    (let ((result (search-backward buf "world")))
      (assert (not result) () "Test 1 failed: should not find text at current position"))
    (format t "✓ Test 1 passed: Does not find text at current position~%"))
  
  ;; Test 2: Find text that ends at current position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (let ((result (search-backward buf "hello")))
      (assert result () "Test 2 failed: should find text that ends at current position")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point)))
    (format t "✓ Test 2 passed: Finds text ending at current position~%"))
  
  ;; Test 3: Multiple occurrences - find most recent
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test hello test hello test"))
    (buffer-set-point buf 0 26)  ; At end
    (let ((result (search-backward buf "hello")))
      (assert result () "Test 3 failed: should find most recent 'hello'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 16)) () "Test 3 failed: expected (0 16), got ~A" point)))
    (format t "✓ Test 3 passed: Finds most recent occurrence~%"))
  
  (format t "All search-backward position edge case tests passed!~%~%"))

(defun test-search-backward-minibuffer-integration ()
  "Test that search-backward works correctly through minibuffer interface"
  (format t "Running search-backward minibuffer integration tests...~%")
  
  ;; Test 1: Basic minibuffer search functionality  
  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer)))
    ;; Setup buffer with test content
    (setf (lines buffer) #("hello world test" "second line"))
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 16)
    
    ;; Test that activating search creates proper minibuffer state
    (activate-minibuffer editor "Search backward: " nil 'search-backward-command)
    
    ;; Verify minibuffer is active
    (assert (minibuffer-active-p editor) () "Minibuffer should be active after activation")
    (assert (string= (minibuffer-prompt editor) "Search backward: ") () 
            "Minibuffer prompt should be 'Search backward: '")
    (assert (eq (minibuffer-callback editor) 'search-backward-command) ()
            "Minibuffer callback should be search-backward-command")
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
      (assert (equal initial-point '(0 16)) () "Initial position should be (0 16)")
      
      ;; Simulate pressing Enter (this should execute the callback)
      (handle-minibuffer-input editor "Enter" nil nil nil)
      
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
    (buffer-set-point buffer 0 16)
    
    ;; Activate search and type some text
    (activate-minibuffer editor "Search backward: " nil 'search-backward-command)
    (let ((minibuf (minibuffer editor)))
      (insert-char minibuf #\w)
      (insert-char minibuf #\o)
      (insert-char minibuf #\r))
    
    (let ((initial-point (buffer-get-point buffer)))
      ;; Press Escape to cancel
      (handle-minibuffer-input editor "Escape" nil nil nil)
      
      ;; Verify minibuffer is deactivated
      (assert (not (minibuffer-active-p editor)) () 
              "Minibuffer should be deactivated after Escape")
      
      ;; Verify cursor didn't move (search was cancelled)
      (let ((final-point (buffer-get-point buffer)))
        (assert (equal initial-point final-point) () 
                "Cursor should not move when search is cancelled"))
      (format t "✓ Escape cancels search without moving cursor~%")))
  
  (format t "All search-backward minibuffer integration tests passed!~%~%"))

(defun run-all-search-backward-tests ()
  "Run all search-backward tests"
  (format t "~%======================================~%")
  (format t "Running Search-Backward Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-search-backward-basic)
        (test-search-backward-edge-cases)
        (test-search-backward-multiline)
        (test-search-backward-undo)
        (test-search-backward-mark-clearing)
        (test-search-backward-position-edge-cases)
        (test-search-backward-minibuffer-integration)
        (format t "~%======================================~%")
        (format t "All search-backward tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: search-backward test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)