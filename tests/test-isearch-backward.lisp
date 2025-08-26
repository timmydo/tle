(in-package :tle)

(defun test-isearch-backward-basic ()
  "Test basic isearch-backward functionality"
  (format t "Running basic isearch-backward tests...~%")
  
  ;; Test 1: Find text in same line, searching backward
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 16)  ; End of line
    (let ((result (isearch-backward buf "world")))
      (assert result () "Test 1a failed: should find 'world'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 6)) () "Test 1a failed: expected (0 6), got ~A" point)))
    (format t "✓ Test 1a passed: Find text in same line backwards~%"))
  
  ;; Test 2: Find from current position backwards
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world hello"))
    (buffer-set-point buf 0 17)  ; At end
    (let ((result (isearch-backward buf "hello")))
      (assert result () "Test 2 failed: should find second 'hello'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 12)) () "Test 2 failed: expected (0 12), got ~A" point)))
    (format t "✓ Test 2 passed: Find from current position backwards~%"))
  
  ;; Test 3: Text not found
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)
    (let ((result (isearch-backward buf "missing")))
      (assert (not result) () "Test 3 failed: should not find 'missing'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 5)) () "Test 3 failed: point should not move when not found")))
    (format t "✓ Test 3 passed: Text not found~%"))
  
  ;; Test 4: Find across lines backwards
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello target" "world test"))
    (buffer-set-point buf 1 10)  ; Second line
    (let ((result (isearch-backward buf "target")))
      (assert result () "Test 4 failed: should find 'target' on previous line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 6)) () "Test 4 failed: expected (0 6), got ~A" point)))
    (format t "✓ Test 4 passed: Find across lines backwards~%"))
  
  (format t "All basic isearch-backward tests passed!~%~%"))

(defun test-isearch-backward-edge-cases ()
  "Test edge cases for isearch-backward"
  (format t "Running isearch-backward edge case tests...~%")
  
  ;; Test 1: Empty search string
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)
    (let ((result (isearch-backward buf "")))
      (assert (not result) () "Test 1 failed: empty search should return nil"))
    (format t "✓ Test 1 passed: Empty search string~%"))
  
  ;; Test 2: Nil search string
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)
    (let ((result (isearch-backward buf nil)))
      (assert (not result) () "Test 2 failed: nil search should return nil"))
    (format t "✓ Test 2 passed: Nil search string~%"))
  
  ;; Test 3: Search at beginning of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)  ; At beginning
    (let ((result (isearch-backward buf "world")))
      (assert (not result) () "Test 3 failed: should not find text when at beginning")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Test 3 failed: point should not move")))
    (format t "✓ Test 3 passed: Search at beginning of buffer~%"))
  
  ;; Test 4: Search in empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #())  ; Empty buffer
    (let ((result (isearch-backward buf "test")))
      (assert (not result) () "Test 4 failed: should not find text in empty buffer"))
    (format t "✓ Test 4 passed: Search in empty buffer~%"))
  
  ;; Test 5: Case sensitive search
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("Hello World"))
    (buffer-set-point buf 0 11)
    (let ((result (isearch-backward buf "hello")))
      (assert (not result) () "Test 5 failed: search should be case sensitive"))
    (format t "✓ Test 5 passed: Case sensitive search~%"))
  
  ;; Test 6: Find partial word backward
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)
    (let ((result (isearch-backward buf "ell")))
      (assert result () "Test 6 failed: should find partial word 'ell'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 1)) () "Test 6 failed: expected (0 1), got ~A" point)))
    (format t "✓ Test 6 passed: Find partial word backward~%"))
  
  ;; Test 7: Single character search backward
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)
    (let ((result (isearch-backward buf "o")))
      (assert result () "Test 7 failed: should find single character 'o'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 7)) () "Test 7 failed: expected (0 7), got ~A" point)))
    (format t "✓ Test 7 passed: Single character search backward~%"))
  
  (format t "All isearch-backward edge case tests passed!~%~%"))

(defun test-isearch-backward-multiline ()
  "Test isearch-backward across multiple lines"
  (format t "Running isearch-backward multiline tests...~%")
  
  ;; Test 1: Search backward across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first target here" "second line" "third line"))
    (buffer-set-point buf 2 10)  ; Third line
    (let ((result (isearch-backward buf "target")))
      (assert result () "Test 1 failed: should find 'target' in first line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 6)) () "Test 1 failed: expected (0 6), got ~A" point)))
    (format t "✓ Test 1 passed: Search backward across multiple lines~%"))
  
  ;; Test 2: Search starting from middle line backwards
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first target" "second line" "third target"))
    (buffer-set-point buf 1 5)  ; Middle of second line
    (let ((result (isearch-backward buf "target")))
      (assert result () "Test 2 failed: should find 'target' in first line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 6)) () "Test 2 failed: expected (0 6), got ~A" point)))
    (format t "✓ Test 2 passed: Search backward from middle line~%"))
  
  ;; Test 3: Search backward with empty lines in between
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("target here" "" "last line"))
    (buffer-set-point buf 2 5)  ; Third line
    (let ((result (isearch-backward buf "target")))
      (assert result () "Test 3 failed: should find 'target' before empty line")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point)))
    (format t "✓ Test 3 passed: Search backward with empty lines~%"))
  
  ;; Test 4: Text spans across lines - should not find
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("tar" "get"))
    (buffer-set-point buf 1 3)
    (let ((result (isearch-backward buf "target")))
      (assert (not result) () "Test 4 failed: should not find text spanning lines"))
    (format t "✓ Test 4 passed: Text spanning lines not found~%"))
  
  (format t "All isearch-backward multiline tests passed!~%~%"))

(defun test-isearch-backward-undo ()
  "Test that isearch-backward creates proper undo records"
  (format t "Running isearch-backward undo tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 16)  ; End of line
    
    ;; Check initial state
    (let ((initial-point (buffer-get-point buf))
          (initial-size (tree-size (buffer-undo-tree buf))))
      
      ;; Perform search
      (let ((result (isearch-backward buf "world")))
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
  
  (format t "All isearch-backward undo tests passed!~%~%"))

(defun test-isearch-backward-mark-clearing ()
  "Test that isearch-backward properly clears the mark"
  (format t "Running isearch-backward mark clearing tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 16)  ; End
    
    ;; Set a mark
    (buffer-set-mark buf 0 10)
    (let ((initial-mark (buffer-get-mark buf)))
      (assert initial-mark () "Mark should be set initially"))
    
    ;; Perform search
    (let ((result (isearch-backward buf "hello")))
      (assert result () "Search should succeed")
      
      ;; Check that mark is cleared
      (let ((final-mark (buffer-get-mark buf)))
        (assert (not final-mark) () "Mark should be cleared after search"))
      (format t "✓ Test 1 passed: Mark cleared after successful search~%"))
    
    ;; Test unsuccessful search
    (buffer-set-mark buf 0 5)  ; Set mark again
    (let ((result (isearch-backward buf "missing")))
      (assert (not result) () "Search should fail")
      
      ;; Mark should still be there since search failed
      (let ((final-mark (buffer-get-mark buf)))
        (assert final-mark () "Mark should remain after unsuccessful search"))
      (format t "✓ Test 2 passed: Mark preserved after unsuccessful search~%")))
  
  (format t "All isearch-backward mark clearing tests passed!~%~%"))

(defun test-isearch-backward-incremental-behavior ()
  "Test incremental search behavior backwards - finding matches progressively"
  (format t "Running isearch-backward incremental behavior tests...~%")
  
  ;; Test progressive search backwards (simulating typing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("testing incremental search functionality" "test another line"))
    (buffer-set-point buf 1 17)  ; End of second line
    
    ;; Search for "t" backward - should find rightmost 't' before current position
    (let ((result (isearch-backward buf "t")))
      (assert result () "Should find 't'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(1 8)) () "Should find 't' in 'another' at position 8")))
    
    ;; Search for "te" from current position backwards
    (buffer-set-point buf 1 17)  ; Reset position
    (let ((result (isearch-backward buf "te")))
      (assert result () "Should find 'te'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(1 0)) () "Should find 'te' at beginning of 'test' in second line")))
    
    ;; Search for "test" backward from end
    (buffer-set-point buf 1 17)  ; Reset position
    (let ((result (isearch-backward buf "test")))
      (assert result () "Should find 'test'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(1 0)) () "Should find 'test' at beginning of second line")))
    
    ;; Move to first line and search for "test" again - should find in "testing"
    (buffer-set-point buf 0 30)  ; Middle of first line
    (let ((result (isearch-backward buf "test")))
      (assert result () "Should find 'test' in 'testing'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Should find 'test' at beginning of 'testing'")))
    
    (format t "✓ Test 1 passed: Progressive incremental search backwards works~%"))
  
  ;; Test search continuation from current position backwards
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("abc def abc ghi abc"))
    (buffer-set-point buf 0 19)  ; End of line
    
    ;; First search for "abc" backward
    (let ((result (isearch-backward buf "abc")))
      (assert result () "Should find last 'abc'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 16)) () "Should find last 'abc' at position 16")))
    
    ;; Move before last match and search again
    (buffer-set-point buf 0 15)  ; Before last "abc"
    (let ((result (isearch-backward buf "abc")))
      (assert result () "Should find second 'abc'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 8)) () "Should find second 'abc' at position 8")))
    
    ;; Move before second match and search again
    (buffer-set-point buf 0 7)  ; Before second "abc"
    (let ((result (isearch-backward buf "abc")))
      (assert result () "Should find first 'abc'")
      (let ((point (buffer-get-point buf)))
        (assert (equal point '(0 0)) () "Should find first 'abc' at position 0")))
    
    (format t "✓ Test 2 passed: Multiple match navigation backwards works~%"))
  
  (format t "All isearch-backward incremental behavior tests passed!~%~%"))

(defun run-all-isearch-backward-tests ()
  "Run all isearch-backward tests"
  (format t "~%======================================~%")
  (format t "Running Isearch-Backward Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-isearch-backward-basic)
        (test-isearch-backward-edge-cases)
        (test-isearch-backward-multiline)
        (test-isearch-backward-undo)
        (test-isearch-backward-mark-clearing)
        (test-isearch-backward-incremental-behavior)
        (format t "~%======================================~%")
        (format t "All isearch-backward tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: isearch-backward test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)