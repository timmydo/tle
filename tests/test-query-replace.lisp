(in-package :tle)

(defun test-query-replace-basic ()
  "Test basic query-replace functionality"
  (format t "Running basic query-replace tests...~%")
  
  ;; Test 1: Single replacement in same line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "world" "universe")))
      (assert (= result 1) () "Test 1a failed: should replace 1 occurrence")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "hello universe test") () "Test 1a failed: expected 'hello universe test', got '~A'" line)))
    (format t "✓ Test 1a passed: Single replacement in same line~%"))
  
  ;; Test 2: Multiple replacements in same line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("foo bar foo baz foo"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "foo" "qux")))
      (assert (= result 3) () "Test 2 failed: should replace 3 occurrences")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "qux bar qux baz qux") () "Test 2 failed: expected 'qux bar qux baz qux', got '~A'" line)))
    (format t "✓ Test 2 passed: Multiple replacements in same line~%"))
  
  ;; Test 3: No matches found
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 5)
    (let ((result (query-replace buf "missing" "found"))
          (point (buffer-get-point buf)))
      (assert (= result 0) () "Test 3 failed: should replace 0 occurrences")
      (assert (equal point '(0 5)) () "Test 3 failed: point should remain at original position when no matches"))
    (format t "✓ Test 3 passed: No matches found~%"))
  
  ;; Test 4: Replacement across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first test line" "second test here" "third test end"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "test" "exam")))
      (assert (= result 3) () "Test 4 failed: should replace 3 occurrences")
      (assert (string= (buffer-line buf 0) "first exam line") () "Test 4 failed: line 0 wrong")
      (assert (string= (buffer-line buf 1) "second exam here") () "Test 4 failed: line 1 wrong")
      (assert (string= (buffer-line buf 2) "third exam end") () "Test 4 failed: line 2 wrong"))
    (format t "✓ Test 4 passed: Replacement across multiple lines~%"))
  
  (format t "All basic query-replace tests passed!~%~%"))

(defun test-query-replace-edge-cases ()
  "Test edge cases for query-replace"
  (format t "Running query-replace edge case tests...~%")
  
  ;; Test 1: Empty search string
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "" "replacement")))
      (assert (= result 0) () "Test 1 failed: empty search should return 0 replacements"))
    (format t "✓ Test 1 passed: Empty search string~%"))
  
  ;; Test 2: Nil search string  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf nil "replacement")))
      (assert (= result 0) () "Test 2 failed: nil search should return 0 replacements"))
    (format t "✓ Test 2 passed: Nil search string~%"))
  
  ;; Test 3: Empty replacement string
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world hello"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "hello" "")))
      (assert (= result 2) () "Test 3 failed: should replace 2 occurrences")
      (let ((line (buffer-line buf 0)))
        (assert (string= line " world ") () "Test 3 failed: expected ' world ', got '~A'" line)))
    (format t "✓ Test 3 passed: Empty replacement string~%"))
  
  ;; Test 4: Search in empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #())
    (let ((result (query-replace buf "test" "replacement")))
      (assert (= result 0) () "Test 4 failed: should not replace anything in empty buffer"))
    (format t "✓ Test 4 passed: Search in empty buffer~%"))
  
  ;; Test 5: Case sensitive search
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("Hello World hello"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "hello" "hi")))
      (assert (= result 1) () "Test 5 failed: should only replace lowercase 'hello'")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "Hello World hi") () "Test 5 failed: expected 'Hello World hi', got '~A'" line)))
    (format t "✓ Test 5 passed: Case sensitive search~%"))
  
  ;; Test 6: Replacement text longer than original
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("a b a"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "a" "replacement")))
      (assert (= result 2) () "Test 6 failed: should replace 2 occurrences")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "replacement b replacement") () "Test 6 failed: expected 'replacement b replacement', got '~A'" line)))
    (format t "✓ Test 6 passed: Replacement text longer than original~%"))
  
  ;; Test 7: Replacement text shorter than original
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("testing another testing"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "testing" "x")))
      (assert (= result 2) () "Test 7 failed: should replace 2 occurrences")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "x another x") () "Test 7 failed: expected 'x another x', got '~A'" line)))
    (format t "✓ Test 7 passed: Replacement text shorter than original~%"))
  
  (format t "All query-replace edge case tests passed!~%~%"))

(defun test-query-replace-position-handling ()
  "Test query-replace position and cursor handling"
  (format t "Running query-replace position handling tests...~%")
  
  ;; Test 1: Starting position doesn't affect results (always starts from beginning)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test middle test end"))
    (buffer-set-point buf 0 10)  ; Start in middle
    (let ((result (query-replace buf "test" "exam")))
      (assert (= result 2) () "Test 1 failed: should find all matches regardless of starting position")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "exam middle exam end") () "Test 1 failed: both occurrences should be replaced")))
    (format t "✓ Test 1 passed: Starting position handling~%"))
  
  ;; Test 2: Cursor position after successful replacements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 8)
    (query-replace buf "world" "universe")
    (let ((point (buffer-get-point buf)))
      ;; Should be at end of last replacement (6 + 8 = 14)
      (assert (equal point '(0 14)) () "Test 2 failed: cursor should be at end of replacement, got ~A" point))
    (format t "✓ Test 2 passed: Cursor position after replacement~%"))
  
  ;; Test 3: Multiple line replacements position tracking
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first foo" "second foo" "third foo"))
    (buffer-set-point buf 1 5)
    (query-replace buf "foo" "bar")
    (let ((point (buffer-get-point buf)))
      ;; Should be at end of last replacement (line 2, after "bar")
      (assert (equal point '(2 9)) () "Test 3 failed: cursor should be at end of last replacement, got ~A" point))
    (format t "✓ Test 3 passed: Multi-line position tracking~%"))
  
  (format t "All query-replace position handling tests passed!~%~%"))

(defun test-query-replace-undo ()
  "Test that query-replace creates proper undo records"
  (format t "Running query-replace undo tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 0)
    
    ;; Check initial state
    (let ((initial-size (tree-size (buffer-undo-tree buf)))
          (initial-content (buffer-line buf 0)))
      
      ;; Perform replacement
      (let ((result (query-replace buf "world" "universe")))
        (assert (= result 1) () "Replacement should succeed")
        
        ;; Check that undo records were created
        (let ((final-size (tree-size (buffer-undo-tree buf))))
          (assert (> final-size initial-size) ()
                  "Replacement should create undo records: initial ~A, final ~A" 
                  initial-size final-size))
        
        ;; Check that content changed
        (let ((final-content (buffer-line buf 0)))
          (assert (string= final-content "hello universe test") () 
                  "Content should be changed after replacement"))
        
        ;; Test undo
        (buffer-undo buf)
        (let ((undo-content (buffer-line buf 0)))
          (assert (string= undo-content initial-content) ()
                  "Undo should restore original content: expected '~A', got '~A'"
                  initial-content undo-content))
        (format t "✓ Test 1 passed: Undo restores original content~%")
        
        ;; Test redo
        (buffer-redo buf)
        (let ((redo-content (buffer-line buf 0)))
          (assert (string= redo-content "hello universe test") ()
                  "Redo should restore replacement: expected 'hello universe test', got '~A'"
                  redo-content))
        (format t "✓ Test 2 passed: Redo restores replacement~%")))
    
    ;; Test multiple replacements with undo
    (let ((buf2 (make-instance 'standard-buffer)))
      (setf (lines buf2) #("foo bar foo"))
      (buffer-set-point buf2 0 0)
      (let ((initial-content (buffer-line buf2 0)))
        (query-replace buf2 "foo" "baz")
        
        ;; Undo should restore original
        (buffer-undo buf2)
        (let ((undo-content (buffer-line buf2 0)))
          (assert (string= undo-content initial-content) ()
                  "Undo should restore original content for multiple replacements"))
        (format t "✓ Test 3 passed: Multiple replacements undo correctly~%")))
    
    ;; Test multiple undos/redos in a row
    (buffer-undo buf)  ; Back to original
    (buffer-undo buf)  ; Should have no effect (already at start)
    (let ((content (buffer-line buf 0)))
      (assert (string= content "hello world test") () 
              "Multiple undos should not go beyond start"))
    (format t "✓ Test 4 passed: Multiple undos work correctly~%")
    
    (buffer-redo buf)  ; Forward to replacement
    (buffer-redo buf)  ; Should have no effect (already at end)
    (let ((content (buffer-line buf 0)))
      (assert (string= content "hello universe test") ()
              "Multiple redos should not go beyond end"))
    (format t "✓ Test 5 passed: Multiple redos work correctly~%"))
  
  (format t "All query-replace undo tests passed!~%~%"))

(defun test-query-replace-complex-patterns ()
  "Test query-replace with complex replacement patterns"
  (format t "Running query-replace complex pattern tests...~%")
  
  ;; Test 1: Overlapping matches prevention
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("aaaa"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "aa" "bb")))
      ;; Should replace "aa" at positions 0-1 and 2-3, resulting in "bbbb"
      (assert (= result 2) () "Test 1 failed: should replace 2 non-overlapping occurrences")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "bbbb") () "Test 1 failed: expected 'bbbb', got '~A'" line)))
    (format t "✓ Test 1 passed: Overlapping matches handled correctly~%"))
  
  ;; Test 2: Replacement affects subsequent search positions
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("ab ab ab"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "ab" "xyz")))
      (assert (= result 3) () "Test 2 failed: should replace 3 occurrences")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "xyz xyz xyz") () "Test 2 failed: expected 'xyz xyz xyz', got '~A'" line)))
    (format t "✓ Test 2 passed: Position tracking with length changes~%"))
  
  ;; Test 3: Special characters in search/replace
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello (world) test"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "(world)" "[universe]")))
      (assert (= result 1) () "Test 3 failed: should replace 1 occurrence")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "hello [universe] test") () "Test 3 failed: expected 'hello [universe] test', got '~A'" line)))
    (format t "✓ Test 3 passed: Special characters in patterns~%"))
  
  ;; Test 4: Single character replacements
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("a.b.c.d"))
    (buffer-set-point buf 0 0)
    (let ((result (query-replace buf "." ",")))
      (assert (= result 3) () "Test 4 failed: should replace 3 occurrences")
      (let ((line (buffer-line buf 0)))
        (assert (string= line "a,b,c,d") () "Test 4 failed: expected 'a,b,c,d', got '~A'" line)))
    (format t "✓ Test 4 passed: Single character replacements~%"))
  
  (format t "All query-replace complex pattern tests passed!~%~%"))

(defun run-all-query-replace-tests ()
  "Run all query-replace tests"
  (format t "~%======================================~%")
  (format t "Running Query-Replace Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-query-replace-basic)
        (test-query-replace-edge-cases)
        (test-query-replace-position-handling)
        (test-query-replace-undo)
        (test-query-replace-complex-patterns)
        (format t "~%======================================~%")
        (format t "All query-replace tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: query-replace test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)