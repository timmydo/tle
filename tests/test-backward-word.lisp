(in-package :tle)

(defun test-backward-word-basic ()
  "Test basic backward-word movement"
  (format t "Running basic backward-word tests...~%")
  
  ;; Test 1: Move backward through words on a single line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world test"))
    (buffer-set-point buf 0 16)  ; End of "test"
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 12)) () "Test 1a failed: expected (0 12), got ~A" point))
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 1b failed: expected (0 6), got ~A" point))
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1c failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Backward through words on single line~%"))
  
  ;; Test 2: Move from middle of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 8)  ; Middle of "world"
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 2 failed: expected (0 6), got ~A" point))
    (format t "✓ Test 2 passed: Backward from middle of word~%"))
  
  ;; Test 3: Skip whitespace and punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("word1   !!!   word2"))
    (buffer-set-point buf 0 14)  ; Before "word2"
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Skip whitespace and punctuation~%"))
  
  ;; Test 4: At beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line"))
    (buffer-set-point buf 1 0)  ; Beginning of second line
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 4 failed: expected (0 6), got ~A" point))
    (format t "✓ Test 4 passed: Move to start of previous word across lines~%"))
  
  (format t "All basic backward-word tests passed!~%~%"))

(defun test-backward-word-edge-cases ()
  "Test edge cases for backward-word"
  (format t "Running backward-word edge case tests...~%")
  
  ;; Test 1: At beginning of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 0)  ; At beginning
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Stay at beginning of buffer~%"))
  
  ;; Test 2: Empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("word" ""))
    (buffer-set-point buf 1 0)  ; On empty line
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Backward from empty line to start of previous word~%"))
  
  ;; Test 3: Line with only whitespace
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("word" "   "))
    (buffer-set-point buf 1 3)  ; End of whitespace line
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Backward from line with only whitespace~%"))
  
  ;; Test 4: Line with only punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("word" "!!!"))
    (buffer-set-point buf 1 3)  ; End of punctuation line
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: Backward from line with only punctuation~%"))
  
  ;; Test 5: Single character word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("a b c"))
    (buffer-set-point buf 0 5)  ; End of "c"
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "Test 5a failed: expected (0 4), got ~A" point))
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 5b failed: expected (0 2), got ~A" point))
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 5c failed: expected (0 0), got ~A" point))
    (format t "✓ Test 5 passed: Single character words~%"))
  
  ;; Test 6: Underscore in word (should be treated as word character)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello_world next"))
    (buffer-set-point buf 0 16)  ; End of "next"
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 12)) () "Test 6a failed: expected (0 12), got ~A" point))
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 6b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 6 passed: Underscore treated as word character~%"))
  
  ;; Test 7: Numbers in words
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("word123 next"))
    (buffer-set-point buf 0 12)  ; End of "next"
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 8)) () "Test 7a failed: expected (0 8), got ~A" point))
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 7b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 7 passed: Numbers treated as word characters~%"))
  
  (format t "All backward-word edge case tests passed!~%~%"))

(defun test-backward-word-multiline ()
  "Test backward-word across multiple lines"
  (format t "Running backward-word multiline tests...~%")
  
  ;; Test 1: Across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line" "third line"))
    (buffer-set-point buf 2 10)  ; End of "third line"
    (backward-word buf)  ; "line"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 6)) () "Test 1a failed: expected (2 6), got ~A" point))
    (backward-word buf)  ; "third"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () "Test 1b failed: expected (2 0), got ~A" point))
    (backward-word buf)  ; should go to "line" on second line
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 7)) () "Test 1c failed: expected (1 7), got ~A" point))
    (format t "✓ Test 1 passed: Backward across multiple lines~%"))
  
  ;; Test 2: Empty lines in between
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "" "last"))
    (buffer-set-point buf 2 4)  ; End of "last"
    (backward-word buf)  ; to "last"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () "Test 2a failed: expected (2 0), got ~A" point))
    (backward-word buf)  ; skip empty line, go to "first"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Skip empty lines~%"))
  
  ;; Test 3: Multiple empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "" "" "last"))
    (buffer-set-point buf 3 4)  ; End of "last"
    (backward-word buf)  ; to "last"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(3 0)) () "Test 3a failed: expected (3 0), got ~A" point))
    (backward-word buf)  ; skip empty lines, go to "first"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Skip multiple empty lines~%"))
  
  ;; Test 4: Line ending with word, next line starting with word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("endword" "startword"))
    (buffer-set-point buf 1 9)  ; End of "startword"
    (backward-word buf)  ; to "startword"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 4a failed: expected (1 0), got ~A" point))
    (backward-word buf)  ; to "endword"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: Adjacent words across lines~%"))
  
  (format t "All backward-word multiline tests passed!~%~%"))

(defun test-backward-word-special-characters ()
  "Test backward-word with various special characters"
  (format t "Running backward-word special character tests...~%")
  
  ;; Test 1: Mixed punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello,world;test"))
    (buffer-set-point buf 0 16)  ; End of "test"
    (backward-word buf)  ; "test"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 12)) () "Test 1a failed: expected (0 12), got ~A" point))
    (backward-word buf)  ; "world" (skip semicolon)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 1b failed: expected (0 6), got ~A" point))
    (backward-word buf)  ; "hello" (skip comma)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1c failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Mixed punctuation~%"))
  
  ;; Test 2: Tabs and multiple spaces
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("word1	  word2"))  ; Tab and spaces
    (buffer-set-point buf 0 13)  ; End of "word2"
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 8)) () "Test 2a failed: expected (0 8), got ~A" point))
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Tabs and multiple spaces~%"))
  
  ;; Test 3: Start from punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("word1!!!word2"))
    (buffer-set-point buf 0 8)  ; At first ! after word1
    (backward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Start from punctuation~%"))
  
  (format t "All backward-word special character tests passed!~%~%"))

(defun test-backward-word-no-undo ()
  "Test that backward-word doesn't create undo records"
  (format t "Running backward-word undo tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 11)  ; End of line
    
    ;; Check initial undo tree state
    (let ((initial-size (tree-size (buffer-undo-tree buf))))
      (backward-word buf)
      (backward-word buf)
      (let ((final-size (tree-size (buffer-undo-tree buf))))
        (assert (= initial-size final-size) ()
                "backward-word should not create undo records: initial ~A, final ~A" 
                initial-size final-size)))
    (format t "✓ backward-word doesn't create undo records~%"))
  
  (format t "All backward-word undo tests passed!~%~%"))

(defun test-backward-word-undo-redo ()
  "Test backward-word operations with undo/redo functionality"
  (format t "Running backward-word undo/redo tests...~%")
  
  ;; Test that backward-word works after text operations and undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 6)  ; After "hello "
    
    ;; Insert text character by character
    (dolist (char '(#\a #\m #\a #\z #\i #\n #\g #\Space))
      (insert-char buf char))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 14)) () "After insert, expected (0 14), got ~A" point))
    
    ;; Test backward-word after insert
    (backward-word buf)  ; Should go to start of "amazing"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "After backward-word, expected (0 6), got ~A" point))
    
    ;; Undo the insert (undo character by character)
    (dotimes (i 8)  ; Undo 8 character insertions
      (buffer-undo buf))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "After undo, expected (0 6), got ~A" point))
    
    ;; Test backward-word after undo
    (backward-word buf)  ; Should go to start of "hello"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "After backward-word post-undo, expected (0 0), got ~A" point))
    
    ;; Redo the insert (redo character by character)
    (dotimes (i 8)  ; Redo 8 character insertions
      (buffer-redo buf))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 14)) () "After redo, expected (0 14), got ~A" point))
    
    ;; Test backward-word after redo
    (backward-word buf)  ; Should go to start of "amazing"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "After backward-word post-redo, expected (0 6), got ~A" point))
    
    (format t "✓ backward-word works correctly with undo/redo operations~%"))
  
  ;; Test multiple undo/redo cycles
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("one two three"))
    (buffer-set-point buf 0 13)  ; End of line
    
    ;; Insert text twice
    (dolist (char '(#\Space #\f #\o #\u #\r))
      (insert-char buf char))
    (dolist (char '(#\Space #\f #\i #\v #\e))
      (insert-char buf char))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 23)) () "After double insert, expected (0 23), got ~A" point))
    
    ;; Test backward-word
    (backward-word buf)  ; "five"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 19)) () "First backward-word, expected (0 19), got ~A" point))
    
    ;; Undo twice
    (dotimes (i 5) (buffer-undo buf))  ; Undo " five"
    (dotimes (i 5) (buffer-undo buf))  ; Undo " four"
    
    ;; Test backward-word after double undo
    (backward-word buf)  ; Should go to start of "three"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 8)) () "After double undo + backward-word, expected (0 8), got ~A" point))
    
    ;; Redo twice
    (dotimes (i 5) (buffer-redo buf))  ; Redo " four"
    (dotimes (i 5) (buffer-redo buf))  ; Redo " five"
    
    ;; Test backward-word after double redo
    (backward-word buf)  ; Should go to start of "five"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 19)) () "After double redo + backward-word, expected (0 19), got ~A" point))
    
    (format t "✓ backward-word works correctly with multiple undo/redo cycles~%"))
  
  (format t "All backward-word undo/redo tests passed!~%~%"))

(defun run-all-backward-word-tests ()
  "Run all backward-word tests"
  (format t "~%======================================~%")
  (format t "Running Backward-Word Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-backward-word-basic)
        (test-backward-word-edge-cases)
        (test-backward-word-multiline)
        (test-backward-word-special-characters)
        (test-backward-word-no-undo)
        (test-backward-word-undo-redo)
        (format t "~%======================================~%")
        (format t "All backward-word tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: backward-word test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)