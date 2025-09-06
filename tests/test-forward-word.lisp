(in-package :tle)

(defun test-forward-word-basic ()
  "Test basic forward-word movement"
  (format t "Running basic forward-word tests...~%")
  
  ;; Test 1: Move forward through words on a single line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 0)  ; Beginning of "hello"
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1a failed: expected (0 5), got ~A" point))
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 1b failed: expected (0 11), got ~A" point))
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 16)) () "Test 1c failed: expected (0 16), got ~A" point))
    (format t "✓ Test 1 passed: Forward through words on single line~%"))
  
  ;; Test 2: Move from middle of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 3)  ; Middle of "hello"
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 2 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 2 passed: Forward from middle of word~%"))
  
  ;; Test 3: Skip whitespace and punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word1   !!!   word2"))
    (buffer-set-point buf 0 5)  ; After "word1"
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 19)) () "Test 3 failed: expected (0 19), got ~A" point))
    (format t "✓ Test 3 passed: Skip whitespace and punctuation~%"))
  
  ;; Test 4: At end of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world" "second line"))
    (buffer-set-point buf 0 11)  ; End of first line
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 6)) () "Test 4 failed: expected (1 6), got ~A" point))
    (format t "✓ Test 4 passed: Move to end of next word across lines~%"))
  
  (format t "All basic forward-word tests passed!~%~%"))

(defun test-forward-word-edge-cases ()
  "Test edge cases for forward-word"
  (format t "Running forward-word edge case tests...~%")
  
  ;; Test 1: At end of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 11)  ; At end
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 1 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 1 passed: Stay at end of buffer~%"))
  
  ;; Test 2: Empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "" "word"))
    (buffer-set-point buf 0 0)
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 4)) () "Test 2 failed: expected (1 4), got ~A" point))
    (format t "✓ Test 2 passed: Forward from empty line to end of next word~%"))
  
  ;; Test 3: Line with only whitespace
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "   " "word"))
    (buffer-set-point buf 0 0)
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 4)) () "Test 3 failed: expected (1 4), got ~A" point))
    (format t "✓ Test 3 passed: Forward from line with only whitespace to next word~%"))
  
  ;; Test 4: Line with only punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "!!!" "word"))
    (buffer-set-point buf 0 0)
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 4)) () "Test 4 failed: expected (1 4), got ~A" point))
    (format t "✓ Test 4 passed: Forward from line with only punctuation to next word~%"))
  
  ;; Test 5: Single character word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a b c"))
    (buffer-set-point buf 0 0)
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 5a failed: expected (0 1), got ~A" point))
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 3)) () "Test 5b failed: expected (0 3), got ~A" point))
    (format t "✓ Test 5 passed: Single character words~%"))
  
  ;; Test 6: Underscore in word (should be treated as word character)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello_world next"))
    (buffer-set-point buf 0 0)
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 6 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 6 passed: Underscore treated as word character~%"))
  
  ;; Test 7: Numbers in words
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word123 next"))
    (buffer-set-point buf 0 0)
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 7)) () "Test 7 failed: expected (0 7), got ~A" point))
    (format t "✓ Test 7 passed: Numbers treated as word characters~%"))
  
  (format t "All forward-word edge case tests passed!~%~%"))

(defun test-forward-word-multiline ()
  "Test forward-word across multiple lines"
  (format t "Running forward-word multiline tests...~%")
  
  ;; Test 1: Across multiple lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line"))
    (buffer-set-point buf 0 0)
    (forward-word buf)  ; "first"
    (forward-word buf)  ; "line"
    (forward-word buf)  ; should go to "second"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 6)) () "Test 1 failed: expected (1 6), got ~A" point))
    (format t "✓ Test 1 passed: Forward across multiple lines~%"))
  
  ;; Test 2: Empty lines in between
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word" "" "next"))
    (buffer-set-point buf 0 0)
    (forward-word buf)  ; to end of "word"
    (forward-word buf)  ; skip empty line, go to "next"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 4)) () "Test 2 failed: expected (2 4), got ~A" point))
    (format t "✓ Test 2 passed: Skip empty lines~%"))
  
  ;; Test 3: Multiple empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word" "" "" "next"))
    (buffer-set-point buf 0 0)
    (forward-word buf)  ; to end of "word"
    (forward-word buf)  ; skip empty lines, go to "next"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(3 4)) () "Test 3 failed: expected (3 4), got ~A" point))
    (format t "✓ Test 3 passed: Skip multiple empty lines~%"))
  
  ;; Test 4: Line ending with word, next line starting with word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "endword" "startword"))
    (buffer-set-point buf 0 0)
    (forward-word buf)  ; to end of "endword"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 7)) () "Test 4a failed: expected (0 7), got ~A" point))
    (forward-word buf)  ; to end of "startword"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 9)) () "Test 4b failed: expected (1 9), got ~A" point))
    (format t "✓ Test 4 passed: Adjacent words across lines~%"))
  
  (format t "All forward-word multiline tests passed!~%~%"))

(defun test-forward-word-special-characters ()
  "Test forward-word with various special characters"
  (format t "Running forward-word special character tests...~%")
  
  ;; Test 1: Mixed punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello,world;test"))
    (buffer-set-point buf 0 0)
    (forward-word buf)  ; "hello"
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1a failed: expected (0 5), got ~A" point))
    (forward-word buf)  ; "world" (skip comma)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 1b failed: expected (0 11), got ~A" point))
    (forward-word buf)  ; "test" (skip semicolon)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 16)) () "Test 1c failed: expected (0 16), got ~A" point))
    (format t "✓ Test 1 passed: Mixed punctuation~%"))
  
  ;; Test 2: Tabs and multiple spaces
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word1	  word2"))  ; Tab and spaces
    (buffer-set-point buf 0 0)
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 2a failed: expected (0 5), got ~A" point))
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 13)) () "Test 2b failed: expected (0 13), got ~A" point))
    (format t "✓ Test 2 passed: Tabs and multiple spaces~%"))
  
  ;; Test 3: Start from punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word1!!!word2"))
    (buffer-set-point buf 0 5)  ; At first !
    (forward-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 13)) () "Test 3 failed: expected (0 13), got ~A" point))
    (format t "✓ Test 3 passed: Start from punctuation~%"))
  
  (format t "All forward-word special character tests passed!~%~%"))

(defun test-forward-word-no-undo ()
  "Test that forward-word doesn't create undo records"
  (format t "Running forward-word undo tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 0)
    
    ;; Check initial undo tree state
    (let ((initial-size (tree-size (buffer-undo-tree buf))))
      (forward-word buf)
      (forward-word buf)
      (let ((final-size (tree-size (buffer-undo-tree buf))))
        (assert (= initial-size final-size) ()
                "forward-word should not create undo records: initial ~A, final ~A" 
                initial-size final-size)))
    (format t "✓ forward-word doesn't create undo records~%"))
  
  (format t "All forward-word undo tests passed!~%~%"))

(defun run-all-forward-word-tests ()
  "Run all forward-word tests"
  (format t "~%======================================~%")
  (format t "Running Forward-Word Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-forward-word-basic)
        (test-forward-word-edge-cases)
        (test-forward-word-multiline)
        (test-forward-word-special-characters)
        (test-forward-word-no-undo)
        (format t "~%======================================~%")
        (format t "All forward-word tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: forward-word test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)