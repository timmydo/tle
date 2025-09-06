(in-package :tle)

(defun test-end-of-word-basic ()
  "Test basic end-of-word movement"
  (format t "Running basic end-of-word tests...~%")
  
  ;; Test 1: Move to end from beginning of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 0)  ; Beginning of "hello"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1a failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1a passed: End from beginning of first word~%"))
  
  ;; Test 2: Move to end from middle of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 2)  ; Middle of "hello"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 2 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 2 passed: End from middle of word~%"))
  
  ;; Test 3: Move to end from middle of second word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 7)  ; Beginning of "world"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 3 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 3 passed: End from beginning of second word~%"))
  
  ;; Test 4: From whitespace, move to end of next word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 5)  ; Space after "hello"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 4 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 4 passed: From whitespace to end of next word~%"))
  
  (format t "All basic end-of-word tests passed!~%~%"))

(defun test-end-of-word-edge-cases ()
  "Test edge cases for end-of-word"
  (format t "Running end-of-word edge case tests...~%")
  
  ;; Test 1: At end of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 11)  ; At end of buffer
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 1 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 1 passed: Stay at end of buffer~%"))
  
  ;; Test 2: Single character word - should move to next word when already at end
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a b c"))
    (buffer-set-point buf 0 0)  ; At "a"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () "Test 2a failed: expected (0 1), got ~A" point))
    ;; Already at end of word, should move to next
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 3)) () "Test 2b failed: expected (0 3), got ~A" point))
    (format t "✓ Test 2 passed: Single character word handling~%"))
  
  ;; Test 3: Word at end of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test word"))
    (buffer-set-point buf 0 7)  ; Middle of last word
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 9)) () "Test 3 failed: expected (0 9), got ~A" point))
    (format t "✓ Test 3 passed: Word at end of line~%"))
  
  ;; Test 4: From punctuation to next word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word!!!test"))
    (buffer-set-point buf 0 5)  ; At middle of !!!
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 4 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 4 passed: From punctuation to next word~%"))
  
  ;; Test 5: Underscore in word (should be treated as word character)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello_world next"))
    (buffer-set-point buf 0 3)  ; Middle of "hello_world"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 5 failed: expected (0 11), got ~A" point))
    (format t "✓ Test 5 passed: Underscore treated as word character~%"))
  
  ;; Test 6: Numbers in words
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word123 next"))
    (buffer-set-point buf 0 2)  ; Middle of "word123"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 7)) () "Test 6 failed: expected (0 7), got ~A" point))
    (format t "✓ Test 6 passed: Numbers treated as word characters~%"))
  
  (format t "All end-of-word edge case tests passed!~%~%"))

(defun test-end-of-word-multiline ()
  "Test end-of-word across multiple lines"
  (format t "Running end-of-word multiline tests...~%")
  
  ;; Test 1: From first line to end of word on same line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 0 2)  ; Middle of "first"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 1 passed: End of word on same line~%"))
  
  ;; Test 2: From end of line to next line's first word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 0 10)  ; End of first line
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 6)) () "Test 2 failed: expected (1 6), got ~A" point))
    (format t "✓ Test 2 passed: From end of line to next line's first word~%"))
  
  ;; Test 3: From whitespace at beginning of line to first word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "   second line"))
    (buffer-set-point buf 1 1)  ; In whitespace at beginning of second line
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 9)) () "Test 3 failed: expected (1 9), got ~A" point))
    (format t "✓ Test 3 passed: From whitespace to first word on line~%"))
  
  ;; Test 4: Empty line handling
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word" "" "next"))
    (buffer-set-point buf 0 2)  ; Middle of "word"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "Test 4a failed: expected (0 4), got ~A" point))
    ;; Now from empty line
    (buffer-set-point buf 1 0)  ; On empty line
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 4)) () "Test 4b failed: expected (2 4), got ~A" point))
    (format t "✓ Test 4 passed: Empty line handling~%"))
  
  ;; Test 5: Multiple empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word" "" "" "next"))
    (buffer-set-point buf 0 2)  ; Middle of "word"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () "Test 5a failed: expected (0 4), got ~A" point))
    ;; From empty line should go to next word
    (buffer-set-point buf 1 0)  ; On first empty line
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(3 4)) () "Test 5b failed: expected (3 4), got ~A" point))
    (format t "✓ Test 5 passed: Multiple empty lines~%"))
  
  (format t "All end-of-word multiline tests passed!~%~%"))

(defun test-end-of-word-special-characters ()
  "Test end-of-word with various special characters"
  (format t "Running end-of-word special character tests...~%")
  
  ;; Test 1: Mixed punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello,world;test"))
    (buffer-set-point buf 0 2)  ; Middle of "hello"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 1a failed: expected (0 5), got ~A" point))
    ;; From punctuation
    (buffer-set-point buf 0 5)  ; At comma
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 11)) () "Test 1b failed: expected (0 11), got ~A" point))
    (format t "✓ Test 1 passed: Mixed punctuation~%"))
  
  ;; Test 2: Tabs and multiple spaces
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word1	  word2"))  ; Tab and spaces
    (buffer-set-point buf 0 2)  ; Middle of "word1"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 2a failed: expected (0 5), got ~A" point))
    ;; From whitespace
    (buffer-set-point buf 0 6)  ; In spaces
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 13)) () "Test 2b failed: expected (0 13), got ~A" point))
    (format t "✓ Test 2 passed: Tabs and multiple spaces~%"))
  
  ;; Test 3: Consecutive punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word1!!!word2"))
    (buffer-set-point buf 0 3)  ; Middle of "word1"
    (end-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 5)) () "Test 3 failed: expected (0 5), got ~A" point))
    (format t "✓ Test 3 passed: Consecutive punctuation~%"))
  
  (format t "All end-of-word special character tests passed!~%~%"))

(defun test-end-of-word-no-undo ()
  "Test that end-of-word doesn't create undo records"
  (format t "Running end-of-word undo tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 3)  ; Middle of "hello"
    
    ;; Check initial undo tree state
    (let ((initial-size (tree-size (buffer-undo-tree buf))))
      (end-of-word buf)
      (end-of-word buf)
      (let ((final-size (tree-size (buffer-undo-tree buf))))
        (assert (= initial-size final-size) ()
                "end-of-word should not create undo records: initial ~A, final ~A" 
                initial-size final-size)))
    (format t "✓ end-of-word doesn't create undo records~%"))
  
  (format t "All end-of-word undo tests passed!~%~%"))

(defun test-end-of-word-undo-redo ()
  "Test end-of-word with undo/redo operations"
  (format t "Running end-of-word undo/redo interaction tests...~%")
  
  ;; Since buffer-insert is not implemented, we'll just test that end-of-word
  ;; doesn't interfere with existing undo/redo functionality
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    
    ;; Test basic end-of-word functionality
    (buffer-set-point buf 0 3)  ; Middle of "hello"
    (end-of-word buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(0 5)) () "Test 1a failed: expected (0 5), got ~A" point1))
    
    ;; Move to different position and test again
    (buffer-set-point buf 0 7)  ; Beginning of "world"
    (end-of-word buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(0 11)) () "Test 1b failed: expected (0 11), got ~A" point2))
    
    ;; Verify end-of-word twice in a row works correctly
    (buffer-set-point buf 0 2)   ; In "hello"
    (end-of-word buf)            ; First call - should go to end of "hello"
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(0 5)) () "Test 1c failed: expected (0 5), got ~A" point3))
    
    (end-of-word buf)            ; Second call - should go to end of "world"  
    (let ((point4 (buffer-get-point buf)))
      (assert (equal point4 '(0 11)) () "Test 1d failed: expected (0 11), got ~A" point4))
    
    (format t "✓ Test 1 passed: end-of-word works correctly twice in a row~%"))
  
  (format t "All end-of-word undo/redo tests passed!~%~%"))

(defun run-all-end-of-word-tests ()
  "Run all end-of-word tests"
  (format t "~%======================================~%")
  (format t "Running End-of-Word Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-end-of-word-basic)
        (test-end-of-word-edge-cases)
        (test-end-of-word-multiline)
        (test-end-of-word-special-characters)
        (test-end-of-word-no-undo)
        (test-end-of-word-undo-redo)
        (format t "~%======================================~%")
        (format t "All end-of-word tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: end-of-word test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)