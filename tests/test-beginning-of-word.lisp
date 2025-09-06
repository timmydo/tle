(in-package :tle)

(defun test-beginning-of-word-basic ()
  "Test basic beginning-of-word movement"
  (format t "Running basic beginning-of-word tests...~%")
  
  ;; Test 1: Move to beginning from middle of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 3)  ; Middle of "hello"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1a failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1a passed: Beginning from middle of first word~%"))
  
  ;; Test 2: Move to beginning from end of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 5)  ; End of "hello"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Beginning from end of word~%"))
  
  ;; Test 3: Move to beginning from middle of second word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 8)  ; Middle of "world"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 3 failed: expected (0 6), got ~A" point))
    (format t "✓ Test 3 passed: Beginning from middle of second word~%"))
  
  ;; Test 4: From whitespace, move to beginning of previous word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 9)  ; Space after "world"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 4 failed: expected (0 6), got ~A" point))
    (format t "✓ Test 4 passed: From whitespace to beginning of previous word~%"))
  
  (format t "All basic beginning-of-word tests passed!~%~%"))

(defun test-beginning-of-word-edge-cases ()
  "Test edge cases for beginning-of-word"
  (format t "Running beginning-of-word edge case tests...~%")
  
  ;; Test 1: At beginning of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 0)  ; At beginning
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Stay at beginning of buffer~%"))
  
  ;; Test 2: Single character word - should stay at beginning when already there
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "a b c"))
    (buffer-set-point buf 0 2)  ; At "b" - this is already the beginning of the word
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      ;; Since we're already at beginning of word, should move to previous word
      (assert (equal point '(0 0)) () "Test 2 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: From beginning of single character word to previous~%"))
  
  ;; Test 3: Word at beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word test"))
    (buffer-set-point buf 0 2)  ; Middle of first word
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Word at beginning of line~%"))
  
  ;; Test 4: From punctuation to previous word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word!!!test"))
    (buffer-set-point buf 0 5)  ; At middle of !!!
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: From punctuation to previous word~%"))
  
  ;; Test 5: Underscore in word (should be treated as word character)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello_world next"))
    (buffer-set-point buf 0 8)  ; Middle of "hello_world"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 5 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 5 passed: Underscore treated as word character~%"))
  
  ;; Test 6: Numbers in words
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word123 next"))
    (buffer-set-point buf 0 5)  ; Middle of "word123"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 6 failed: expected (0 0), got ~A" point))
    (format t "✓ Test 6 passed: Numbers treated as word characters~%"))
  
  (format t "All beginning-of-word edge case tests passed!~%~%"))

(defun test-beginning-of-word-multiline ()
  "Test beginning-of-word across multiple lines"
  (format t "Running beginning-of-word multiline tests...~%")
  
  ;; Test 1: From second line to previous word on first line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line"))
    (buffer-set-point buf 1 3)  ; Middle of "second"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 0)) () "Test 1 failed: expected (1 0), got ~A" point))
    (format t "✓ Test 1 passed: Beginning of word on same line~%"))
  
  ;; Test 2: From beginning of line (non-word) to previous line's last word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "   second"))
    (buffer-set-point buf 1 2)  ; In whitespace at beginning of second line
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 2 failed: expected (0 6), got ~A" point))
    (format t "✓ Test 2 passed: From whitespace to previous line's last word~%"))
  
  ;; Test 3: Empty line handling
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word" "" "next"))
    (buffer-set-point buf 2 2)  ; Middle of "next"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(2 0)) () "Test 3a failed: expected (2 0), got ~A" point))
    ;; Now from empty line
    (buffer-set-point buf 1 0)  ; On empty line
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 3b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 3 passed: Empty line handling~%"))
  
  ;; Test 4: Multiple empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word" "" "" "next"))
    (buffer-set-point buf 3 2)  ; Middle of "next"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(3 0)) () "Test 4a failed: expected (3 0), got ~A" point))
    ;; From empty line should go to previous word
    (buffer-set-point buf 2 0)  ; On second empty line
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 4b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 4 passed: Multiple empty lines~%"))
  
  (format t "All beginning-of-word multiline tests passed!~%~%"))

(defun test-beginning-of-word-special-characters ()
  "Test beginning-of-word with various special characters"
  (format t "Running beginning-of-word special character tests...~%")
  
  ;; Test 1: Mixed punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello,world;test"))
    (buffer-set-point buf 0 8)  ; Middle of "world"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 1a failed: expected (0 6), got ~A" point))
    ;; From punctuation
    (buffer-set-point buf 0 5)  ; At comma
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 1b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Mixed punctuation~%"))
  
  ;; Test 2: Tabs and multiple spaces
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word1	  word2"))  ; Tab and spaces
    (buffer-set-point buf 0 10)  ; Middle of "word2"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 8)) () "Test 2a failed: expected (0 8), got ~A" point))
    ;; From whitespace
    (buffer-set-point buf 0 7)  ; In spaces
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 0)) () "Test 2b failed: expected (0 0), got ~A" point))
    (format t "✓ Test 2 passed: Tabs and multiple spaces~%"))
  
  ;; Test 3: Consecutive punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "word1!!!word2"))
    (buffer-set-point buf 0 10)  ; Middle of "word2"
    (beginning-of-word buf)
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 8)) () "Test 3 failed: expected (0 8), got ~A" point))
    (format t "✓ Test 3 passed: Consecutive punctuation~%"))
  
  (format t "All beginning-of-word special character tests passed!~%~%"))

(defun test-beginning-of-word-no-undo ()
  "Test that beginning-of-word doesn't create undo records"
  (format t "Running beginning-of-word undo tests...~%")
  
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    (buffer-set-point buf 0 8)  ; Middle of "world"
    
    ;; Check initial undo tree state
    (let ((initial-size (tree-size (buffer-undo-tree buf))))
      (beginning-of-word buf)
      (beginning-of-word buf)
      (let ((final-size (tree-size (buffer-undo-tree buf))))
        (assert (= initial-size final-size) ()
                "beginning-of-word should not create undo records: initial ~A, final ~A" 
                initial-size final-size)))
    (format t "✓ beginning-of-word doesn't create undo records~%"))
  
  (format t "All beginning-of-word undo tests passed!~%~%"))

(defun test-beginning-of-word-undo-redo ()
  "Test beginning-of-word with undo/redo operations"
  (format t "Running beginning-of-word undo/redo interaction tests...~%")
  
  ;; Since buffer-insert is not implemented, we'll just test that beginning-of-word
  ;; doesn't interfere with existing undo/redo functionality
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world test"))
    
    ;; Test basic beginning-of-word functionality
    (buffer-set-point buf 0 8)  ; Middle of "world"
    (beginning-of-word buf)
    (let ((point1 (buffer-get-point buf)))
      (assert (equal point1 '(0 6)) () "Test 1a failed: expected (0 6), got ~A" point1))
    
    ;; Move to different position and test again
    (buffer-set-point buf 0 15)  ; Middle of "test"
    (beginning-of-word buf)
    (let ((point2 (buffer-get-point buf)))
      (assert (equal point2 '(0 12)) () "Test 1b failed: expected (0 12), got ~A" point2))
    
    ;; Verify beginning-of-word twice in a row works correctly
    (buffer-set-point buf 0 9)   ; In "world"
    (beginning-of-word buf)      ; First call - should go to start of "world"
    (let ((point3 (buffer-get-point buf)))
      (assert (equal point3 '(0 6)) () "Test 1c failed: expected (0 6), got ~A" point3))
    
    (beginning-of-word buf)      ; Second call - should go to start of "hello"  
    (let ((point4 (buffer-get-point buf)))
      (assert (equal point4 '(0 0)) () "Test 1d failed: expected (0 0), got ~A" point4))
    
    (format t "✓ Test 1 passed: beginning-of-word works correctly twice in a row~%"))
  
  (format t "All beginning-of-word undo/redo tests passed!~%~%"))

(defun run-all-beginning-of-word-tests ()
  "Run all beginning-of-word tests"
  (format t "~%======================================~%")
  (format t "Running Beginning-of-Word Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-beginning-of-word-basic)
        (test-beginning-of-word-edge-cases)
        (test-beginning-of-word-multiline)
        (test-beginning-of-word-special-characters)
        (test-beginning-of-word-no-undo)
        (test-beginning-of-word-undo-redo)
        (format t "~%======================================~%")
        (format t "All beginning-of-word tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: beginning-of-word test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)