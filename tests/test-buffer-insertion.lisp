(in-package :tle)

(defun test-insert-char-beginning ()
  "Test inserting characters at the beginning of lines"
  (format t "Running insert-char at beginning tests...~%")
  
  ;; Test 1: Insert at beginning of first line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world"))
    (buffer-set-point buf 0 0)
    (insert-char buf #\X)
    (assert (string= (buffer-line buf 0) "Xhello") () 
            "Test 1 failed: expected 'Xhello', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () 
              "Test 1 point failed: expected (0 1), got ~A" point))
    (format t "✓ Test 1 passed: Insert at beginning of first line~%"))
  
  ;; Test 2: Insert at beginning of middle line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line1" "line2" "line3"))
    (buffer-set-point buf 1 0)
    (insert-char buf #\A)
    (assert (string= (buffer-line buf 1) "Aline2") () 
            "Test 2 failed: expected 'Aline2', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 1)) () 
              "Test 2 point failed: expected (1 1), got ~A" point))
    (format t "✓ Test 2 passed: Insert at beginning of middle line~%"))
  
  ;; Test 3: Insert at beginning of empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("" "content"))
    (buffer-set-point buf 0 0)
    (insert-char buf #\Z)
    (assert (string= (buffer-line buf 0) "Z") () 
            "Test 3 failed: expected 'Z', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 1)) () 
              "Test 3 point failed: expected (0 1), got ~A" point))
    (format t "✓ Test 3 passed: Insert at beginning of empty line~%"))
  
  (format t "All insert-char beginning tests passed!~%~%"))

(defun test-insert-char-middle ()
  "Test inserting characters in the middle of lines"
  (format t "Running insert-char at middle tests...~%")
  
  ;; Test 1: Insert in middle of word
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world"))
    (buffer-set-point buf 0 3)  ; Between 'l' and 'l'
    (insert-char buf #\X)
    (assert (string= (buffer-line buf 0) "helXlo") () 
            "Test 1 failed: expected 'helXlo', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () 
              "Test 1 point failed: expected (0 4), got ~A" point))
    (format t "✓ Test 1 passed: Insert in middle of word~%"))
  
  ;; Test 2: Insert between words
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)  ; Between 'hello' and ' world'
    (insert-char buf #\-)
    (assert (string= (buffer-line buf 0) "hello- world") () 
            "Test 2 failed: expected 'hello- world', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () 
              "Test 2 point failed: expected (0 6), got ~A" point))
    (format t "✓ Test 2 passed: Insert between words~%"))
  
  ;; Test 3: Multiple insertions in sequence
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("abc"))
    (buffer-set-point buf 0 1)  ; Between 'a' and 'b'
    (insert-char buf #\1)
    (insert-char buf #\2)
    (insert-char buf #\3)
    (assert (string= (buffer-line buf 0) "a123bc") () 
            "Test 3 failed: expected 'a123bc', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 4)) () 
              "Test 3 point failed: expected (0 4), got ~A" point))
    (format t "✓ Test 3 passed: Multiple insertions in sequence~%"))
  
  (format t "All insert-char middle tests passed!~%~%"))

(defun test-insert-char-end ()
  "Test inserting characters at the end of lines"
  (format t "Running insert-char at end tests...~%")
  
  ;; Test 1: Insert at end of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world"))
    (buffer-set-point buf 0 5)  ; At end of "hello"
    (insert-char buf #\!)
    (assert (string= (buffer-line buf 0) "hello!") () 
            "Test 1 failed: expected 'hello!', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () 
              "Test 1 point failed: expected (0 6), got ~A" point))
    (format t "✓ Test 1 passed: Insert at end of line~%"))
  
  ;; Test 2: Insert at end of last line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "last"))
    (buffer-set-point buf 1 4)  ; At end of "last"
    (insert-char buf #\.)
    (assert (string= (buffer-line buf 1) "last.") () 
            "Test 2 failed: expected 'last.', got '~A'" (buffer-line buf 1))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(1 5)) () 
              "Test 2 point failed: expected (1 5), got ~A" point))
    (format t "✓ Test 2 passed: Insert at end of last line~%"))
  
  ;; Test 3: Insert multiple characters at end
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test"))
    (buffer-set-point buf 0 4)  ; At end of "test"
    (insert-char buf #\i)
    (insert-char buf #\n)
    (insert-char buf #\g)
    (assert (string= (buffer-line buf 0) "testing") () 
            "Test 3 failed: expected 'testing', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 7)) () 
              "Test 3 point failed: expected (0 7), got ~A" point))
    (format t "✓ Test 3 passed: Insert multiple characters at end~%"))
  
  (format t "All insert-char end tests passed!~%~%"))

(defun test-insert-char-special-characters ()
  "Test inserting special characters"
  (format t "Running insert-char special characters tests...~%")
  
  ;; Test 1: Insert space character
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("helloworld"))
    (buffer-set-point buf 0 5)
    (insert-char buf #\Space)
    (assert (string= (buffer-line buf 0) "hello world") () 
            "Test 1 failed: expected 'hello world', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Insert space character~%"))
  
  ;; Test 2: Insert punctuation
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello"))
    (buffer-set-point buf 0 5)
    (insert-char buf #\,)
    (insert-char buf #\Space)
    (insert-char buf #\")
    (insert-char buf #\w)
    (insert-char buf #\o)
    (insert-char buf #\r)
    (insert-char buf #\l)
    (insert-char buf #\d)
    (insert-char buf #\")
    (assert (string= (buffer-line buf 0) "hello, \"world\"") () 
            "Test 2 failed: expected 'hello, \"world\"', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Insert punctuation~%"))
  
  ;; Test 3: Insert digits
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("version"))
    (buffer-set-point buf 0 7)
    (insert-char buf #\Space)
    (insert-char buf #\1)
    (insert-char buf #\.)
    (insert-char buf #\0)
    (assert (string= (buffer-line buf 0) "version 1.0") () 
            "Test 3 failed: expected 'version 1.0', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 3 passed: Insert digits~%"))
  
  (format t "All insert-char special characters tests passed!~%~%"))

(defun test-insert-char-mark-clearing ()
  "Test that inserting characters clears the mark"
  (format t "Running insert-char mark clearing tests...~%")
  
  ;; Test 1: Mark is cleared after character insertion
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello" "world"))
    (buffer-set-point buf 0 2)
    (buffer-set-mark buf 0 4)  ; Set a mark
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (insert-char buf #\X)
    (assert (null (buffer-get-mark buf)) () 
            "Test 1 failed: mark should be cleared after insertion")
    (assert (string= (buffer-line buf 0) "heXllo") () 
            "Test 1 failed: expected 'heXllo', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Mark is cleared after character insertion~%"))
  
  ;; Test 2: Mark remains cleared after multiple insertions
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test"))
    (buffer-set-point buf 0 2)
    (buffer-set-mark buf 0 3)  ; Set a mark
    (insert-char buf #\1)
    (assert (null (buffer-get-mark buf)) () 
            "Test 2 failed: mark should be cleared after first insertion")
    (insert-char buf #\2)
    (assert (null (buffer-get-mark buf)) () 
            "Test 2 failed: mark should remain cleared after second insertion")
    (assert (string= (buffer-line buf 0) "te12st") () 
            "Test 2 failed: expected 'te12st', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Mark remains cleared after multiple insertions~%"))
  
  (format t "All insert-char mark clearing tests passed!~%~%"))

(defun run-all-buffer-insertion-tests ()
  "Run all buffer character insertion tests"
  (format t "~%======================================~%")
  (format t "Running Buffer Character Insertion Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-insert-char-beginning)
        (test-insert-char-middle)
        (test-insert-char-end)
        (test-insert-char-special-characters)
        (test-insert-char-mark-clearing)
        (format t "~%======================================~%")
        (format t "All character insertion tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)