(in-package :tle)

(defun test-ctrl-space-mark-setting ()
  "Test that Ctrl+Space sets mark at current cursor position"
  (format t "Running Ctrl+Space mark setting tests...~%")
  
  ;; Test 1: Set mark at beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world" "second line"))
    (buffer-set-point buf 0 0)
    ;; Simulate Ctrl+Space by setting mark at current point
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    (let ((mark (buffer-get-mark buf))
          (point (buffer-get-point buf)))
      (assert (equal mark '(0 0)) () 
              "Test 1 failed: expected mark at (0 0), got ~A" mark)
      (assert (equal point '(0 0)) () 
              "Test 1 failed: expected point at (0 0), got ~A" point))
    (format t "✓ Test 1 passed: Set mark at beginning of line~%"))
  
  ;; Test 2: Set mark in middle of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)
    ;; Simulate Ctrl+Space
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    (let ((mark (buffer-get-mark buf))
          (point (buffer-get-point buf)))
      (assert (equal mark '(0 5)) () 
              "Test 2 failed: expected mark at (0 5), got ~A" mark)
      (assert (equal point '(0 5)) () 
              "Test 2 failed: expected point at (0 5), got ~A" point))
    (format t "✓ Test 2 passed: Set mark in middle of line~%"))
  
  ;; Test 3: Set mark, move cursor, verify selection
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 5)
    ;; Set mark at position 5
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    ;; Move cursor to position 8
    (buffer-set-point buf 0 8)
    (let ((mark (buffer-get-mark buf))
          (point (buffer-get-point buf)))
      (assert (equal mark '(0 5)) () 
              "Test 3 failed: expected mark at (0 5), got ~A" mark)
      (assert (equal point '(0 8)) () 
              "Test 3 failed: expected point at (0 8), got ~A" point))
    (format t "✓ Test 3 passed: Set mark, move cursor, verify selection~%"))
  
  ;; Test 4: Set mark on different line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line" "third line"))
    (buffer-set-point buf 1 7)  ; On second line, position 7
    ;; Set mark
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    (let ((mark (buffer-get-mark buf))
          (point (buffer-get-point buf)))
      (assert (equal mark '(1 7)) () 
              "Test 4 failed: expected mark at (1 7), got ~A" mark)
      (assert (equal point '(1 7)) () 
              "Test 4 failed: expected point at (1 7), got ~A" point))
    (format t "✓ Test 4 passed: Set mark on different line~%"))
  
  ;; Test 5: Overwrite existing mark
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("testing"))
    (buffer-set-point buf 0 2)
    ;; Set initial mark
    (buffer-set-mark buf 0 4)
    (assert (equal (buffer-get-mark buf) '(0 4)) () 
            "Test 5 setup failed: initial mark should be at (0 4)")
    ;; Set new mark at current position
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    (let ((mark (buffer-get-mark buf)))
      (assert (equal mark '(0 2)) () 
              "Test 5 failed: expected mark at (0 2), got ~A" mark))
    (format t "✓ Test 5 passed: Overwrite existing mark~%"))
  
  (format t "All Ctrl+Space mark setting tests passed!~%~%"))

(defun test-mark-and-insertion-interaction ()
  "Test interaction between mark setting and insertion operations"
  (format t "Running mark and insertion interaction tests...~%")
  
  ;; Test 1: Set mark, then insert character (should clear mark)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello"))
    (buffer-set-point buf 0 3)
    ;; Set mark
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    ;; Insert character (should clear mark)
    (insert-char buf #\X)
    (assert (null (buffer-get-mark buf)) () 
            "Test 1 failed: mark should be cleared after character insertion")
    (assert (string= (buffer-line buf 0) "helXlo") () 
            "Test 1 failed: expected 'helXlo', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Mark cleared after character insertion~%"))
  
  ;; Test 2: Set mark, then insert newline (should clear mark)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello"))
    (buffer-set-point buf 0 3)
    ;; Set mark
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    (assert (buffer-get-mark buf) () "Test 2 setup failed: mark should be set")
    ;; Insert newline (should clear mark)
    (insert-newline buf)
    (assert (null (buffer-get-mark buf)) () 
            "Test 2 failed: mark should be cleared after newline insertion")
    (assert (= (buffer-line-count buf) 2) () 
            "Test 2 failed: expected 2 lines, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "hel") () 
            "Test 2 failed: expected 'hel' on first line, got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "lo") () 
            "Test 2 failed: expected 'lo' on second line, got '~A'" (buffer-line buf 1))
    (format t "✓ Test 2 passed: Mark cleared after newline insertion~%"))
  
  ;; Test 3: Set mark, move cursor, set new mark (should overwrite)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 3)
    ;; Set first mark
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    ;; Move cursor
    (buffer-set-point buf 0 7)
    ;; Set new mark at new position
    (let ((point (buffer-get-point buf)))
      (buffer-set-mark buf (first point) (second point)))
    (let ((mark (buffer-get-mark buf)))
      (assert (equal mark '(0 7)) () 
              "Test 3 failed: expected mark at (0 7), got ~A" mark))
    (format t "✓ Test 3 passed: New mark overwrites old mark~%"))
  
  (format t "All mark and insertion interaction tests passed!~%~%"))

(defun run-all-mark-setting-tests ()
  "Run all mark setting tests"
  (format t "~%======================================~%")
  (format t "Running Mark Setting Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-ctrl-space-mark-setting)
        (test-mark-and-insertion-interaction)
        (format t "~%======================================~%")
        (format t "All mark setting tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)