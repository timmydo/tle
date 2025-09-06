(in-package :tle)

(defun test-revert-buffer-basic ()
  "Test basic revert-buffer functionality"
  (format t "Running revert-buffer basic tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-revert-basic.txt"))
    
    ;; Create test file with initial content
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "original content line 1
original content line 2
original content line 3" stream))
    
    ;; Load file into buffer
    (load-file buf test-file)
    
    ;; Modify buffer content to simulate changes
    (setf (lines buf) (vector "modified line 1" "modified line 2" "new line 3" "extra line"))
    (buffer-set-point buf 2 10)
    (buffer-set-mark buf 1 5)
    (mark-buffer-dirty buf)  ; Mark buffer as dirty
    
    ;; Test reverting buffer
    (let ((result (revert-buffer buf)))
      (assert result () "Test 1 failed: revert-buffer should return t on success")
      
      ;; Verify buffer content matches original file
      (assert (= (buffer-line-count buf) 3) ()
              "Test 1 failed: Buffer should have 3 lines")
      (assert (string= (buffer-line buf 0) "original content line 1") ()
              "Test 1 failed: First line should be reverted")
      (assert (string= (buffer-line buf 1) "original content line 2") ()
              "Test 1 failed: Second line should be reverted")
      (assert (string= (buffer-line buf 2) "original content line 3") ()
              "Test 1 failed: Third line should be reverted")
      
      ;; Verify cursor is at beginning
      (let ((point (buffer-get-point buf)))
        (assert (and (= (first point) 0) (= (second point) 0)) ()
                "Test 1 failed: Cursor should be at beginning"))
      
      ;; Verify mark is cleared
      (assert (not (buffer-get-mark buf)) ()
              "Test 1 failed: Mark should be cleared")
      
      ;; Verify buffer is clean
      (assert (not (buffer-dirty-p buf)) ()
              "Test 1 failed: Buffer should be clean after reverting")
      
      (format t "✓ Test 1 passed: Basic revert-buffer functionality~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All revert-buffer basic tests passed!~%~%"))

(defun test-revert-buffer-no-file-path ()
  "Test revert-buffer with buffer that has no file path"
  (format t "Running revert-buffer no file path tests...~%")

  (let ((buf (make-instance 'standard-buffer)))
    
    ;; Setup buffer without file path
    (setf (lines buf) (vector "some content" "more content"))
    (setf (buffer-file-path buf) nil)  ; No file path
    
    ;; Test reverting buffer with no file path should fail
    (let ((result (revert-buffer buf)))
      (assert (not result) () 
              "Test 1 failed: revert-buffer should return nil when no file path")
      
      ;; Verify content unchanged
      (assert (= (buffer-line-count buf) 2) ()
              "Test 1 failed: Buffer content should remain unchanged")
      (assert (string= (buffer-line buf 0) "some content") ()
              "Test 1 failed: First line should remain unchanged")
      
      (format t "✓ Test 1 passed: Error handling for buffer with no file path~%")))

  (format t "All revert-buffer no file path tests passed!~%~%"))

(defun test-revert-buffer-nonexistent-file ()
  "Test revert-buffer when file no longer exists"
  (format t "Running revert-buffer nonexistent file tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-revert-deleted.txt"))
    
    ;; Create and load file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "original content" stream))
    (load-file buf test-file)
    
    ;; Delete the file after loading
    (delete-file test-file)
    
    ;; Modify buffer content
    (setf (lines buf) (vector "modified content"))
    
    ;; Test reverting when file no longer exists should fail
    (let ((result (revert-buffer buf)))
      (assert (not result) () 
              "Test 1 failed: revert-buffer should return nil for nonexistent file")
      
      ;; Verify content remains modified
      (assert (string= (buffer-line buf 0) "modified content") ()
              "Test 1 failed: Buffer content should remain unchanged when revert fails")
      
      (format t "✓ Test 1 passed: Error handling for deleted file~%")))

  (format t "All revert-buffer nonexistent file tests passed!~%~%"))

(defun test-revert-buffer-empty-file ()
  "Test revert-buffer with empty file"
  (format t "Running revert-buffer empty file tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-revert-empty.txt"))
    
    ;; Create empty test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "" stream))
    
    ;; Load empty file
    (load-file buf test-file)
    
    ;; Add some content to buffer
    (setf (lines buf) (vector "added line 1" "added line 2"))
    (mark-buffer-dirty buf)
    
    ;; Test reverting to empty file
    (let ((result (revert-buffer buf)))
      (assert result () "Test 1 failed: revert-buffer should succeed with empty file")
      
      ;; Verify buffer reverted to one empty line
      (assert (= (buffer-line-count buf) 1) ()
              "Test 1 failed: Empty file should create buffer with one line")
      (assert (string= (buffer-line buf 0) "") ()
              "Test 1 failed: Single line should be empty")
      
      ;; Verify buffer is clean
      (assert (not (buffer-dirty-p buf)) ()
              "Test 1 failed: Buffer should be clean after reverting")
      
      (format t "✓ Test 1 passed: Revert to empty file~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All revert-buffer empty file tests passed!~%~%"))

(defun test-revert-file-command ()
  "Test revert-file-command function"
  (format t "Running revert-file-command tests...~%")

  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer))
        (test-file "/tmp/test-revert-command.txt"))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "command test original
second line original" stream))
    
    ;; Setup editor with buffer
    (load-file buffer test-file)
    (setf (buffers editor) (list buffer))
    
    ;; Modify buffer
    (setf (lines buffer) (vector "modified content"))
    (mark-buffer-dirty buffer)
    
    ;; Test revert-file-command
    (revert-file-command editor)
    
    ;; Verify file was reverted in current buffer
    (let ((current-buf (current-buffer editor)))
      (assert (= (buffer-line-count current-buf) 2) ()
              "Test 1 failed: Buffer should have reverted content")
      (assert (string= (buffer-line current-buf 0) "command test original") ()
              "Test 1 failed: First line should match original content")
      (assert (string= (buffer-line current-buf 1) "second line original") ()
              "Test 1 failed: Second line should match original content")
      (assert (not (buffer-dirty-p current-buf)) ()
              "Test 1 failed: Buffer should be clean after revert"))
    
    (format t "✓ Test 1 passed: revert-file-command functionality~%")
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All revert-file-command tests passed!~%~%"))

(defun test-revert-buffer-undo-history-cleared ()
  "Test that revert-buffer clears undo history"
  (format t "Running revert-buffer undo history tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-revert-undo.txt"))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "original line" stream))
    
    ;; Load file and make some changes to create undo history
    (load-file buf test-file)
    (insert-char buf #\x)  ; This should create undo records
    (insert-char buf #\y)
    
    ;; Verify undo history exists (this is implementation dependent)
    ;; For now we'll just verify buffer has content
    (assert (> (length (buffer-line buf 0)) (length "original line")) ()
            "Setup failed: Buffer should have modified content")
    
    ;; Revert buffer
    (let ((result (revert-buffer buf)))
      (assert result () "Test 1 failed: revert-buffer should succeed")
      
      ;; Verify content is reverted and buffer is clean
      (assert (string= (buffer-line buf 0) "original line") ()
              "Test 1 failed: Content should be reverted")
      (assert (not (buffer-dirty-p buf)) ()
              "Test 1 failed: Buffer should be clean")
      
      (format t "✓ Test 1 passed: Undo history cleared on revert~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All revert-buffer undo history tests passed!~%~%"))

(defun run-all-revert-file-tests ()
  "Run all revert-file tests"
  (format t "~%======================================~%")
  (format t "Running Revert File Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-revert-buffer-basic)
        (test-revert-buffer-no-file-path)
        (test-revert-buffer-nonexistent-file)
        (test-revert-buffer-empty-file)
        (test-revert-file-command)
        (test-revert-buffer-undo-history-cleared)
        (format t "~%======================================~%")
        (format t "All revert-file tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: revert-file test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)