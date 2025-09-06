(in-package :tle)

(defun test-load-file-basic ()
  "Test basic load-file functionality"
  (format t "Running load-file basic tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-load-file-basic.txt"))
    
    ;; Create test file with content
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "hello world
test line two
third line here" stream))
    
    ;; Test loading file into buffer
    (let ((result (load-file buf test-file)))
      (assert result () "Test 1 failed: load-file should return t on success")
      
      ;; Verify buffer content matches file
      (assert (= (buffer-line-count buf) 3) ()
              "Test 1 failed: Buffer should have 3 lines")
      (assert (string= (buffer-line buf 0) "hello world") ()
              "Test 1 failed: First line should match")
      (assert (string= (buffer-line buf 1) "test line two") ()
              "Test 1 failed: Second line should match")
      (assert (string= (buffer-line buf 2) "third line here") ()
              "Test 1 failed: Third line should match")
      
      ;; Verify file path was set
      (assert (string= (buffer-file-path buf) test-file) ()
              "Test 1 failed: Buffer file path should be set")
      
      ;; Verify cursor is at beginning
      (let ((point (buffer-get-point buf)))
        (assert (and (= (first point) 0) (= (second point) 0)) ()
                "Test 1 failed: Cursor should be at beginning"))
      
      ;; Verify mark is cleared
      (assert (not (buffer-get-mark buf)) ()
              "Test 1 failed: Mark should be cleared")
      
      ;; Verify buffer is clean
      (assert (not (buffer-dirty-p buf)) ()
              "Test 1 failed: Buffer should be clean after loading")
      
      (format t "✓ Test 1 passed: Basic load-file functionality~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All load-file basic tests passed!~%~%"))

(defun test-load-file-empty ()
  "Test load-file with empty file"
  (format t "Running load-file empty file tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-load-file-empty.txt"))
    
    ;; Create empty test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "" stream))
    
    ;; Test loading empty file
    (let ((result (load-file buf test-file)))
      (assert result () "Test 1 failed: load-file should succeed with empty file")
      
      ;; Verify buffer has one empty line (minimum)
      (assert (= (buffer-line-count buf) 1) ()
              "Test 1 failed: Empty file should create buffer with one line")
      (assert (string= (buffer-line buf 0) "") ()
              "Test 1 failed: Single line should be empty")
      
      ;; Verify cursor is at beginning
      (let ((point (buffer-get-point buf)))
        (assert (and (= (first point) 0) (= (second point) 0)) ()
                "Test 1 failed: Cursor should be at beginning"))
      
      (format t "✓ Test 1 passed: Empty file loading~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All load-file empty file tests passed!~%~%"))

(defun test-load-file-single-line ()
  "Test load-file with single line file (no newline)"
  (format t "Running load-file single line tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-load-file-single.txt"))
    
    ;; Create single line file without newline
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "single line no newline" stream))
    
    ;; Test loading single line file
    (let ((result (load-file buf test-file)))
      (assert result () "Test 1 failed: load-file should succeed")
      
      ;; Verify buffer content
      (assert (= (buffer-line-count buf) 1) ()
              "Test 1 failed: Single line file should create one line buffer")
      (assert (string= (buffer-line buf 0) "single line no newline") ()
              "Test 1 failed: Line content should match")
      
      (format t "✓ Test 1 passed: Single line file loading~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All load-file single line tests passed!~%~%"))

(defun test-load-file-error-handling ()
  "Test load-file error handling"
  (format t "Running load-file error handling tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (nonexistent-file "/tmp/this-file-does-not-exist.txt"))
    
    ;; Make sure file doesn't exist
    (when (probe-file nonexistent-file)
      (delete-file nonexistent-file))
    
    ;; Test loading nonexistent file should fail gracefully
    (let ((result (load-file buf nonexistent-file)))
      (assert (not result) () 
              "Test 1 failed: load-file should return nil for nonexistent file")
      (format t "✓ Test 1 passed: Error handling for nonexistent file~%")))

  (format t "All load-file error handling tests passed!~%~%"))

(defun test-load-file-overwrite-buffer ()
  "Test load-file overwrites existing buffer content"
  (format t "Running load-file buffer overwrite tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-load-file-overwrite.txt"))
    
    ;; Setup buffer with existing content
    (setf (lines buf) (vector "old line 1" "old line 2" "old line 3"))
    (setf (buffer-file-path buf) "/old/path/file.txt")
    (buffer-set-point buf 1 5)
    (buffer-set-mark buf 2 3)
    
    ;; Create new test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "new content line 1
new content line 2" stream))
    
    ;; Test loading new file overwrites old content
    (let ((result (load-file buf test-file)))
      (assert result () "Test 1 failed: load-file should succeed")
      
      ;; Verify old content is replaced
      (assert (= (buffer-line-count buf) 2) ()
              "Test 1 failed: Buffer should have new line count")
      (assert (string= (buffer-line buf 0) "new content line 1") ()
              "Test 1 failed: First line should be replaced")
      (assert (string= (buffer-line buf 1) "new content line 2") ()
              "Test 1 failed: Second line should be replaced")
      
      ;; Verify file path updated
      (assert (string= (buffer-file-path buf) test-file) ()
              "Test 1 failed: File path should be updated")
      
      ;; Verify cursor reset to beginning
      (let ((point (buffer-get-point buf)))
        (assert (and (= (first point) 0) (= (second point) 0)) ()
                "Test 1 failed: Cursor should be reset to beginning"))
      
      ;; Verify mark cleared
      (assert (not (buffer-get-mark buf)) ()
              "Test 1 failed: Mark should be cleared")
      
      (format t "✓ Test 1 passed: Buffer content overwrite~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All load-file buffer overwrite tests passed!~%~%"))

(defun test-load-file-command ()
  "Test load-file-command function"
  (format t "Running load-file-command tests...~%")

  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer))
        (test-file "/tmp/test-load-file-command.txt"))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "command test line 1
command test line 2" stream))
    
    ;; Setup editor with buffer
    (setf (lines buffer) (vector "old content"))
    (setf (buffers editor) (list buffer))
    
    ;; Test load-file-command
    (load-file-command test-file editor)
    
    ;; Verify file was loaded into current buffer
    (let ((current-buf (current-buffer editor)))
      (assert (= (buffer-line-count current-buf) 2) ()
              "Test 1 failed: Buffer should have loaded content")
      (assert (string= (buffer-line current-buf 0) "command test line 1") ()
              "Test 1 failed: First line should match loaded content")
      (assert (string= (buffer-line current-buf 1) "command test line 2") ()
              "Test 1 failed: Second line should match loaded content"))
    
    (format t "✓ Test 1 passed: load-file-command functionality~%")
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All load-file-command tests passed!~%~%"))

(defun test-load-file-buffer-name-update ()
  "Test that load-file updates buffer name to filename"
  (format t "Running load-file buffer name update tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-filename.lisp"))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "test content" stream))
    
    ;; Set initial buffer name
    (setf (buffer-name buf) "old-name")
    
    ;; Load file and verify name is updated
    (let ((result (load-file buf test-file)))
      (assert result () "Test 1 failed: load-file should succeed")
      (assert (string= (buffer-name buf) "test-filename") ()
              "Test 1 failed: Buffer name should be updated to filename")
      
      (format t "✓ Test 1 passed: Buffer name update~%"))
    
    ;; Test with different file extension
    (let ((test-file2 "/tmp/another-test.txt"))
      (with-open-file (stream test-file2 :direction :output :if-exists :supersede)
        (write-string "more content" stream))
      
      (load-file buf test-file2)
      (assert (string= (buffer-name buf) "another-test") ()
              "Test 2 failed: Buffer name should update for different file")
      
      (format t "✓ Test 2 passed: Buffer name update with different extension~%")
      
      (when (probe-file test-file2)
        (delete-file test-file2)))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All load-file buffer name update tests passed!~%~%"))

(defun run-all-load-file-tests ()
  "Run all load-file tests"
  (format t "~%======================================~%")
  (format t "Running Load File Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-load-file-basic)
        (test-load-file-empty)
        (test-load-file-single-line)
        (test-load-file-error-handling)
        (test-load-file-overwrite-buffer)
        (test-load-file-command)
        (test-load-file-buffer-name-update)
        (format t "~%======================================~%")
        (format t "All load-file tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: load-file test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)