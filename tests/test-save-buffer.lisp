(in-package :tle)

(defun test-save-buffer-basic ()
  "Test basic save-buffer functionality"
  (format t "Running save-buffer basic tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-save-buffer-1.txt"))
    
    ;; Cleanup previous test file
    (when (probe-file test-file)
      (delete-file test-file))
    
    ;; Setup buffer with file path
    (setf (lines buf) (vector "hello world" "test line two"))
    (setf (buffer-file-path buf) test-file)
    (buffer-set-point buf 0 0)
    
    ;; Test saving buffer
    (let ((result (save-buffer buf)))
      (assert result () "Test 1 failed: save-buffer should return t on success")
      
      ;; Verify file was created and has correct content
      (assert (probe-file test-file) () "Test 1 failed: File should exist after saving")
      
      (with-open-file (stream test-file :direction :input)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          (assert (string= content "hello world
test line two") ()
                  "Test 1 failed: File content should match buffer content")))
      
      (format t "✓ Test 1 passed: Basic save-buffer functionality~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All save-buffer basic tests passed!~%~%"))

(defun test-save-buffer-no-file-path ()
  "Test save-buffer with no file path"
  (format t "Running save-buffer no file path tests...~%")

  (let ((buf (make-instance 'standard-buffer)))
    
    ;; Setup buffer without file path
    (setf (lines buf) (vector "hello world" "test line two"))
    (setf (buffer-file-path buf) nil)
    (buffer-set-point buf 0 0)
    
    ;; Test saving buffer should fail gracefully
    (let ((result (save-buffer buf)))
      (assert (not result) () "Test 1 failed: save-buffer should return nil when no file path")
      (format t "✓ Test 1 passed: save-buffer handles nil file path~%")))

  (format t "All save-buffer no file path tests passed!~%~%"))

(defun test-save-buffer-empty-buffer ()
  "Test save-buffer with empty buffer"
  (format t "Running save-buffer empty buffer tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-save-buffer-empty.txt"))
    
    ;; Cleanup previous test file
    (when (probe-file test-file)
      (delete-file test-file))
    
    ;; Setup empty buffer with file path
    (setf (lines buf) (vector ))
    (setf (buffer-file-path buf) test-file)
    (buffer-set-point buf 0 0)
    
    ;; Test saving empty buffer
    (let ((result (save-buffer buf)))
      (assert result () "Test 1 failed: save-buffer should succeed with empty buffer")
      
      ;; Verify empty file was created
      (assert (probe-file test-file) () "Test 1 failed: Empty file should be created")
      
      (with-open-file (stream test-file :direction :input)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          (assert (string= content "") ()
                  "Test 1 failed: Empty buffer should create empty file")))
      
      (format t "✓ Test 1 passed: Empty buffer save~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All save-buffer empty buffer tests passed!~%~%"))

(defun test-save-buffer-overwrite ()
  "Test save-buffer file overwriting behavior"
  (format t "Running save-buffer overwrite tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-save-buffer-overwrite.txt"))
    
    ;; Create initial file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "old content" stream))
    
    ;; Setup buffer with new content
    (setf (lines buf) (vector "new content line 1" "new content line 2"))
    (setf (buffer-file-path buf) test-file)
    (buffer-set-point buf 0 0)
    
    ;; Test overwriting existing file
    (let ((result (save-buffer buf)))
      (assert result () "Test 1 failed: save-buffer should succeed when overwriting")
      
      ;; Verify file was overwritten with new content
      (with-open-file (stream test-file :direction :input)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          (assert (string= content "new content line 1
new content line 2") ()
                  "Test 1 failed: File should be overwritten with new content")))
      
      (format t "✓ Test 1 passed: File overwriting~%"))
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All save-buffer overwrite tests passed!~%~%"))

(defun test-save-buffer-error-handling ()
  "Test save-buffer error handling"
  (format t "Running save-buffer error handling tests...~%")

  (let ((buf (make-instance 'standard-buffer))
        (invalid-file "/invalid/path/that/does/not/exist/file.txt"))
    
    ;; Setup buffer with invalid file path
    (setf (lines buf) (vector "test content"))
    (setf (buffer-file-path buf) invalid-file)
    (buffer-set-point buf 0 0)
    
    ;; Test saving to invalid path should fail gracefully
    (let ((result (save-buffer buf)))
      (assert (not result) () 
              "Test 1 failed: save-buffer should return nil for invalid path")
      (format t "✓ Test 1 passed: Error handling for invalid path~%")))

  (format t "All save-buffer error handling tests passed!~%~%"))

(defun test-save-buffer-command ()
  "Test save-buffer-command function"
  (format t "Running save-buffer-command tests...~%")

  (let ((editor (make-instance 'standard-editor))
        (buffer (make-instance 'standard-buffer))
        (test-file "/tmp/test-save-buffer-command.txt"))
    
    ;; Cleanup previous test file
    (when (probe-file test-file)
      (delete-file test-file))
    
    ;; Setup editor with buffer
    (setf (lines buffer) (vector "hello world" "save command test"))
    (setf (buffer-file-path buffer) test-file)
    (setf (buffers editor) (list buffer))
    (buffer-set-point buffer 0 0)
    
    ;; Test save-buffer-command
    (save-buffer-command editor)
    
    ;; Verify file was saved
    (assert (probe-file test-file) () "Test 1 failed: File should exist after command")
    
    (with-open-file (stream test-file :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (assert (string= content "hello world
save command test") ()
                "Test 1 failed: Command should save buffer content correctly")))
    
    (format t "✓ Test 1 passed: save-buffer-command functionality~%")
    
    ;; Cleanup
    (when (probe-file test-file)
      (delete-file test-file)))

  (format t "All save-buffer-command tests passed!~%~%"))

(defun run-all-save-buffer-tests ()
  "Run all save-buffer tests"
  (format t "~%======================================~%")
  (format t "Running Save Buffer Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-save-buffer-basic)
        (test-save-buffer-no-file-path)
        (test-save-buffer-empty-buffer)
        (test-save-buffer-overwrite)
        (test-save-buffer-error-handling)
        (test-save-buffer-command)
        (format t "~%======================================~%")
        (format t "All save-buffer tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: save-buffer test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)