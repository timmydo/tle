(in-package :tle)

(defun test-save-buffer-as-basic ()
  "Test basic save-buffer-as functionality"
  (format t "Running save-buffer-as basic tests...~%")
  
  ;; Test 1: Save buffer to new file
  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-save-buffer-as-1.txt"))
    (setf (lines buf) (vector "line 1" "line 2" "line 3"))
    (setf (buffer-name buf) "test-buffer")
    
    ;; Clean up any existing test file
    (when (probe-file test-file)
      (delete-file test-file))
    
    ;; Save the buffer
    (let ((result (save-buffer-as buf test-file)))
      (assert result () "Test 1 failed: save-buffer-as should return t on success")
      
      ;; Check that file was created and has correct content
      (assert (probe-file test-file) () "Test 1 failed: file should exist after save")
      
      ;; Read file content and verify
      (with-open-file (stream test-file :direction :input)
        (let ((content (read-file-to-string stream)))
          (assert (string= content (format nil "line 1~%line 2~%line 3")) ()
                  "Test 1 failed: file content should match buffer content, got: '~A'" content)))
      
      ;; Check that buffer's file-path was updated
      (assert (string= (buffer-file-path buf) test-file) ()
              "Test 1 failed: buffer file-path should be updated to ~A, got ~A" 
              test-file (buffer-file-path buf))
      
      ;; Clean up
      (delete-file test-file))
    
    (format t "✓ Test 1 passed: Basic save-buffer-as functionality~%"))
  
  (format t "All save-buffer-as basic tests passed!~%~%"))

(defun test-save-buffer-as-empty-buffer ()
  "Test save-buffer-as with empty buffer"
  (format t "Running save-buffer-as empty buffer tests...~%")
  
  ;; Test 1: Save empty buffer
  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-save-buffer-as-empty.txt"))
    (setf (lines buf) (vector ""))
    
    ;; Clean up any existing test file
    (when (probe-file test-file)
      (delete-file test-file))
    
    (let ((result (save-buffer-as buf test-file)))
      (assert result () "Test 1 failed: save-buffer-as should succeed with empty buffer")
      (assert (probe-file test-file) () "Test 1 failed: file should be created for empty buffer")
      
      ;; Read file and verify it contains empty content
      (with-open-file (stream test-file :direction :input)
        (let ((content (read-file-to-string stream)))
          (assert (string= content "") ()
                  "Test 1 failed: empty buffer should create empty file, got: '~A'" content)))
      
      ;; Clean up
      (delete-file test-file))
    
    (format t "✓ Test 1 passed: Save empty buffer~%"))
  
  (format t "All save-buffer-as empty buffer tests passed!~%~%"))

(defun test-save-buffer-as-overwrite ()
  "Test save-buffer-as file overwriting behavior"
  (format t "Running save-buffer-as overwrite tests...~%")
  
  ;; Test 1: Overwrite existing file
  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-save-buffer-as-overwrite.txt"))
    (setf (lines buf) (vector "new content" "second line"))
    
    ;; Create an existing file with different content
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string "old content" stream))
    
    (let ((result (save-buffer-as buf test-file)))
      (assert result () "Test 1 failed: save-buffer-as should succeed when overwriting")
      
      ;; Verify the file was overwritten with new content
      (with-open-file (stream test-file :direction :input)
        (let ((content (read-file-to-string stream)))
          (assert (string= content (format nil "new content~%second line")) ()
                  "Test 1 failed: file should be overwritten with buffer content, got: '~A'" content)))
      
      ;; Clean up
      (delete-file test-file))
    
    (format t "✓ Test 1 passed: Overwrite existing file~%"))
  
  (format t "All save-buffer-as overwrite tests passed!~%~%"))

(defun test-save-buffer-as-file-path-update ()
  "Test that save-buffer-as updates buffer file-path correctly"
  (format t "Running save-buffer-as file-path update tests...~%")
  
  ;; Test 1: File-path update from nil
  (let ((buf (make-instance 'standard-buffer))
        (test-file "/tmp/test-save-buffer-as-path1.txt"))
    (setf (lines buf) (vector "test content"))
    (setf (buffer-file-path buf) nil)
    
    ;; Clean up any existing test file
    (when (probe-file test-file)
      (delete-file test-file))
    
    (save-buffer-as buf test-file)
    (assert (string= (buffer-file-path buf) test-file) ()
            "Test 1 failed: file-path should be set to ~A, got ~A" 
            test-file (buffer-file-path buf))
    
    ;; Clean up
    (delete-file test-file)
    (format t "✓ Test 1 passed: File-path updated from nil~%"))
  
  ;; Test 2: File-path update from existing path
  (let ((buf (make-instance 'standard-buffer))
        (old-file "/tmp/test-save-buffer-as-old.txt")
        (new-file "/tmp/test-save-buffer-as-new.txt"))
    (setf (lines buf) (vector "test content"))
    (setf (buffer-file-path buf) old-file)
    
    ;; Clean up any existing test files
    (when (probe-file old-file) (delete-file old-file))
    (when (probe-file new-file) (delete-file new-file))
    
    (save-buffer-as buf new-file)
    (assert (string= (buffer-file-path buf) new-file) ()
            "Test 2 failed: file-path should be updated to ~A, got ~A" 
            new-file (buffer-file-path buf))
    
    ;; Clean up
    (when (probe-file new-file) (delete-file new-file))
    (format t "✓ Test 2 passed: File-path updated from existing path~%"))
  
  (format t "All save-buffer-as file-path update tests passed!~%~%"))

(defun test-save-buffer-as-error-handling ()
  "Test save-buffer-as error handling"
  (format t "Running save-buffer-as error handling tests...~%")
  
  ;; Test 1: Invalid directory path
  (let ((buf (make-instance 'standard-buffer))
        (invalid-file "/nonexistent/directory/test.txt"))
    (setf (lines buf) (vector "test content"))
    (setf (buffer-file-path buf) nil)
    
    (let ((result (save-buffer-as buf invalid-file)))
      (assert (null result) () 
              "Test 1 failed: save-buffer-as should return nil for invalid path")
      (assert (null (buffer-file-path buf)) ()
              "Test 1 failed: buffer file-path should remain nil on error"))
    
    (format t "✓ Test 1 passed: Invalid directory path handled~%"))
  
  (format t "All save-buffer-as error handling tests passed!~%~%"))

(defun read-file-to-string (stream)
  "Helper function to read entire stream content into a string"
  (with-output-to-string (output)
    (loop for line = (read-line stream nil nil)
          while line
          do (write-string line output)
             (when (peek-char nil stream nil nil)
               (terpri output)))))

(defun run-all-save-buffer-as-tests ()
  "Run all save-buffer-as tests"
  (format t "~%======================================~%")
  (format t "Running Save Buffer As Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-save-buffer-as-basic)
        (test-save-buffer-as-empty-buffer)
        (test-save-buffer-as-overwrite)
        (test-save-buffer-as-file-path-update)
        (test-save-buffer-as-error-handling)
        (format t "~%======================================~%")
        (format t "All save-buffer-as tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)