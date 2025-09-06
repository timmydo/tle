(in-package :tle)

(defun test-query-replace-undo-bug ()
  "Test that undo works correctly after query-replace operations"
  (format t "Running query-replace undo bug test...~%")
  
  ;; Test 1: Batch query-replace (should work)
  (format t "  Testing batch query-replace undo...~%")
  (let ((buf (make-instance 'standard-buffer)))
    ;; Setup: Create buffer with some replaceable text
    (setf (lines buf) (vector "hello world test" "hello again test"))
    (buffer-set-point buf 0 0)
    
    ;; Store original content for comparison
    (let ((original-content (copy-seq (lines buf)))
          (original-point (buffer-get-point buf)))
      
      ;; Perform query-replace operation 
      (format t "    Original content: ~A~%" (coerce (lines buf) 'list))
      (let ((result (query-replace buf "hello" "hi")))
        (format t "    After replacement: ~A~%" (coerce (lines buf) 'list))
        (format t "    Replacements made: ~A~%" result)
        (assert (= result 2) () "Setup failed: should replace 2 occurrences")
        (assert (string= (buffer-line buf 0) "hi world test") () 
                "Setup failed: first line should be 'hi world test'")
        (assert (string= (buffer-line buf 1) "hi again test") ()
                "Setup failed: second line should be 'hi again test'"))
      
      ;; Now test undo - this should work for batch mode
      (format t "    Testing undo...~%")
      (let ((undo-result (buffer-undo buf)))
        (format t "    Undo returned: ~A~%" undo-result)
        (format t "    Content after undo: ~A~%" (coerce (lines buf) 'list))
        (format t "    Point after undo: ~A~%" (buffer-get-point buf))
        
        ;; The test: undo should restore original content
        (assert undo-result () "Batch mode: undo returned nil instead of t")
        
        ;; Check that content was restored
        (assert (= (length (lines buf)) (length original-content)) ()
                "Batch mode: line count mismatch after undo")
        (loop for i from 0 below (length original-content) do
              (assert (string= (buffer-line buf i) (aref original-content i)) ()
                      "Batch mode: line ~A not restored correctly. Expected '~A', got '~A'" 
                      i (aref original-content i) (buffer-line buf i)))
        
        ;; Check that point was restored
        (assert (equal (buffer-get-point buf) original-point) ()
                "Batch mode: point not restored correctly. Expected ~A, got ~A" 
                original-point (buffer-get-point buf))
        
        (format t "    ✓ Batch mode test passed~%"))))
  
  ;; Test 2: Interactive query-replace using the actual interactive system
  (format t "  Testing interactive query-replace undo behavior...~%")
  (let ((editor (make-standard-editor))
        (buf (make-standard-buffer "*test*")))
    ;; Setup: Create buffer with replaceable text  
    (setf (lines buf) (vector "hello world hello"))
    (buffer-set-point buf 0 0)
    (setf (buffers editor) (list buf))
    
    ;; Store original content
    (let ((original-content (copy-seq (lines buf)))
          (original-point (buffer-get-point buf)))
      (format t "    Original content: ~A~%" (coerce (lines buf) 'list))
      
      ;; Start interactive query-replace session
      (start-interactive-query-replace editor "hello" "hi")
      
      ;; Simulate replacing all matches by performing individual replacements
      ;; This mimics the user pressing 'y' for each match
      (let ((matches (query-replace-matches editor)))
        (format t "    Found ~A matches~%" (length matches))
        (loop for i from 0 below (length matches) do
              (perform-current-replacement editor)
              (setf (query-replace-current-match editor) (1+ i))))
      
      ;; Finish the session (this should create the single undo record)
      (finish-query-replace editor)
      
      (format t "    After interactive replacements: ~A~%" (coerce (lines buf) 'list))
      (assert (string= (buffer-line buf 0) "hi world hi") ()
              "Setup failed: should be 'hi world hi'")
      
      ;; Now test undo behavior - this should work with just one undo!
      (format t "    Testing single undo (should restore everything)...~%")
      (let ((undo-result (buffer-undo buf)))
        (format t "    Undo returned: ~A~%" undo-result)
        (format t "    Content after undo: ~A~%" (coerce (lines buf) 'list))
        (format t "    Point after undo: ~A~%" (buffer-get-point buf))
        
        ;; The fix test: undo should restore original content with single undo
        (assert undo-result () "Interactive mode: undo returned nil instead of t")
        
        ;; Check that content was restored
        (assert (= (length (lines buf)) (length original-content)) ()
                "Interactive mode: line count mismatch after undo")
        (loop for i from 0 below (length original-content) do
              (assert (string= (buffer-line buf i) (aref original-content i)) ()
                      "Interactive mode: line ~A not restored correctly. Expected '~A', got '~A'" 
                      i (aref original-content i) (buffer-line buf i)))
        
        ;; Check that point was restored
        (assert (equal (buffer-get-point buf) original-point) ()
                "Interactive mode: point not restored correctly. Expected ~A, got ~A" 
                original-point (buffer-get-point buf))
        
        (format t "    ✓ Interactive mode test passed: Single undo restores everything~%"))))
    
    ;; Show the fix
    (format t "~%  FIX IMPLEMENTED:~%")
    (format t "  - Both batch and interactive query-replace now use 1 undo to restore everything~%")
    (format t "  - Interactive mode disables individual undo recording during the session~%")
    (format t "  - A single comprehensive undo record is created when the session finishes~%"))
  
  (format t "Query-replace undo bug test completed!~%~%"))

(defun run-query-replace-undo-test ()
  "Run the query-replace undo bug test"
  (format t "~%======================================~%")
  (format t "Running Query-Replace Undo Bug Test~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-query-replace-undo-bug)
        (format t "~%======================================~%")
        (format t "Query-replace undo test passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "This confirms the bug exists.~%")
      (format t "======================================~%")
      (return-from run-query-replace-undo-test nil)))
  
  t)

(defun run-all-query-replace-undo-bug-tests ()
  "Run all query-replace undo bug tests"
  (run-query-replace-undo-test))