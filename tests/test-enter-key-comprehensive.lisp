;;;; test-enter-key-comprehensive.lisp
;;;; Comprehensive test for Enter key behavior in query-replace

(ql:quickload :tle)

(in-package :tle)

(defun test-enter-key-behavior ()
  "Test that Enter key behavior matches 'y' behavior"
  (format t "=== Testing Enter Key Behavior ===~%")
  
  (let ((passed 0) (failed 0))
    
    ;; Test 1: Enter key should perform replacement (not quit)
    (format t "Test 1: Enter key performs replacement...~%")
    (let* ((editor (make-standard-editor))
           (buffer (current-buffer editor)))
      
      ;; Setup buffer content manually
      (setf (lines buffer) (list "hello world hello universe"))
      (buffer-set-point buffer 0 0)
      
      ;; Setup query-replace state
      (setf (query-replace-active-p editor) t)
      (setf (query-replace-from-string editor) "hello")
      (setf (query-replace-to-string editor) "hi")
      (setf (query-replace-matches editor) '((0 0) (0 12)))
      (setf (query-replace-current-match editor) 0)
      (setf (query-replace-replacements editor) '())
      (setf (buffers editor) (list buffer))
      
      ;; Test Enter action
      (query-replace-action-callback (string #\Return) editor)
      
      ;; Verify replacement occurred
      (let ((line-content (buffer-line buffer 0)))
        (if (search "hi" line-content)
            (progn 
              (format t "  ✓ PASS: Replacement occurred~%")
              (incf passed))
            (progn
              (format t "  ✗ FAIL: No replacement occurred. Content: '~A'~%" line-content)
              (incf failed))))
      
      ;; Verify query-replace is still active
      (if (query-replace-active-p editor)
          (progn
            (format t "  ✓ PASS: Query-replace still active~%")
            (incf passed))
          (progn
            (format t "  ✗ FAIL: Query-replace quit (should still be active)~%")
            (incf failed))))
    
    ;; Test 2: 'y' key behavior for comparison
    (format t "~%Test 2: 'y' key performs replacement (for comparison)...~%")
    (let* ((editor (make-standard-editor))
           (buffer (current-buffer editor)))
      
      ;; Setup buffer content manually
      (setf (lines buffer) (list "hello world hello universe"))
      (buffer-set-point buffer 0 0)
      
      ;; Setup query-replace state
      (setf (query-replace-active-p editor) t)
      (setf (query-replace-from-string editor) "hello")
      (setf (query-replace-to-string editor) "hi")
      (setf (query-replace-matches editor) '((0 0) (0 12)))
      (setf (query-replace-current-match editor) 0)
      (setf (query-replace-replacements editor) '())
      (setf (buffers editor) (list buffer))
      
      ;; Test 'y' action
      (query-replace-action-callback "y" editor)
      
      ;; Verify replacement occurred
      (let ((line-content (buffer-line buffer 0)))
        (if (search "hi" line-content)
            (progn 
              (format t "  ✓ PASS: 'y' replacement occurred~%")
              (incf passed))
            (progn
              (format t "  ✗ FAIL: 'y' replacement failed. Content: '~A'~%" line-content)
              (incf failed))))
      
      ;; Verify query-replace is still active
      (if (query-replace-active-p editor)
          (progn
            (format t "  ✓ PASS: Query-replace still active after 'y'~%")
            (incf passed))
          (progn
            (format t "  ✗ FAIL: Query-replace quit after 'y' (should still be active)~%")
            (incf failed))))
    
    ;; Test 3: 'q' key should quit (verify quit still works)
    (format t "~%Test 3: 'q' key quits query-replace...~%")
    (let* ((editor (make-standard-editor))
           (buffer (current-buffer editor)))
      
      ;; Setup query-replace state
      (setf (query-replace-active-p editor) t)
      (setf (query-replace-from-string editor) "hello")
      (setf (query-replace-to-string editor) "hi")
      (setf (buffers editor) (list buffer))
      
      ;; Test 'q' action
      (query-replace-action-callback "q" editor)
      
      ;; Verify query-replace quit
      (if (not (query-replace-active-p editor))
          (progn
            (format t "  ✓ PASS: 'q' quit query-replace correctly~%")
            (incf passed))
          (progn
            (format t "  ✗ FAIL: 'q' did not quit query-replace~%")
            (incf failed))))
    
    ;; Summary
    (format t "~%=== Test Results ===~%")
    (format t "Passed: ~A~%" passed)
    (format t "Failed: ~A~%" failed)
    (format t "Total:  ~A~%~%" (+ passed failed))
    
    (if (= failed 0)
        (format t "🎉 ALL TESTS PASSED! Enter key fix is working correctly.~%")
        (format t "❌ Some tests failed. The fix may need more work.~%"))
    
    (= failed 0))) ; Return t if all tests passed

;; Run the test
(test-enter-key-behavior)