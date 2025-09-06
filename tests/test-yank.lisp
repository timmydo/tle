(in-package :tle)

(defun test-yank-basic ()
  "Test basic yank functionality"
  (format t "Running basic yank tests...~%")
  
  ;; Test 1: Yank from empty kill ring (should do nothing)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 6)  ; At space
    (let ((original-line (buffer-line buf 0))
          (original-point (copy-list (buffer-get-point buf))))
      (yank buf)  ; Should do nothing since kill ring is empty
      (assert (string= (buffer-line buf 0) original-line) ()
              "Test 1 failed: line should not change, got '~A'" (buffer-line buf 0))
      (assert (equal (buffer-get-point buf) original-point) ()
              "Test 1 point failed: expected ~A, got ~A" original-point (buffer-get-point buf))
      (format t "✓ Test 1 passed: Yank from empty kill ring does nothing~%")))
  
  ;; Test 2: Kill then yank (single line)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 6)  ; At 'w'
    (buffer-set-mark buf 0 2)   ; At first 'l'
    ;; Kill the region "llo " (positions 2-6)
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "heworld") ()
            "Test 2a failed: expected 'heworld', got '~A'" (buffer-line buf 0))
    ;; Now yank it back
    (yank buf)
    (assert (string= (buffer-line buf 0) "hello world") ()
            "Test 2b failed: expected 'hello world', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 6)) () "Test 2 point failed: expected (0 6), got ~A" point))
    (format t "✓ Test 2 passed: Kill then yank (single line)~%"))
  
  ;; Test 3: Kill whole line then yank
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line one" "line two" "line three"))
    (buffer-set-point buf 1 3)  ; In middle of "line two"
    (buffer-clear-mark buf)
    ;; Kill whole line
    (kill-region buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3a line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "line one") ()
            "Test 3b failed: expected 'line one', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "line three") ()
            "Test 3c failed: expected 'line three', got '~A'" (buffer-line buf 1))
    ;; Now yank the killed line back
    (buffer-set-point buf 1 0)  ; Beginning of "line three"
    (yank buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 3d line count failed: expected 3, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "line two") ()
            "Test 3e failed: expected 'line two', got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "line three") ()
            "Test 3f failed: expected 'line three', got '~A'" (buffer-line buf 2))
    (format t "✓ Test 3 passed: Kill whole line then yank~%"))
  
  ;; Test 4: Multiple kills build kill ring, yank gets most recent
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "abc def ghi"))
    ;; Kill "abc"
    (buffer-set-point buf 0 3)
    (buffer-set-mark buf 0 0)
    (kill-region buf)
    (assert (string= (buffer-line buf 0) " def ghi") ()
            "Test 4a failed: expected ' def ghi', got '~A'" (buffer-line buf 0))
    ;; Kill " def"
    (buffer-set-point buf 0 4)
    (buffer-set-mark buf 0 0)
    (kill-region buf)
    (assert (string= (buffer-line buf 0) " ghi") ()
            "Test 4b failed: expected ' ghi', got '~A'" (buffer-line buf 0))
    ;; Yank should get " def" (most recent)
    (buffer-set-point buf 0 0)
    (yank buf)
    (assert (string= (buffer-line buf 0) " def ghi") ()
            "Test 4c failed: expected ' def ghi', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 4 passed: Kill ring maintains order, yank gets most recent~%"))
  
  (format t "All basic yank tests passed!~%~%"))

(defun test-yank-multiline ()
  "Test yank with multi-line content"
  (format t "Running multi-line yank tests...~%")
  
  ;; Test 1: Kill and yank multi-line region
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first line" "second line" "third line" "fourth line"))
    (buffer-set-point buf 2 6)  ; At 'd' in "second line"  
    (buffer-set-mark buf 1 3)   ; At 'o' in "second line"
    ;; Kill multi-line region
    (kill-region buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 1a line count failed: expected 3, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "secline") ()
            "Test 1b failed: expected 'secline', got '~A'" (buffer-line buf 1))
    ;; Yank it back
    (buffer-set-point buf 2 0)  ; Beginning of "fourth line"
    (yank buf)
    (assert (= (buffer-line-count buf) 4) ()
            "Test 1c line count failed: expected 4, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 2) "ond line") ()
            "Test 1d failed: expected 'ond line', got '~A'" (buffer-line buf 2))
    (assert (string= (buffer-line buf 3) "third fourth line") ()
            "Test 1e failed: expected 'third fourth line', got '~A'" (buffer-line buf 3))
    (format t "✓ Test 1 passed: Multi-line kill and yank~%"))
  
  ;; Test 2: Yank multi-line at different positions
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2" "line3"))
    ;; Kill whole middle line
    (buffer-set-point buf 1 2)
    (buffer-clear-mark buf)
    (kill-region buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 2a line count failed: expected 2, got ~A" (buffer-line-count buf))
    ;; Yank in middle of first line
    (buffer-set-point buf 0 2)  ; After "li" in "line1"
    (yank buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 2b line count failed: expected 3, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "liline2") ()
            "Test 2c failed: expected 'liline2', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "ne1") ()
            "Test 2d failed: expected 'ne1', got '~A'" (buffer-line buf 1))
    (assert (string= (buffer-line buf 2) "line3") ()
            "Test 2e failed: expected 'line3', got '~A'" (buffer-line buf 2))
    (format t "✓ Test 2 passed: Yank multi-line at different position~%"))
  
  (format t "All multi-line yank tests passed!~%~%"))

(defun test-yank-with-copy ()
  "Test yank functionality with copy-region-as-kill"
  (format t "Running yank with copy tests...~%")
  
  ;; Test 1: Copy then yank (no deletion)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "original text"))
    (buffer-set-point buf 0 9)  ; At 't' in "text"
    (buffer-set-mark buf 0 5)   ; At 'i' in "original"
    ;; Copy the region "nal " (positions 5-9)
    (copy-region-as-kill buf)
    (assert (string= (buffer-line buf 0) "original text") ()
            "Test 1a failed: line should not change after copy, got '~A'" (buffer-line buf 0))
    ;; Move cursor and yank
    (buffer-set-point buf 0 13)  ; At end
    (yank buf)
    (assert (string= (buffer-line buf 0) "original textnal ") ()
            "Test 1b failed: expected 'original textnal ', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Copy then yank~%"))
  
  ;; Test 2: Copy whole line then yank
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "first" "second" "third"))
    (buffer-set-point buf 1 2)  ; In middle of "second"
    (buffer-clear-mark buf)
    ;; Copy whole line
    (copy-region-as-kill buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 2a line count failed: expected 3, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 1) "second") ()
            "Test 2b failed: line should not change after copy, got '~A'" (buffer-line buf 1))
    ;; Yank at end
    (buffer-set-point buf 2 5)  ; End of "third"
    (yank buf)
    (assert (= (buffer-line-count buf) 4) ()
            "Test 2c line count failed: expected 4, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 2) "thirdsecond") ()
            "Test 2d failed: expected 'thirdsecond', got '~A'" (buffer-line buf 2))
    (assert (string= (buffer-line buf 3) "") ()
            "Test 2e failed: expected empty line, got '~A'" (buffer-line buf 3))
    (format t "✓ Test 2 passed: Copy whole line then yank~%"))
  
  (format t "All copy and yank tests passed!~%~%"))

(defun test-yank-undo-redo ()
  "Test yank undo/redo functionality"
  (format t "Running yank undo/redo tests...~%")
  
  ;; Test 1: Basic yank undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    (buffer-set-point buf 0 6)  ; At 'w'
    (buffer-set-mark buf 0 2)   ; At first 'l'
    ;; Kill region
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "heworld") ()
            "Test 1a failed: expected 'heworld', got '~A'" (buffer-line buf 0))
    ;; Yank it back
    (buffer-set-point buf 0 2)  ; Where the text was originally killed from
    (yank buf)
    (assert (string= (buffer-line buf 0) "hello world") ()
            "Test 1b failed: expected 'hello world', got '~A'" (buffer-line buf 0))
    ;; Undo the yank
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "heworld") ()
            "Test 1c failed: expected 'heworld', got '~A'" (buffer-line buf 0))
    (let ((point (buffer-get-point buf)))
      (assert (equal point '(0 2)) () "Test 1d point failed: expected (0 2), got ~A" point))
    ;; Redo the yank
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "hello world") ()
            "Test 1e failed: expected 'hello world', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Basic yank undo/redo~%"))
  
  ;; Test 2: Yank twice in a row, then undo/redo twice
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 2)  ; After "te"
    (buffer-set-mark buf 0 0)   ; At beginning
    ;; Kill "te"
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "st") ()
            "Test 2a failed: expected 'st', got '~A'" (buffer-line buf 0))
    ;; First yank
    (buffer-set-point buf 0 1)  ; After "s"
    (yank buf)
    (assert (string= (buffer-line buf 0) "stet") ()
            "Test 2b failed: expected 'stet', got '~A'" (buffer-line buf 0))
    ;; Second yank (should work)
    (yank buf)
    (assert (string= (buffer-line buf 0) "stetet") ()
            "Test 2c failed: expected 'stetet', got '~A'" (buffer-line buf 0))
    ;; First undo
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "stet") ()
            "Test 2d failed: expected 'stet', got '~A'" (buffer-line buf 0))
    ;; Second undo
    (buffer-undo buf)
    (assert (string= (buffer-line buf 0) "st") ()
            "Test 2e failed: expected 'st', got '~A'" (buffer-line buf 0))
    ;; First redo
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "stet") ()
            "Test 2f failed: expected 'stet', got '~A'" (buffer-line buf 0))
    ;; Second redo
    (buffer-redo buf)
    (assert (string= (buffer-line buf 0) "stetet") ()
            "Test 2g failed: expected 'stetet', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Yank twice in a row, undo/redo twice~%"))
  
  ;; Test 3: Multi-line yank undo/redo
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "line1" "line2" "line3"))
    ;; Kill whole middle line
    (buffer-set-point buf 1 2)
    (buffer-clear-mark buf)
    (kill-region buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3a line count failed: expected 2, got ~A" (buffer-line-count buf))
    ;; Yank at beginning of first line
    (buffer-set-point buf 0 0)
    (yank buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 3b line count failed: expected 3, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "line2") ()
            "Test 3c failed: expected 'line2', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "line1") ()
            "Test 3d failed: expected 'line1', got '~A'" (buffer-line buf 1))
    ;; Undo multi-line yank
    (buffer-undo buf)
    (assert (= (buffer-line-count buf) 2) ()
            "Test 3e line count failed: expected 2, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "line1") ()
            "Test 3f failed: expected 'line1', got '~A'" (buffer-line buf 0))
    (assert (string= (buffer-line buf 1) "line3") ()
            "Test 3g failed: expected 'line3', got '~A'" (buffer-line buf 1))
    ;; Redo multi-line yank
    (buffer-redo buf)
    (assert (= (buffer-line-count buf) 3) ()
            "Test 3h line count failed: expected 3, got ~A" (buffer-line-count buf))
    (assert (string= (buffer-line buf 0) "line2") ()
            "Test 3i failed: expected 'line2', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 3 passed: Multi-line yank undo/redo~%"))
  
  (format t "All yank undo/redo tests passed!~%~%"))

(defun test-yank-edge-cases ()
  "Test yank edge cases and boundary conditions"
  (format t "Running yank edge case tests...~%")
  
  ;; Test 1: Yank at beginning of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 2)  ; After "he"
    (buffer-set-mark buf 0 0)   ; At beginning
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "llo") ()
            "Test 1a failed: expected 'llo', got '~A'" (buffer-line buf 0))
    ;; Yank at beginning
    (buffer-set-point buf 0 0)
    (yank buf)
    (assert (string= (buffer-line buf 0) "hello") ()
            "Test 1b failed: expected 'hello', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 1 passed: Yank at beginning of buffer~%"))
  
  ;; Test 2: Yank at end of buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello"))
    (buffer-set-point buf 0 5)  ; At end
    (buffer-set-mark buf 0 3)   ; At 'l'
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "hel") ()
            "Test 2a failed: expected 'hel', got '~A'" (buffer-line buf 0))
    ;; Yank at end
    (buffer-set-point buf 0 3)  ; At end
    (yank buf)
    (assert (string= (buffer-line buf 0) "hello") ()
            "Test 2b failed: expected 'hello', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 2 passed: Yank at end of buffer~%"))
  
  ;; Test 3: Yank with empty string (should not crash)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "test"))
    (buffer-set-point buf 0 2)  ; After "te"
    (buffer-set-mark buf 0 2)   ; Same position (empty region)
    ;; Kill empty region
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "test") ()
            "Test 3a failed: line should not change, got '~A'" (buffer-line buf 0))
    ;; Try to yank (should do nothing if kill ring is empty or contains empty string)
    (yank buf)
    ;; This test verifies no crash occurs - the buffer state doesn't matter much
    (format t "✓ Test 3 passed: Yank with empty string (no crash)~%"))
  
  ;; Test 4: Yank in single line buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "single"))
    (buffer-set-point buf 0 3)  ; After "sin"
    (buffer-set-mark buf 0 1)   ; After "s"
    (kill-region buf)
    (assert (string= (buffer-line buf 0) "sgle") ()
            "Test 4a failed: expected 'sgle', got '~A'" (buffer-line buf 0))
    ;; Yank back
    (buffer-set-point buf 0 1)  ; After "s"
    (yank buf)
    (assert (string= (buffer-line buf 0) "single") ()
            "Test 4b failed: expected 'single', got '~A'" (buffer-line buf 0))
    (format t "✓ Test 4 passed: Yank in single line buffer~%"))
  
  (format t "All yank edge case tests passed!~%~%"))

(defun test-yank-mark-clearing ()
  "Test that yank properly clears the mark"
  (format t "Running yank mark clearing tests...~%")
  
  ;; Test 1: Mark should be cleared after yank
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) (vector "hello world"))
    ;; Set up kill ring
    (buffer-set-point buf 0 6)
    (buffer-set-mark buf 0 2)
    (kill-region buf)
    ;; Set mark again and yank
    (buffer-set-point buf 0 2)
    (buffer-set-mark buf 0 5)  ; Set mark somewhere
    (assert (buffer-get-mark buf) () "Test 1 setup failed: mark should be set")
    (yank buf)
    (assert (null (buffer-get-mark buf)) () "Test 1 failed: mark should be cleared after yank")
    (format t "✓ Test 1 passed: Mark cleared after yank~%"))
  
  (format t "All yank mark clearing tests passed!~%~%"))

(defun run-all-yank-tests ()
  "Run all yank tests"
  (format t "~%======================================~%")
  (format t "Running Yank Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-yank-basic)
        (test-yank-multiline)
        (test-yank-with-copy)
        (test-yank-undo-redo)
        (test-yank-edge-cases)
        (test-yank-mark-clearing)
        (format t "~%======================================~%")
        (format t "All yank tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)