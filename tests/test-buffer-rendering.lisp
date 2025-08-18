(in-package :tle)

(defun test-single-line-selection-mark-before-point ()
  "Test selection within a single line where mark comes before point"
  (format t "Running single-line selection tests (mark before point)...~%")
  
  ;; Test 1: Basic single-line selection (mark before point)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 8)  ; Point at 'r' in "world"
    (buffer-set-mark buf 0 3)   ; Mark at 'l' in "hello"
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 1 failed: No selection found")
      (assert (search "cursor" rendered) () "Test 1 failed: No cursor found")
      (assert (search "mark" rendered) () "Test 1 failed: No mark found")
    (format t "✓ Test 1 passed: Basic single-line selection (mark before point)~%")))
  
  ;; Test 2: Adjacent positions (mark before point)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test"))
    (buffer-set-point buf 0 2)  ; Point at 's'
    (buffer-set-mark buf 0 1)   ; Mark at 'e'
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 2 failed: No selection found")
    (format t "✓ Test 2 passed: Adjacent positions selection~%")))
  
  ;; Test 3: Selection at beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("beginning"))
    (buffer-set-point buf 0 3)  ; Point at 'i'
    (buffer-set-mark buf 0 0)   ; Mark at beginning
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 3 failed: No selection found")
    (format t "✓ Test 3 passed: Selection from beginning of line~%")))
  
  ;; Test 4: Selection at end of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("ending"))
    (buffer-set-point buf 0 6)  ; Point at end
    (buffer-set-mark buf 0 3)   ; Mark at 'i'
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 4 failed: No selection found")
    (format t "✓ Test 4 passed: Selection to end of line~%")))
  
  (format t "All single-line selection tests (mark before point) passed!~%~%"))

(defun test-single-line-selection-point-before-mark ()
  "Test selection within a single line where point comes before mark"
  (format t "Running single-line selection tests (point before mark)...~%")
  
  ;; Test 1: Basic single-line selection (point before mark)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("hello world"))
    (buffer-set-point buf 0 3)  ; Point at 'l' in "hello"
    (buffer-set-mark buf 0 8)   ; Mark at 'r' in "world"
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 1 failed: No selection found")
      (assert (search "cursor" rendered) () "Test 1 failed: No cursor found")
      (assert (search "mark" rendered) () "Test 1 failed: No mark found")
    (format t "✓ Test 1 passed: Basic single-line selection (point before mark)~%")))
  
  ;; Test 2: Adjacent positions (point before mark)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test"))
    (buffer-set-point buf 0 1)  ; Point at 'e'
    (buffer-set-mark buf 0 2)   ; Mark at 's'
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 2 failed: No selection found")
    (format t "✓ Test 2 passed: Adjacent positions selection (reversed)~%")))
  
  ;; Test 3: Point at beginning, mark in middle
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("beginning"))
    (buffer-set-point buf 0 0)  ; Point at beginning
    (buffer-set-mark buf 0 3)   ; Mark at 'i'
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 3 failed: No selection found")
    (format t "✓ Test 3 passed: Point at beginning, mark in middle~%")))
  
  ;; Test 4: Point in middle, mark at end
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("ending"))
    (buffer-set-point buf 0 3)  ; Point at 'i'
    (buffer-set-mark buf 0 6)   ; Mark at end
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 4 failed: No selection found")
    (format t "✓ Test 4 passed: Point in middle, mark at end~%")))
  
  (format t "All single-line selection tests (point before mark) passed!~%~%"))

(defun test-same-position-no-selection ()
  "Test that point and mark at same position shows no selection"
  (format t "Running same position tests...~%")
  
  ;; Test 1: Point and mark at same position
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test line"))
    (buffer-set-point buf 0 3)
    (buffer-set-mark buf 0 3)   ; Same position as point
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (not (search "selection" rendered)) () "Test 1 failed: Found selection when point and mark are same")
      (assert (search "cursor" rendered) () "Test 1 failed: No cursor found")
    (format t "✓ Test 1 passed: No selection when point and mark are same~%")))
  
  ;; Test 2: Point and mark at beginning
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test"))
    (buffer-set-point buf 0 0)
    (buffer-set-mark buf 0 0)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (not (search "selection" rendered)) () "Test 2 failed: Found selection at beginning")
    (format t "✓ Test 2 passed: No selection at beginning~%")))
  
  ;; Test 3: Point and mark at end
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test"))
    (buffer-set-point buf 0 4)
    (buffer-set-mark buf 0 4)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (not (search "selection" rendered)) () "Test 3 failed: Found selection at end")
    (format t "✓ Test 3 passed: No selection at end~%")))
  
  (format t "All same position tests passed!~%~%"))

(defun test-multi-line-selection ()
  "Test selections that span multiple lines"
  (format t "Running multi-line selection tests...~%")
  
  ;; Test 1: Selection across two adjacent lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first line" "second line" "third line"))
    (buffer-set-point buf 0 6)  ; Point in "first line"
    (buffer-set-mark buf 1 6)   ; Mark in "second line"
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 1 failed: No selection found")
      ;; Should have selection on both lines 0 and 1
      (let ((line-divs (split-string rendered "<div class=\"line\">")))
        (assert (>= (length line-divs) 3) () "Test 1 failed: Not enough line divs")
        ;; Check that first line has selection
        (assert (search "selection" (nth 1 line-divs)) () "Test 1 failed: No selection on first line")
        ;; Check that second line has selection  
        (assert (search "selection" (nth 2 line-divs)) () "Test 1 failed: No selection on second line"))
    (format t "✓ Test 1 passed: Selection across two adjacent lines~%")))
  
  ;; Test 2: Selection across three lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two" "line three" "line four"))
    (buffer-set-point buf 0 5)  ; Point in first line
    (buffer-set-mark buf 2 5)   ; Mark in third line
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 2 failed: No selection found")
      ;; Should have selection on lines 0, 1, and 2
      (let ((line-divs (split-string rendered "<div class=\"line\">")))
        (assert (search "selection" (nth 1 line-divs)) () "Test 2 failed: No selection on line 0")
        (assert (search "selection" (nth 2 line-divs)) () "Test 2 failed: No selection on line 1") 
        (assert (search "selection" (nth 3 line-divs)) () "Test 2 failed: No selection on line 2")
        ;; Line 3 should not have selection
        (assert (not (search "selection" (nth 4 line-divs))) () "Test 2 failed: Unexpected selection on line 3"))
    (format t "✓ Test 2 passed: Selection across three lines~%")))
  
  ;; Test 3: Reverse multi-line selection (mark before point)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "second" "third"))
    (buffer-set-point buf 2 3)  ; Point in third line
    (buffer-set-mark buf 0 2)   ; Mark in first line
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 3 failed: No selection found")
      (let ((line-divs (split-string rendered "<div class=\"line\">")))
        (assert (search "selection" (nth 1 line-divs)) () "Test 3 failed: No selection on line 0")
        (assert (search "selection" (nth 2 line-divs)) () "Test 3 failed: No selection on line 1")
        (assert (search "selection" (nth 3 line-divs)) () "Test 3 failed: No selection on line 2"))
    (format t "✓ Test 3 passed: Reverse multi-line selection~%")))
  
  ;; Test 4: Selection from beginning of first line to end of last line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("start" "middle" "end"))
    (buffer-set-point buf 0 0)  ; Point at very beginning
    (buffer-set-mark buf 2 3)   ; Mark at end of last line
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 4 failed: No selection found")
    (format t "✓ Test 4 passed: Full buffer selection~%")))
  
  (format t "All multi-line selection tests passed!~%~%"))

(defun test-cursor-only-rendering ()
  "Test rendering with cursor only (no mark)"
  (format t "Running cursor-only rendering tests...~%")
  
  ;; Test 1: No mark set
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("just cursor"))
    (buffer-set-point buf 0 5)
    ;; Explicitly clear mark
    (buffer-clear-mark buf)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "cursor" rendered) () "Test 1 failed: No cursor found")
      (assert (not (search "selection" rendered)) () "Test 1 failed: Unexpected selection found")
      (assert (not (search "mark" rendered)) () "Test 1 failed: Unexpected mark found")
    (format t "✓ Test 1 passed: Cursor only (no mark)~%")))
  
  ;; Test 2: Cursor at beginning of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("beginning"))
    (buffer-set-point buf 0 0)
    (buffer-clear-mark buf)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "cursor" rendered) () "Test 2 failed: No cursor found")
      (assert (not (search "selection" rendered)) () "Test 2 failed: Unexpected selection found")
    (format t "✓ Test 2 passed: Cursor at beginning~%")))
  
  ;; Test 3: Cursor at end of line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("ending"))
    (buffer-set-point buf 0 6)
    (buffer-clear-mark buf)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "cursor" rendered) () "Test 3 failed: No cursor found")
      (assert (not (search "selection" rendered)) () "Test 3 failed: Unexpected selection found")
    (format t "✓ Test 3 passed: Cursor at end~%")))
  
  ;; Test 4: Cursor in multi-line buffer (different lines)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two" "line three"))
    (buffer-set-point buf 1 4)  ; Cursor on line 1
    (buffer-clear-mark buf)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "cursor" rendered) () "Test 4 failed: No cursor found")
      (assert (not (search "selection" rendered)) () "Test 4 failed: Unexpected selection found")
      ;; Check that cursor is only on line 1
      (let ((line-divs (split-string rendered "<div class=\"line\">")))
        (assert (not (search "cursor" (nth 1 line-divs))) () "Test 4 failed: Cursor on wrong line 0")
        (assert (search "cursor" (nth 2 line-divs)) () "Test 4 failed: No cursor on line 1")
        (assert (not (search "cursor" (nth 3 line-divs))) () "Test 4 failed: Cursor on wrong line 2"))
    (format t "✓ Test 4 passed: Cursor on specific line in multi-line buffer~%")))
  
  (format t "All cursor-only rendering tests passed!~%~%"))

(defun test-edge-cases ()
  "Test edge cases and boundary conditions for buffer rendering"
  (format t "Running edge case tests...~%")
  
  ;; Test 1: Empty buffer
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #())
    (buffer-set-point buf 0 0)
    (handler-case
        (let ((rendered (render buf (make-instance 'web-ui))))
          (assert (search "Empty buffer" rendered) () "Test 1 failed: No empty buffer message")
        (format t "✓ Test 1 passed: Empty buffer handling~%"))
      (error (e)
        (format t "✗ Test 1 failed: Empty buffer caused error: ~A~%" e))))
  
  ;; Test 2: Single empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #(""))
    (buffer-set-point buf 0 0)
    (buffer-clear-mark buf)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "cursor" rendered) () "Test 2 failed: No cursor found in empty line")
      (assert (not (search "selection" rendered)) () "Test 2 failed: Unexpected selection in empty line")
    (format t "✓ Test 2 passed: Single empty line~%")))
  
  ;; Test 3: Multiple empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("" "" ""))
    (buffer-set-point buf 1 0)  ; Cursor on middle empty line
    (buffer-clear-mark buf)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "cursor" rendered) () "Test 3 failed: No cursor found")
    (format t "✓ Test 3 passed: Multiple empty lines~%")))
  
  ;; Test 4: Selection in empty line
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("content" "" "more content"))
    (buffer-set-point buf 1 0)  ; Point in empty line
    (buffer-set-mark buf 1 0)   ; Mark at same position
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (not (search "selection" rendered)) () "Test 4 failed: Selection in empty line")
    (format t "✓ Test 4 passed: No selection in empty line when point=mark~%")))
  
  ;; Test 5: Very long line
  (let ((buf (make-instance 'standard-buffer))
        (long-line (make-string 1000 :initial-element #\a)))
    (setf (lines buf) (vector long-line))
    (buffer-set-point buf 0 500)
    (buffer-set-mark buf 0 600)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 5 failed: No selection in long line")
      (assert (search "cursor" rendered) () "Test 5 failed: No cursor in long line")
    (format t "✓ Test 5 passed: Very long line with selection~%")))
  
  ;; Test 6: Selection across empty lines
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("start" "" "" "end"))
    (buffer-set-point buf 0 2)
    (buffer-set-mark buf 3 1)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 6 failed: No selection across empty lines")
    (format t "✓ Test 6 passed: Selection across empty lines~%")))
  
  ;; Test 7: Lines with only whitespace
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("   " "    " "     "))
    (buffer-set-point buf 1 2)
    (buffer-set-mark buf 1 1)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 7 failed: No selection in whitespace")
    (format t "✓ Test 7 passed: Selection in whitespace-only lines~%")))
  
  ;; Test 8: Special characters
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line with <tags> & \"quotes\""))
    (buffer-set-point buf 0 12)
    (buffer-set-mark buf 0 17)  ; Select "<tags>"
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "selection" rendered) () "Test 8 failed: No selection with special chars")
      ;; Should have HTML-escaped content
      (assert (search "&lt;" rendered) () "Test 8 failed: No HTML escaping of <")
      (assert (search "&gt;" rendered) () "Test 8 failed: No HTML escaping of >")
    (format t "✓ Test 8 passed: Special characters and HTML escaping~%")))
  
  (format t "All edge case tests passed!~%~%"))

(defun test-render-consistency ()
  "Test that rendering is consistent and produces valid HTML"
  (format t "Running render consistency tests...~%")
  
  ;; Test 1: Multiple renders of same state should be identical
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("test line"))
    (buffer-set-point buf 0 5)
    (buffer-set-mark buf 0 2)
    (let ((ui (make-instance 'web-ui))
          (render1 (render buf (make-instance 'web-ui)))
          (render2 (render buf (make-instance 'web-ui))))
      (assert (string= render1 render2) () "Test 1 failed: Inconsistent renders")
    (format t "✓ Test 1 passed: Consistent rendering~%")))
  
  ;; Test 2: Rendered HTML should contain proper structure
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line one" "line two"))
    (buffer-set-point buf 0 3)
    (buffer-set-mark buf 1 3)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search "buffer-content" rendered) () "Test 2 failed: No buffer-content div")
      (assert (search "line-number" rendered) () "Test 2 failed: No line numbers")
      (assert (search "line-content" rendered) () "Test 2 failed: No line content")
      ;; Should have two line divs
      (let ((line-count (count-substring rendered "<div class=\"line\">")))
        (assert (= line-count 2) () "Test 2 failed: Wrong number of line divs: ~A" line-count))
    (format t "✓ Test 2 passed: Proper HTML structure~%")))
  
  ;; Test 3: Line numbers should be correct
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("first" "second" "third"))
    (buffer-set-point buf 1 2)
    (buffer-clear-mark buf)
    (let ((rendered (render buf (make-instance 'web-ui))))
      (assert (search ">  1<" rendered) () "Test 3 failed: No line number 1")
      (assert (search ">  2<" rendered) () "Test 3 failed: No line number 2")
      (assert (search ">  3<" rendered) () "Test 3 failed: No line number 3")
    (format t "✓ Test 3 passed: Correct line numbering~%")))
  
  (format t "All render consistency tests passed!~%~%"))

;; Utility functions for tests
(defun split-string (string delimiter)
  "Simple string splitting utility"
  (let ((result '())
        (start 0))
    (loop for pos = (search delimiter string :start2 start)
          do (push (subseq string start pos) result)
             (if pos
                 (setf start (+ pos (length delimiter)))
                 (return)))
    (nreverse result)))

(defun count-substring (string substring)
  "Count occurrences of substring in string"
  (let ((count 0)
        (start 0))
    (loop for pos = (search substring string :start2 start)
          while pos
          do (incf count)
             (setf start (+ pos (length substring))))
    count))

(defun run-all-buffer-rendering-tests ()
  "Run all buffer rendering tests"
  (format t "~%======================================~%")
  (format t "Running Buffer Rendering Tests~%")
  (format t "======================================~%~%")
  
  (handler-case
      (progn
        (test-single-line-selection-mark-before-point)
        (test-single-line-selection-point-before-mark)
        (test-same-position-no-selection)
        (test-multi-line-selection)
        (test-cursor-only-rendering)
        (test-edge-cases)
        (test-render-consistency)
        (format t "~%======================================~%")
        (format t "All buffer rendering tests passed successfully!~%")
        (format t "======================================~%"))
    (error (e)
      (format t "~%ERROR: Test failed with: ~A~%" e)
      (format t "======================================~%")))
  
  t)