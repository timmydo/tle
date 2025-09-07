;; Test isearch highlighting functionality
(asdf:load-system :tle)

(defpackage #:test-isearch-highlighting
  (:use #:cl)
  (:export #:run-all-tests))

(in-package #:test-isearch-highlighting)

(defun test-isearch-state-management ()
  "Test that isearch state is properly managed in buffer."
  (format t "Testing isearch state management...~%")
  (let ((buffer (tle::make-empty-buffer "*test*")))
    ;; Initially no isearch state
    (assert (not (tle::buffer-isearch-active-p buffer)))
    (assert (not (tle::buffer-isearch-term buffer)))
    
    ;; Set isearch state
    (tle::set-isearch-state buffer "test" t)
    (assert (tle::buffer-isearch-active-p buffer))
    (assert (string= "test" (tle::buffer-isearch-term buffer)))
    
    ;; Clear isearch state
    (tle::clear-isearch-state buffer)
    (assert (not (tle::buffer-isearch-active-p buffer)))
    (assert (not (tle::buffer-isearch-term buffer)))
    
    (format t "✓ Isearch state management test passed~%")))

(defun test-find-line-matches ()
  "Test finding matches in a line."
  (format t "Testing line match finding...~%")
  (let ((line-text "hello world hello test hello"))
    ;; Test finding all matches of "hello"
    (let ((matches (tle::get-line-isearch-matches line-text "hello")))
      (assert (= 3 (length matches)))
      (assert (equal '(0 5) (first matches)))   ; First "hello"
      (assert (equal '(12 17) (second matches))) ; Second "hello"
      (assert (equal '(23 28) (third matches)))) ; Third "hello"
    
    ;; Test finding single match of "world"
    (let ((matches (tle::get-line-isearch-matches line-text "world")))
      (assert (= 1 (length matches)))
      (assert (equal '(6 11) (first matches))))
    
    ;; Test no matches
    (let ((matches (tle::get-line-isearch-matches line-text "notfound")))
      (assert (= 0 (length matches))))
    
    ;; Test empty search term
    (let ((matches (tle::get-line-isearch-matches line-text "")))
      (assert (= 0 (length matches))))
    
    (format t "✓ Line match finding test passed~%")))

(defun test-char-in-match ()
  "Test character position matching."
  (format t "Testing character position matching...~%")
  (let ((matches '((0 5) (12 17))))  ; Two matches: positions 0-4 and 12-16
    ;; Test characters within first match
    (assert (tle::char-in-isearch-match-p 0 matches))
    (assert (tle::char-in-isearch-match-p 2 matches))
    (assert (tle::char-in-isearch-match-p 4 matches))
    
    ;; Test character just outside first match
    (assert (not (tle::char-in-isearch-match-p 5 matches)))
    
    ;; Test characters within second match
    (assert (tle::char-in-isearch-match-p 12 matches))
    (assert (tle::char-in-isearch-match-p 15 matches))
    (assert (tle::char-in-isearch-match-p 16 matches))
    
    ;; Test character just outside second match
    (assert (not (tle::char-in-isearch-match-p 17 matches)))
    
    ;; Test characters between matches
    (assert (not (tle::char-in-isearch-match-p 6 matches)))
    (assert (not (tle::char-in-isearch-match-p 11 matches)))
    
    (format t "✓ Character position matching test passed~%")))

(defun test-buffer-rendering-with-isearch ()
  "Test that buffer rendering includes isearch highlighting."
  (format t "Testing buffer rendering with isearch...~%")
  (let ((buffer (tle::make-empty-buffer "*test*"))
        (ui (make-instance 'tle::web-ui)))
    ;; Set up buffer with test content
    (setf (tle::lines buffer) (vector "hello world" "test hello world test"))
    (tle::buffer-set-point buffer 0 0)
    
    ;; Test without isearch
    (let ((rendered (tle::render buffer ui)))
      (assert (not (search "isearch-match" rendered)))
      (format t "  ✓ No isearch highlighting when inactive~%"))
    
    ;; Test with isearch active
    (tle::set-isearch-state buffer "hello" t)
    (let ((rendered (tle::render buffer ui)))
      (assert (search "isearch-match" rendered))
      (format t "  ✓ Isearch highlighting present when active~%"))
    
    ;; Test with different search term
    (tle::set-isearch-state buffer "world" t)
    (let ((rendered (tle::render buffer ui)))
      (assert (search "isearch-match" rendered))
      (format t "  ✓ Isearch highlighting changes with search term~%"))
    
    ;; Test clearing isearch
    (tle::clear-isearch-state buffer)
    (let ((rendered (tle::render buffer ui)))
      (assert (not (search "isearch-match" rendered)))
      (format t "  ✓ No isearch highlighting after clearing~%"))
    
    (format t "✓ Buffer rendering with isearch test passed~%")))

(defun run-all-tests ()
  "Run all isearch highlighting tests."
  (format t "~%======================================~%")
  (format t "Running Isearch Highlighting Tests~%")
  (format t "======================================~%~%")
  
  (test-isearch-state-management)
  (test-find-line-matches)
  (test-char-in-match)
  (test-buffer-rendering-with-isearch)
  
  (format t "~%======================================~%")
  (format t "All isearch highlighting tests passed successfully!~%")
  (format t "======================================~%~%"))

;; Run tests when file is loaded
(run-all-tests)