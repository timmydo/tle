(in-package :tle)

(defun main ()
  (let ((ui (make-web-ui))
	(editor (make-standard-editor)))
    (run-ui ui editor)))