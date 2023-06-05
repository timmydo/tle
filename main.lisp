
(in-package :tle)


(defun main ()
  (let ((ui (make-sdl2-ui))
	(editor (make-standard-editor)))
    (run-ui ui editor)))
