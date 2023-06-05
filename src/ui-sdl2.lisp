(in-package :tle)


(defclass sdl2-ui (ui-implementation)
  ((name :initform "SDL2" :reader name))
  (:documentation "A UI implementation"))


(defun make-sdl2-ui ()
  (make-instance 'sdl2-ui))

(defmethod run-ui ((ui-implementation sdl2-ui) (editor editor))
  )
