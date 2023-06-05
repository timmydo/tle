(in-package :tle)

(defclass standard-window (window)
  ((%views :accessor views)))

(defclass buffer-view (window)
  ((%view-start :accessor view-start)
   (%lines-displayed :accessor lines-displayed)))


(defun make-standard-window (buffer)
  (let ((e (make-instance 'standard-window)))
    (setf (views e) (list (make-standard-pane buffer)))
    e))

(defmethod draw-window ((window standard-window))
  
  )

