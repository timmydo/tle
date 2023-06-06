(in-package :tle)

(defclass standard-window (window)
  ((%views :accessor views)))

(defclass buffer-view (window)
  ((%view-start :accessor view-start)
   (%lines-displayed :accessor lines-displayed)))


(defun make-standard-window (buffer)
  (let ((e (make-instance 'standard-window))
	(view (make-instance 'buffer-view)))
    (setf (view-start view) 0)
    (setf (lines-displayed view) 100) ;; fixme
    (setf (views e) (list buffer))
    e))

(defmethod draw-window ((window standard-window))
  
  )

