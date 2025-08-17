(in-package :tle)

(defclass window ()
  ((%handle :initform nil :accessor window-handle))
  (:documentation "A window implementation"))

(defgeneric draw-window (window ui)
  (:documentation "Draw a WINDOW using UI"))

(defgeneric window-dispatch-event (window ui-event)
  (:documentation "The WINDOW dispatches a UI-EVENT to the correct view"))




(defclass standard-window (window)
  ((%views :accessor views)))


(defun make-standard-window (buffer)
  (let ((e (make-instance 'standard-window))
	(view (make-instance 'standard-view)))
    (setf (view-point view) (cons 0 0))
    (setf (view-mark view) nil)
    (setf (view-buffer view) buffer)
    (setf (views e) (list view))
    e))


;; simple window ... only display one buffer now
(defmethod draw-window ((window standard-window) (ui ui-implementation))
  (multiple-value-bind (width height) (ui-window-size window ui)
    (draw-view (first (views window))
		      (make-rect :x 0 :y 0 :width width :height height)
		      window
		      ui)))


;; dispatch to the active view and remap coordinates
(defmethod window-dispatch-event ((window standard-window) event)
  (view-handle-event (first (views window)) event))
