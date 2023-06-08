(in-package :tle)

(defclass standard-view (standard-view)
  ((%buffer :accessor view-buffer)
   ))


(defmethod draw-view ((view view) rect (window window) (ui ui-implementation))
  (let ((x 0)
	(y 0)
	(current-line (view-start-line view))
	(line-height 1))
    (multiple-value-bind (width height) (ui-window-size window ui)
      (format t "win width ~S height ~S ~%" width height)
      (loop

       ;; if we are past the bottom stop drawing
       (when (>= y height)
	 (return-from draw-view (values x y)))

       ;; draw a line of text
       (multiple-value-bind (w h)
	   (draw-text window ui
		      (make-rect :x x :y y :width (- width x) :height (- height y)))
	 (setf line-height h))

       ;; go down the line height
       (setf y (+ line-height y)))
      )))

