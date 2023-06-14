(in-package :tle)

(defclass standard-view (view)
  ((%x :initform 0 :accessor view-x)
   (%y :initform 0 :accessor view-y)
   (%point :initform nil :accessor view-point)
   (%mark :initform nil :accessor view-mark)
   ))

(defmethod view-move-forward ((view standard-view) amount)
  )

(defmethod view-move-down ((view standard-view) amount)
  )

(defmethod view-mark ((view standard-view))
  )

(defmethod draw-view ((view standard-view) rect (window window) (ui ui-implementation))
  (let ((x 0)
	(y 0)
	(current-line (view-y view))
	(text "")
	(line-height (ui-character-height window ui))
	(line-count (buffer-line-count (view-buffer view))))
    (multiple-value-bind (width height) (ui-window-size window ui)
      ;; (format t "win width ~S height ~S ~%" width height)
      (loop

       ;; if we are past the bottom stop drawing
       (when (or (>= y height) (>= current-line line-count))
	 (return-from draw-view (values x y)))

       ;; the text on this line
       (setf text (buffer-line (view-buffer view) current-line))
       
       ;; draw a line of text
       (multiple-value-bind (w h)
	   (ui-draw-text window
			 ui
			 text
			 (make-rect :x x :y y :width (- width x) :height (- height y)))
	 (setf line-height h))

       ;; go down the line height
       (setf y (+ line-height y))
       (setf current-line (1+ current-line)))
      
      )))

