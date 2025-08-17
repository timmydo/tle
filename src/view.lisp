(in-package :tle)

(defclass view ()
  ((%buffer :accessor view-buffer)
   ))

(defgeneric view-move-forward (view amount)
  (:documentation "Move the cursor in a buffer"))

(defgeneric view-move-down (view amount)
  (:documentation "Move the cursor in a buffer"))

(defgeneric view-mark (view)
  (:documentation "Toggle the mark point in a buffer"))

(defgeneric draw-view (view rect window ui)
  (:documentation "Draw a VIEW on a WINDOW at RECT using UI"))

(defgeneric view-handle-event (view ui-event)
  (:documentation "The VIEW handles a UI-EVENT"))


(defvar *standard-view-line-no-fill-width* 5)

(defclass standard-view (view)
  ((%x :initform 0 :accessor view-x)
   (%y :initform 0 :accessor view-y)
   (%point :initform nil :accessor view-point)
   (%mark :initform nil :accessor view-mark)
   (%show-line-numbers :initform t :accessor view-show-line-numbers)
   ))

(defmethod view-move-forward ((view standard-view) amount)
  )

(defmethod view-move-down ((view standard-view) amount)
  )

(defmethod view-mark ((view standard-view))
  )

(defun make-point (line col)
  (cons line col))

(defun cursor-line (p)
  (car p))

(defun cursor-col (p)
  (cdr p))

(defun cursor-compare (p1 p2)
  "Return -1 if p1 comes before p2, or 1 if it comes after, or 0 if equal"
  (if (< (cursor-line p1) (cursor-line p2))
      -1
      (if (> (cursor-line p1) (cursor-line p2))
	  1
	  (if (< (cursor-col p1) (cursor-col p2))
	      -1
	      (if (> (cursor-col p1) (cursor-col p2))
		  1
		  0)))))

(defun min-cursor (p1 p2)
  "Return the cursor that comes first"
  (if (< (cursor-compare p1 p2) 0)
      p1
      p2))

(defun max-cursor (p1 p2)
  "Return the cursor that comes last"
  (if (< (cursor-compare p1 p2) 0)
      p2
      p1))


(defun calculate-selection (current-line point mark)
  "Return (start-column . end-column) if there is a selection on the current line, otherwise NIL"
  (unless mark
    (return-from calculate-selection nil))
  (let ((p1 (min-cursor point mark))
	(p2 (max-cursor point mark)))
    (if (< current-line p1)
	nil
	(if (> current-line p2)
	    nil
	    (if (= current-line (cursor-line p1))
		(cons (cursor-col p1)
		      (if (= current-line (cursor-line p2))
			  (cursor-col p2)
			  9999))
		(cons 0
		      (if (= current-line (cursor-line p2))
			  (cursor-col p2)
			  9999)))))))

(defmethod draw-view ((view standard-view) rect (window window) (ui ui-implementation))
  (let ((x (rect-x rect))
	(y (rect-y rect))
	(current-line (view-y view))
	(show-line-no (view-show-line-numbers view))
	(text "")
	(format-str (format nil "~~~ad ~~A" (- *standard-view-line-no-fill-width* 1)))
	(mark (view-mark view))
	(point (view-point view))
	(line-height (ui-character-height window ui))
	(char-width (ui-character-width window ui))
	(line-count (buffer-line-count (view-buffer view))))
    (multiple-value-bind (width height) (ui-window-size window ui)
      ;; (format t "win width ~S height ~S ~%" width height)
      (loop

       ;; if we are past the bottom stop drawing
       (when (or (>= y height) (>= current-line line-count))
	 (return-from draw-view (values x y)))

       ;; the text on this line
       (setf text (buffer-line (view-buffer view) current-line))

       (when show-line-no
	 (setf text (format nil format-str (1+ current-line) text)))

       ;; figure out if we need to draw part of the selection on this line
       (let ((selection-extent (calculate-selection current-line point mark)))
	 ;; move out for line number mode
	 (when (and selection-extent show-line-no)
	   (setf selection-extent (cons (+ *standard-view-line-no-fill-width* (car selection-extent))
					(+ *standard-view-line-no-fill-width* (cdr selection-extent)))))

	 (when selection-extent
	   (ui-draw-selection window ui (make-rect :x (+ x (* char-width (car selection-extent)))
						   :y y
						   :width (+ x (* char-width (cdr selection-extent)))
						   :height line-height)))
	 )

       ;; draw point
       (when (and point (= (cursor-line point) current-line))
	 (ui-draw-cursor window ui (make-rect :x (+ x (* char-width
							 (+
							  (if show-line-no
							      *standard-view-line-no-fill-width*
							      0)
							  (cursor-col point))))
					     :y y
					     :width char-width
					     :height line-height)))
       
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



(defmethod view-handle-event ((view standard-view) event)
  (format t "view-handle-event ~S~%" event)
  )

(defmethod view-handle-event ((view standard-view) (event key-down-event))
  (format t "key-down-event ~S~%" event)
  (let ((buffer (view-buffer view)))
    
    ))
