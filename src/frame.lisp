(in-package :tle)

(defclass frame ()
  ((id :initarg :id :reader frame-id :initform (gensym "FRAME-"))
   (title :initarg :title :accessor frame-title :initform "Untitled Frame")
   (x :initarg :x :accessor frame-x :initform 50)
   (y :initarg :y :accessor frame-y :initform 50)
   (width :initarg :width :accessor frame-width :initform 400)
   (height :initarg :height :accessor frame-height :initform 300)
   (z-index :initarg :z-index :accessor frame-z-index :initform 1000)
   )
  (:documentation "A frame with position, size, and handle"))

(defclass window ()
  ((%handle :initform nil :accessor window-handle))
  (:documentation "A window implementation"))

(defgeneric draw-window (window ui)
  (:documentation "Draw a WINDOW using UI"))

(defgeneric window-dispatch-event (window ui-event)
  (:documentation "The WINDOW dispatches a UI-EVENT to the correct view"))




(defgeneric draw-frame (frame ui)
  (:documentation "Draw a FRAME using UI"))

(defgeneric frame-dispatch-event (frame ui-event)
  (:documentation "The FRAME dispatches a UI-EVENT to the correct view"))

(defgeneric update-frame-position (frame x y)
  (:documentation "Update frame position"))

(defgeneric update-frame-size (frame width height)
  (:documentation "Update frame size"))

(defgeneric update-frame-z-index-value (frame z-index)
  (:documentation "Update frame z-index"))


(defclass standard-frame (frame)
  ((%views :accessor frame-views)
   (%editor :accessor frame-editor :initarg :editor)))

(defclass standard-window (window)
  ((%views :accessor views)))

(defun make-standard-frame (editor &key (title "Frame") (x 50) (y 50) (width 400) (height 300))
  (let ((f (make-instance 'standard-frame :title title :x x :y y :width width :height height :editor editor))
	(view (make-instance 'standard-view)))
    (setf (view-point view) (cons 0 0))
    (setf (view-mark view) nil)
    (when (and editor (current-buffer editor))
      (setf (view-buffer view) (current-buffer editor)))
    (setf (frame-views f) (list view))
    f))

(defmethod update-frame-position ((frame standard-frame) x y)
  "Update frame position"
  (setf (frame-x frame) x)
  (setf (frame-y frame) y))

(defmethod update-frame-size ((frame standard-frame) width height)
  "Update frame size"
  (setf (frame-width frame) width)
  (setf (frame-height frame) height))

(defmethod update-frame-z-index-value ((frame standard-frame) z-index)
  "Update frame z-index"
  (setf (frame-z-index frame) z-index))


(defmethod render-components ((frame standard-frame) (ui ui-implementation))
  "Render the components of a standard frame."
  (if (and (slot-boundp frame '%editor) (frame-editor frame))
      (render (frame-editor frame) ui)
      "No editor available"))

