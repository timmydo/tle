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

(defclass repl-frame (frame)
  ((%editor :accessor repl-frame-editor :initarg :editor)
   (%rich-object-view :accessor repl-frame-rich-object-view :initarg :rich-object-view))
  (:documentation "A frame containing both an editor and a rich-object-view for REPL functionality."))

(defun make-repl-frame (editor rich-object-view &key (title "REPL") (x 100) (y 100) (width 600) (height 400))
  "Create a new repl-frame with an editor and rich-object-view."
  (make-instance 'repl-frame 
                 :title title 
                 :x x 
                 :y y 
                 :width width 
                 :height height 
                 :editor editor
                 :rich-object-view rich-object-view))

(defmethod update-frame-position ((frame repl-frame) x y)
  "Update repl-frame position"
  (setf (frame-x frame) x)
  (setf (frame-y frame) y))

(defmethod update-frame-size ((frame repl-frame) width height)
  "Update repl-frame size"
  (setf (frame-width frame) width)
  (setf (frame-height frame) height))

(defmethod update-frame-z-index-value ((frame repl-frame) z-index)
  "Update repl-frame z-index"
  (setf (frame-z-index frame) z-index))

(defmethod frame-editor ((frame repl-frame))
  "Get the editor from a repl-frame."
  (repl-frame-editor frame))

(defmethod render-components ((frame repl-frame) (ui ui-implementation))
  "Render the components of a repl-frame - rich-object-view before editor."
  (let ((editor-content (if (and (slot-boundp frame '%editor) (repl-frame-editor frame))
                            (render (repl-frame-editor frame) ui)
                            "No editor available"))
        (object-view-content (if (and (slot-boundp frame '%rich-object-view) (repl-frame-rich-object-view frame))
                                 (render (repl-frame-rich-object-view frame) ui)
                                 "No object view available")))
    (format nil 
            "<div class=\"repl-frame-container\">
               <div class=\"repl-object-section\">~A</div>
               <div class=\"repl-editor-section\">~A</div>
             </div>"
            object-view-content
            editor-content)))

