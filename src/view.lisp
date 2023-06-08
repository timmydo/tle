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

