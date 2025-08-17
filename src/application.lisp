(in-package :tle)

(defclass application ()
  ((name :initarg :name :reader application-name)
   (frames :initarg :frames :accessor application-frames :initform nil)
   (editor :initarg :editor :accessor application-editor :initform nil))
  (:documentation "An application with frames and an editor."))

(defvar *applications* (make-hash-table :test 'equal))
(defvar *default-application-name* "default")

(defun get-application (app-name)
  "Get application by name, creating default if it doesn't exist."
  (or (gethash app-name *applications*)
      (when (string= app-name *default-application-name*)
        (let ((app (make-instance 'application 
                                  :name app-name
                                  :editor *editor-instance*)))
          (setf (gethash app-name *applications*) app)
          app))))

(defun register-application (app-name editor &optional frames)
  "Register a new application with the given name, editor, and optional frames."
  (let ((app (make-instance 'application
                            :name app-name
                            :editor editor
                            :frames (or frames nil))))
    (setf (gethash app-name *applications*) app)
    app))

(defun list-applications ()
  "List all registered application names."
  (let ((apps nil))
    (maphash (lambda (name app)
               (declare (ignore app))
               (push name apps))
             *applications*)
    (sort apps #'string<)))

(defun remove-application (app-name)
  "Remove an application by name."
  (remhash app-name *applications*))

(defun add-frame-to-application (app-name frame)
  "Add a frame to an application."
  (let ((app (gethash app-name *applications*)))
    (when app
      (push frame (application-frames app)))))

(defun remove-frame-from-application (app-name frame)
  "Remove a frame from an application."
  (let ((app (gethash app-name *applications*)))
    (when app
      (setf (application-frames app)
            (remove frame (application-frames app))))))

(defun get-application-frames (app-name)
  "Get all frames for an application."
  (let ((app (gethash app-name *applications*)))
    (when app
      (application-frames app))))

(defun set-application-editor (app-name editor)
  "Set the editor for an application."
  (let ((app (gethash app-name *applications*)))
    (when app
      (setf (application-editor app) editor))))

(defun create-sample-frames (app-name)
  "Create sample frames for testing the application."
  (let ((app (get-application app-name)))
    (when app
      (let* ((editor (application-editor app))
             (buffer (when editor (current-buffer editor)))
             (frame1 (make-standard-frame buffer :title "Main Buffer" :x 50 :y 50 :width 500 :height 400))
             (frame2 (make-standard-frame buffer :title "Secondary View" :x 600 :y 100 :width 400 :height 300)))
        (setf (application-frames app) (list frame1 frame2))
        app))))

;; Export key functions for external use
(export '(application
          application-name
          application-frames
          application-editor
          register-application
          list-applications
          remove-application
          add-frame-to-application
          remove-frame-from-application
          get-application-frames
          set-application-editor
          get-application))