(in-package :tle)

(defclass application ()
  ((name :initarg :name :reader application-name)
   (frames :initarg :frames :accessor application-frames :initform nil))
  (:documentation "An application with frames."))

(defvar *applications* (make-hash-table :test 'equal))
(defvar *default-application-name* "default")

(defun get-application (app-name)
  "Get application by name, creating default if it doesn't exist."
  (or (gethash app-name *applications*)
      (when (string= app-name *default-application-name*)
        (let ((app (make-instance 'application 
                                  :name app-name)))
          (setf (gethash app-name *applications*) app)
          app))))

(defun register-application (app-name &optional frames)
  "Register a new application with the given name and optional frames."
  (let ((app (make-instance 'application
                            :name app-name
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

(defun create-sample-frames (app-name)
  "Create sample frames for testing the application."
  (let ((app (get-application app-name)))
    (when app
      (let* ((editor (make-standard-editor))
             (frame1 (make-standard-frame editor :title "Main Buffer" :x 50 :y 50 :width 500 :height 400)))
        ;; Make the first frame focused by default
        (setf (frame-focused frame1) t)
        (setf (application-frames app) (list frame1))
        app))))

;; Export key functions for external use
(export '(application
          application-name
          application-frames
          register-application
          list-applications
          remove-application
          add-frame-to-application
          remove-frame-from-application
          get-application-frames
          get-application))
