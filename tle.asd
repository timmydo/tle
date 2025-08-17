(asdf:defsystem "tle"
  :description "Lisp Environment"
  :author "Timmy Douglas <mail@timmydouglas.com>"
  :license  "MPL2"
  :version "0.0.1"
  :depends-on ("jsown" "usocket" "bordeaux-threads")
  :pathname "src"
  :serial t
  :components (
               (:file "tle-package")
               (:file "tle-user-package")

	       ;; interface
               (:file "ui") ;; ui toolkit
               (:file "buffer") ;; a list of lines of text
               (:file "editor") ;; total editor state. list of buffers and windows.
               (:file "frame") ;; a frame displayed by the ui
               (:file "view") ;; a buffer with scroll position, point/mark
               (:file "application") ;; application with frames and editor
	       ;; implementation
	       (:file "web-ui")

	       ;; entry point
               (:file "main")
))
