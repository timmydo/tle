(asdf:defsystem "tle"
  :description "Lisp Environment"
  :author "Timmy Douglas <mail@timmydouglas.com>"
  :license  "MPL2"
  :version "0.0.1"
  :depends-on ("sdl2" "sdl2-ttf" "sdl2-image")
  :pathname "src"
  :serial t
  :components (
               (:file "tle-package")
               (:file "tle-user-package")

	       ;; interface
               (:file "ui") ;; ui toolkit
               (:file "buffer") ;; a list of lines of text
               (:file "editor") ;; total editor state. list of buffers and windows.
               (:file "window") ;; a window displayed by the ui
               (:file "view") ;; a buffer with scroll position, point/mark
	       ;; implementation
	       (:file "sdl2-font")
	       (:file "sdl2-ui")
	       (:file "standard-buffer")
	       (:file "standard-editor")
	       (:file "standard-window")
	       (:file "standard-view")

	       ;; entry point
               (:file "main")
))
