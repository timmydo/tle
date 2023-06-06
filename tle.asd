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
               (:file "ui")
               (:file "buffer")
               (:file "editor")
               (:file "window")
	       ;; implementation
	       (:file "sdl2-font")
	       (:file "sdl2-ui")
	       (:file "standard-buffer")
	       (:file "standard-editor")
	       (:file "standard-window")

	       ;; entry point
               (:file "main")
))
