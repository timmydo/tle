(asdf:defsystem "tle"
  :description "Lisp Environment"
  :author "Timmy Douglas <mail@timmydouglas.com>"
  :license  "MPL2"
  :version "0.0.1"
  :depends-on ("trivial-gray-streams"
	       "cffi")
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
	       (:file "ui-sdl2")
	       (:file "buffer-standard")
	       (:file "editor-standard")
	       (:file "window-standard")

	       ;; entry point
               (:file "main")
))
