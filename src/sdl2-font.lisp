(in-package :tle)

;; special thanks https://github.com/lem-project/lem/blob/main/frontends/sdl2/font.lisp

(defvar *default-font-size* 15)

(defstruct (font-config (:constructor %make-font-config))
  size
  latin-normal-file
  latin-bold-file
  cjk-normal-file
  cjk-bold-file
  emoji-file
  braille-file)

(defstruct font
  latin-normal-font
  latin-bold-font
  cjk-normal-font
  cjk-bold-font
  emoji-font
  braille-font
  char-width
  char-height)

(defun get-resource-pathname (name)
  (asdf:system-relative-pathname 'tle name))

(defun make-font-config (&key (size *default-font-size*)
                              latin-normal-file
                              latin-bold-file
                              cjk-normal-file
                              cjk-bold-file
                              emoji-file
                              brail-file)
  (%make-font-config
   :size (or size *default-font-size*)
   :latin-normal-file (or latin-normal-file
                          (get-resource-pathname "resources/fonts/NotoSansMono-Regular.ttf"))
   :latin-bold-file (or latin-bold-file
                        (get-resource-pathname "resources/fonts/NotoSansMono-Bold.ttf"))
   :cjk-normal-file (or cjk-normal-file
                        (get-resource-pathname "resources/fonts/NotoSansCJK-Regular.ttc"))
   :cjk-bold-file (or cjk-bold-file
                      (get-resource-pathname "resources/fonts/NotoSansCJK-Bold.ttc"))
   :emoji-file (or emoji-file
                   (get-resource-pathname "resources/fonts/NotoColorEmoji.ttf"))
   :braille-file (or brail-file
                     (get-resource-pathname "resources/fonts/FreeMono.ttf"))))

(defun merge-font-config (new old)
  (%make-font-config :size (or (font-config-size new)
                               (font-config-size old))
                     :latin-normal-file (or (font-config-latin-normal-file new)
                                            (font-config-latin-normal-file old))
                     :latin-bold-file (or (font-config-latin-bold-file new)
                                          (font-config-latin-bold-file old))
                     :cjk-normal-file (or (font-config-cjk-normal-file new)
                                          (font-config-cjk-normal-file old))
                     :cjk-bold-file (or (font-config-cjk-bold-file new)
                                        (font-config-cjk-bold-file old))
                     :emoji-file (or (font-config-emoji-file new)
                                     (font-config-emoji-file old))
                     :braille-file (or (font-config-braille-file new)
                                       (font-config-braille-file old))))

(defun change-size (font-config size)
  (let ((font-config (copy-font-config font-config)))
    (setf (font-config-size font-config) size)
    font-config))

(defun get-character-size (font)
  (let* ((surface (sdl2-ttf:render-text-solid font " " 0 0 0 0))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (list width height)))

(defun sdl2-open-font (&optional font-config)
  (let* ((font-config (or font-config (make-font-config)))
         (latin-normal-font (sdl2-ttf:open-font (font-config-latin-normal-file font-config)
                                                (font-config-size font-config)))
         (latin-bold-font (sdl2-ttf:open-font (font-config-latin-bold-file font-config)
                                              (font-config-size font-config)))
         (cjk-normal-font (sdl2-ttf:open-font (font-config-cjk-normal-file font-config)
                                              (font-config-size font-config)))
         (cjk-bold-font (sdl2-ttf:open-font (font-config-cjk-bold-file font-config)
                                            (font-config-size font-config)))
         (emoji-font (sdl2-ttf:open-font (font-config-emoji-file font-config)
                                         (font-config-size font-config)))
         (braille-font (sdl2-ttf:open-font (font-config-braille-file font-config)
                                           (font-config-size font-config))))
    (destructuring-bind (char-width char-height)
        (get-character-size latin-normal-font)
      (make-font :latin-normal-font latin-normal-font
                 :latin-bold-font latin-bold-font
                 :cjk-normal-font cjk-normal-font
                 :cjk-bold-font cjk-bold-font
                 :emoji-font emoji-font
                 :braille-font braille-font
                 :char-width char-width
                 :char-height char-height))))

(defun sdl2-close-font (font)
  (sdl2-ttf:close-font (font-latin-normal-font font))
  (sdl2-ttf:close-font (font-latin-bold-font font))
  (sdl2-ttf:close-font (font-cjk-normal-font font))
  (sdl2-ttf:close-font (font-cjk-bold-font font))
  (sdl2-ttf:close-font (font-emoji-font font))
  (sdl2-ttf:close-font (font-braille-font font))
  (values))
