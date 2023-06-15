(in-package :tle)


(defclass sdl2-ui (ui-implementation)
  ((name :initform "SDL2" :reader name)
   (%font :initform nil :accessor sdl-font))
  (:documentation "A UI implementation"))

(defstruct sdl-context
  window
  renderer
  texture)


(defun make-sdl2-ui ()
  (make-instance 'sdl2-ui))

(defun initialize-editor-windows (windows)
  (dolist (w windows)
    (let* ((window (sdl2:create-window :title "TLE" :w 800 :h 600 :flags '(:shown :resizable)))
	   (renderer (sdl2:create-renderer window nil '(:accelerated)))
	   (ctx (make-sdl-context :window window :renderer renderer)))
      (setf (window-handle w) ctx))))

(defun destroy-editor-windows (windows)
  (dolist (w windows)
    (let* ((handle (window-handle w))
	   (renderer (sdl-context-renderer handle))
	   (win (sdl-context-window handle))
	   (texture (sdl-context-texture handle)))
      (when texture
	(format t "destroy texture ~S~%" texture)
	(sdl2:destroy-texture texture))
      (when renderer
	(format t "destroy renderer ~S~%" renderer)
	(sdl2:destroy-renderer renderer))
      (when win
	(format t "destroy win ~S~%" win)
	(sdl2:destroy-window win)))))

(defun paint-editor-windows (ui windows)
  (dolist (w windows)
      (let* ((ctx (window-handle w))
	     (renderer (sdl-context-renderer ctx))
	     (sdlwin (sdl-context-window ctx)))
	(sdl2:set-render-draw-color renderer 0 0 0 255)
	(sdl2:render-clear renderer)
	(draw-window w ui)
	(sdl2:render-present renderer)
	)))

(defmethod ui-window-size (window (ui sdl2-ui))
  (sdl2:get-window-size (sdl-context-window (window-handle window))))

(defmethod ui-draw-text (window (ui sdl2-ui) text rect)
  ;; (format t "ui-draw-text ~S ~S ~S ~S ~%" window ui text rect)
  (values (length text) 10)
  (let* ((ctx (window-handle window))
	 (renderer (sdl-context-renderer ctx))
	 (surface (sdl2-ttf:render-utf8-blended (font-latin-normal-font (sdl-font ui))
						text
						255
						255
						255
						0))
	 (texture (sdl2:create-texture-from-surface renderer surface))
	 (render-width (sdl2:texture-width texture))
	 (render-height (sdl2:texture-height texture))
	 (destination-rect (sdl2:make-rect (rect-x rect)
					   (rect-y rect)
					   (min render-width (rect-width rect))
					   (min render-height (rect-height rect)))))
    (sdl2:free-surface surface)
    (sdl2:render-copy renderer texture :source-rect (cffi:null-pointer) :dest-rect destination-rect)
    (sdl2:destroy-texture texture)
    (values render-width render-height)))

(defun paint-window (ui window)
  (let* ((ctx (window-handle window))
	 (renderer (sdl-context-renderer ctx))
	 (sdlwin (sdl-context-window ctx)))
    (multiple-value-bind (width height) (sdl2:get-window-size sdlwin)
      ;; (format t "win width ~S height ~S ~%" width height)
      (unless (sdl-context-texture ctx)
	(setf (sdl-context-texture ctx)
	      (let* ((surface (sdl2-ttf:render-utf8-blended (font-latin-normal-font (sdl-font ui))
							    "hello world"
							    255
							    255
							    255
							    0))
		     (texture (sdl2:create-texture-from-surface renderer surface)))
		(sdl2:free-surface surface)
		texture)))
      
      (let* ((texture (sdl-context-texture ctx))
	     (destination-rect (sdl2:make-rect (round (- 150 (/ (sdl2:texture-width texture) 2.0)))
					       (round (- 150 (/ (sdl2:texture-height texture) 2.0)))
					       (sdl2:texture-width texture)
					       (sdl2:texture-height texture))))
	

	;; paint
	(sdl2:set-render-draw-color renderer 0 0 0 255)
	(sdl2:render-clear renderer)
	(sdl2:render-copy renderer texture :source-rect (cffi:null-pointer) :dest-rect destination-rect)
	(sdl2:render-present renderer)
	))))

(defmethod run-ui ((ui sdl2-ui) (editor editor))
  (sdl2:with-init (:everything)
    (sdl2-ttf:init)
    (unwind-protect
	 (labels ((repaint () (paint-editor-windows ui (editor-windows editor))))
	   (let ((font (sdl2-open-font)))
	     (setf (sdl-font ui) font)
	     (initialize-editor-windows (editor-windows editor))
	     (sdl2:with-event-loop (:method :poll)
	       (:idle ()
		      (sleep (/ 1 60)))
	       (:textinput (:text text)
			   (on-textinput ui text)
			   (repaint))
	       (:textediting (:text text)
			     (on-textediting ui text)
			     (repaint))
	       (:keydown (:keysym keysym)
			 (on-keydown ui (keysym-to-sdl2-key-event keysym))
			 (repaint))
	       (:keyup (:keysym keysym)
		       (on-keyup ui (keysym-to-sdl2-key-event keysym))
		       (repaint))
	       (:mousebuttondown (:button button :x x :y y :clicks clicks)
				 (on-mouse-button-down ui button x y clicks)
				 (repaint))
	       (:mousebuttonup (:button button :x x :y y)
			       (on-mouse-button-up ui button x y)
			       (repaint))
	       (:mousemotion (:x x :y y :state state)
			     (on-mouse-motion ui x y state))
	       (:mousewheel (:x x :y y :which which :direction direction)
			    (on-mouse-wheel ui x y which direction)
			    (repaint))
	       (:windowevent (:event event)
			     (on-windowevent ui event)
			     (repaint))
	       (:quit ()
		      (format t "quitting~%")
		      (sdl2-close-font font)
		      t))))
      (destroy-editor-windows (editor-windows editor))
      
      (sdl2-ttf:quit))))

(defun char-width (ui)
  (font-char-width (sdl-font ui)))

(defun char-height (ui)
  (font-char-height (sdl-font ui)))

(defmethod ui-character-height (window (ui sdl2-ui))
   (font-char-height (sdl-font ui)))

(defstruct (keyinfo (:type list))
  keycode
  sym
  text-input-p)

(defparameter *code-name-table*
  `((,sdl2-ffi:+sdlk-backspace+ "Backspace" nil)
    (,sdl2-ffi:+sdlk-tab+ "Tab" nil)
    (,sdl2-ffi:+sdlk-return+ "Return" nil)
    (,sdl2-ffi:+sdlk-insert+ "Insert" nil)
    (,sdl2-ffi:+sdlk-delete+ "Delete" nil)
    (,sdl2-ffi:+sdlk-space+ "Space" t)
    (,sdl2-ffi:+sdlk-home+ "Home" nil)
    (,sdl2-ffi:+sdlk-end+ "End" nil)
    (,sdl2-ffi:+sdlk-pageup+ "PageUp" nil)
    (,sdl2-ffi:+sdlk-pagedown+ "PageDown" nil)
    (,sdl2-ffi:+sdlk-escape+ "Escape" nil)
    (,sdl2-ffi:+sdlk-left+ "Left" nil)
    (,sdl2-ffi:+sdlk-right+ "Right" nil)
    (,sdl2-ffi:+sdlk-up+ "Up" nil)
    (,sdl2-ffi:+sdlk-down+ "Down" nil)
    (,sdl2-ffi:+sdlk-f1+ "F1" nil)
    (,sdl2-ffi:+sdlk-f2+ "F2" nil)
    (,sdl2-ffi:+sdlk-f3+ "F3" nil)
    (,sdl2-ffi:+sdlk-f4+ "F4" nil)
    (,sdl2-ffi:+sdlk-f5+ "F5" nil)
    (,sdl2-ffi:+sdlk-f6+ "F6" nil)
    (,sdl2-ffi:+sdlk-f7+ "F7" nil)
    (,sdl2-ffi:+sdlk-f8+ "F8" nil)
    (,sdl2-ffi:+sdlk-f9+ "F9" nil)
    (,sdl2-ffi:+sdlk-f10+ "F10" nil)
    (,sdl2-ffi:+sdlk-f11+ "F11" nil)
    (,sdl2-ffi:+sdlk-f12+ "F12" nil)))

(defun convert-to-sym (code)
  (let ((keyinfo (assoc code *code-name-table*)))
    (if keyinfo
        (values (keyinfo-sym keyinfo) (keyinfo-text-input-p keyinfo))
        (when (<= code #x110000)
          (values (string (code-char code))
                  t)))))

(defun make-key (&key ctrl meta shift super sym)
  (when (equal sym (string #\yen_sign))
    (setf sym "\\"))
  (cond ((and ctrl (equal sym "i"))
         (make-key :ctrl nil
                   :meta meta
                   :super super
                   :shift shift
                   :sym "Tab"))
        ((and ctrl (equal sym "m"))
         (make-key :ctrl nil
                   :meta meta
                   :super super
                   :shift shift
                   :sym "Return"))
        (t
         (make-key :ctrl ctrl
                   :meta meta
                   :super super
                   :shift shift
                   :sym sym))))

(defstruct modifier
  shift
  ctrl
  meta
  super)

(defstruct (sdl2-key-event (:constructor %make-sdl2-key-event))
  code
  modifier)

(defun make-sdl2-key-event (code modifier)
  (%make-sdl2-key-event :code code :modifier modifier))

(defun keysym-to-sdl2-key-event (keysym)
  (let ((code (sdl2:sym-value keysym))
        (modifier (get-modifier keysym)))
    (make-sdl2-key-event code modifier)))

(defun mod-p (mod value)
  (= value (logand value mod)))

(defun get-modifier (keysym)
  (let* ((mod (sdl2:mod-value keysym))
         (shift (or (mod-p mod sdl2-ffi:+kmod-lshift+)
                    (mod-p mod sdl2-ffi:+kmod-rshift+)))
         (ctrl (or (mod-p mod sdl2-ffi:+kmod-lctrl+)
                   (mod-p mod sdl2-ffi:+kmod-rctrl+)))
         (meta (or (mod-p mod sdl2-ffi:+kmod-lalt+)
                   (mod-p mod sdl2-ffi:+kmod-ralt+)))
         (super (or (mod-p mod sdl2-ffi:+kmod-lgui+)
                    (mod-p mod sdl2-ffi:+kmod-rgui+))))
    (make-modifier :shift shift :ctrl ctrl :meta meta :super super)))

(defun update-modifier (modifier new-modifier)
  (setf (modifier-shift modifier) (modifier-shift new-modifier))
  (setf (modifier-ctrl modifier) (modifier-ctrl new-modifier))
  (setf (modifier-meta modifier) (modifier-meta new-modifier))
  (setf (modifier-super modifier) (modifier-super new-modifier)))

(defvar *modifier* (make-modifier))

(defvar *cursor-shown* t)
(defun show-cursor ()
  (setf *cursor-shown* t)
  (sdl2:show-cursor))
(defun hide-cursor ()
  (setf *cursor-shown* nil)
  (sdl2:hide-cursor))


(defun on-windowevent (ui event)
  #+nil(alexandria:switch (event)
	 (sdl2-ffi:+sdl-windowevent-shown+
	  (notify-required-redisplay))
	 (sdl2-ffi:+sdl-windowevent-exposed+
	  (notify-required-redisplay))
	 (sdl2-ffi:+sdl-windowevent-resized+
	  (update-texture *display*)
	  (notify-required-redisplay))
	 (sdl2-ffi:+sdl-windowevent-focus-gained+
	  (setf (display-focus-p *display*) t))
	 (sdl2-ffi:+sdl-windowevent-focus-lost+
	  (setf (display-focus-p *display*) nil)))
  (format t "window event ~S~%" event)
  )


(defun on-mouse-button-down (ui button x y clicks)
  (show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2)
                ((eql button 4) :button-4))))
    (when button
      (let ((x (floor x (char-width ui)))
            (y (floor y (char-height ui))))
        (format t "receive-mouse-button-down ~S ~S ~S ~S ~%" x y button clicks)))))

(defun on-mouse-button-up (ui button x y)
  (show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2)
                ((eql button 4) :button-4)))
        (x (floor x (char-width ui)))
        (y (floor y (char-height ui))))
    (format t "receive-mouse-button-up ~S ~S ~S~%" x y button)))

(defun on-mouse-motion (ui x y state)
  (show-cursor)
  (let ((button (if (= sdl2-ffi:+sdl-button-lmask+ (logand state sdl2-ffi:+sdl-button-lmask+))
                    :button-1
                    nil)))
    (let ((x (floor x (char-width ui)))
          (y (floor y (char-height ui))))
      (format t "receive-mouse-motion ~S ~S ~S ~%" x y button))))

(defun on-mouse-wheel (ui wheel-x wheel-y which direction)
  (declare (ignore which direction))
  (show-cursor)
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (let ((x (floor x (char-width ui)))
          (y (floor y (char-height ui))))
      (format t "receive-mouse-wheel ~S ~S ~S ~S~%" x y wheel-x wheel-y)
					; redraw?
      )))

(defun on-textediting (ui text)
  (format t "textedit: ~S~%" text)
  )

(defun on-textinput (ui value)
  (hide-cursor)
  (let ((text (etypecase value
                (integer (string (code-char value)))
                (string value))))
    (format t "handle-text-input ~S:%" text)))

(defun on-keydown (ui key-event)
  (hide-cursor)
  (format t "keydown ~S~%" key-event))

(defun on-keyup (ui key-event)
  (format t "keyup ~S~%" key-event))
