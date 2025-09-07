(in-package :tle)

(defclass repl-evaluation ()
  ((command :accessor evaluation-command :initarg :command :initform "")
   (result :accessor evaluation-result :initarg :result :initform nil)
   (error-p :accessor evaluation-error-p :initarg :error-p :initform nil)
   (timestamp :accessor evaluation-timestamp :initarg :timestamp :initform (get-universal-time)))
  (:documentation "Represents a REPL evaluation with command, result, and error status."))

(defclass rich-object-view ()
  ((objects :accessor rich-object-view-objects :initform '())
   (title :accessor rich-object-view-title :initform "Objects" :initarg :title))
  (:documentation "A view that displays a list of objects as HTML in a read-only fashion."))

(defgeneric add-object (rich-object-view object)
  (:documentation "Add an object to the rich-object-view."))

(defgeneric clear-objects (rich-object-view)
  (:documentation "Clear all objects from the rich-object-view."))

(defgeneric render-object-as-html (object)
  (:documentation "Render an object as HTML. Should be specialized for different types."))

(defmethod add-object ((view rich-object-view) object)
  "Add an object to the view's object list."
  (push object (rich-object-view-objects view)))

(defmethod clear-objects ((view rich-object-view))
  "Clear all objects from the view."
  (setf (rich-object-view-objects view) '()))

(defmethod render-object-as-html (object)
  "Default implementation - render object as a string with HTML escaping."
  (format nil "<div class=\"object-item\"><pre>~A</pre></div>" 
          (html-escape (prin1-to-string object))))

(defmethod render-object-as-html ((object string))
  "Render strings with simple formatting."
  (format nil "<div class=\"object-item string-object\">\"~A\"</div>" 
          (html-escape object)))

(defmethod render-object-as-html ((object number))
  "Render numbers with number styling."
  (format nil "<div class=\"object-item number-object\">~A</div>" object))

(defmethod render-object-as-html ((object list))
  "Render lists with structure."
  (format nil "<div class=\"object-item list-object\"><pre>~A</pre></div>" 
          (html-escape (prin1-to-string object))))

(defmethod render-object-as-html ((evaluation repl-evaluation))
  "Render a REPL evaluation with command and result."
  (let ((command (html-escape (evaluation-command evaluation)))
        (result (html-escape (prin1-to-string (evaluation-result evaluation))))
        (error-class (if (evaluation-error-p evaluation) " error-result" "")))
    (format nil 
            "<div class=\"object-item repl-evaluation~A\">
               <div class=\"eval-command\"><span class=\"command-prompt\">&gt;</span> ~A</div>
               <div class=\"eval-result\">~A</div>
             </div>"
            error-class
            command
            result)))

(defmethod render ((view rich-object-view) (ui ui-implementation))
  "Render the rich-object-view as HTML."
  (let ((objects (reverse (rich-object-view-objects view))))
    (format nil 
            "<div class=\"rich-object-view\">
               <div class=\"rich-object-view-header\">~A</div>
               <div class=\"rich-object-view-content\">~{~A~}</div>
             </div>"
            (rich-object-view-title view)
            (mapcar #'render-object-as-html objects))))

(defun html-escape (string)
  "Escape HTML special characters in a string."
  (let ((escaped string))
    (setf escaped (substitute-string escaped "&" "&amp;"))
    (setf escaped (substitute-string escaped "<" "&lt;"))
    (setf escaped (substitute-string escaped ">" "&gt;"))
    (setf escaped (substitute-string escaped "\"" "&quot;"))
    escaped))

(defun substitute-string (string old new)
  "Replace all occurrences of OLD with NEW in STRING."
  (let ((result "")
        (old-len (length old))
        (pos 0))
    (loop
      (let ((found (search old string :start2 pos)))
        (if found
            (progn
              (setf result (concatenate 'string result 
                                        (subseq string pos found) 
                                        new))
              (setf pos (+ found old-len)))
            (progn
              (setf result (concatenate 'string result 
                                        (subseq string pos)))
              (return result)))))))