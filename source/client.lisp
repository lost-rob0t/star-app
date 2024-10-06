(in-package :star.app)

(defclass star-app (star-client)
  ((api-client :initarg :client :accessor api-client)
   (base-url :initarg :app-url :accessor app-url)
   (name :initarg :app-name :accessor app-name)
   (settings :initarg :settings :accessor app-settings))


  (:documentation "App class representing state/client for interacting with gserver api"))

;; '((server-url . (:default "http://127.0.0.1:5000")))




;; (defmacro define-settings (app &body forms)
;;   `(progn


;;      `(loop for form in ,forms
;;             collect (cons (car form)
;;                           (list :value (getf form :default))))))



;; (define-setting (app)
;;     (server-url :default "http://127.0.0.1:5000"
;;                 :name "Server Host Url"
;;                 :form-type :url
;;                 :description "The backend star-server url path")
;;   (dataset-filter :default "starintel"
;;                   :name "Filter By Dataset"
;;                   :form-type :text
;;                   :description "Only Show data for this dataset"))

(defun new-star-app (&key
                       (base-url "/")
                       (api-url "http://127.0.0.1:5000")
                       (project-name "StarIntel"))
  (make-instance 'star-app :app-url base-url :client (make-instance 'star-client :base-url api-url)))


(defun format-key (key)
  (if (str:starts-with? "_" key)
      (string-downcase key)
      (str:camel-case key)))


(defun as-json (object &key (format-fn #'format-key))
  (let ((json-obj (jsown:empty-object)))
    (loop for slot in (mapcar #'closer-mop:slot-definition-name
                              (closer-mop:class-slots (class-of object)))
          for value = (slot-value object slot)
          do (setf (jsown:val json-obj (funcall format-fn (string slot)))
                   (typecase value
                     (string value)
                     (integer value)
                     (list (jsown:to-json value))
                     (t (to-json value)))))
    json-obj))

(defun camel-case-to-lisp-case (string)
  (with-output-to-string (s)
    (loop for char across string
          for i from 0
          do (cond
               ((and (not (zerop i))
                     (upper-case-p char))
                (write-char #\- s)
                (write-char (char-downcase char) s))
               (t (write-char (char-downcase char) s))))))

(defun from-json (json-obj class-name &key (format-fn #'format-key))
  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (loop for slot in (sb-mop:class-slots class)
          for slot-name = (sb-mop:slot-definition-name slot)
          for slot-type = (sb-mop:slot-definition-type slot)
          for key = (funcall format-fn (string slot-name))
          for value = (jsown:val-safe json-obj key)
          when value
            do (setf (slot-value object slot-name)
                     (cond
                       ((eq slot-type 'list) value)
                       ((eq slot-type 'string) value)
                       ((eq slot-type 'integer) value)
                       (t (from-json value (eval slot-type))))))
    object))




(defparameter *app* (new-star-app))
