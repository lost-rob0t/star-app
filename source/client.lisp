(in-package :star.app)

(defclass star-app ()
  ((api-client :initarg :client :initform (make-star-client) :accessor api-client)
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
  (make-instance 'star-app :app-url base-url :client (make-star-client :base-url api-url)))


(defun as-json (document)
  (spec:document-wire document))

(defun from-json (input &optional (class-name 'spec:document))
  (spec:parse-document input class-name))

(defun submit-canonical-document (app document)
  ;; Validate and encode before any HTTP request.
  (let ((wire (as-json document)))
    (submit-document (api-client app) (com.inuoe.jzon:stringify wire)
                     (gethash "dtype" wire))))

(defparameter *app* (new-star-app))
