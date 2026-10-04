(in-package :star.app)

(defparameter *dtype-icon-alist*
  '(("person" . "bi bi-person-fill")
    ("org" . "bi bi-building-fill")
    ("domain" . "bi bi-globe-fill")
    ("port" . "bi bi-door-open-fill")
    ("asn" . "bi bi-clipboard-data-fill")
    ("network" . "bi bi-wifi-fill")
    ("host" . "bi bi-display-fill")
    ("geo" . "bi bi-geo-alt-fill")
    ("address" . "bi bi-house-door-fill")
    ("phone" . "bi bi-telephone-fill")
    ("relation" . "bi bi-link-fill")
    ("scope" . "bi bi-bullseye-fill")
    ("message" . "bi bi-chat-left-text-fill")
    ("socialmpost" . "bi bi-chat-left-dots-fill")
    ("breach" . "bi bi-shield-slash-fill")
    ("dataset" . "bi bi-shield-slash-fill")
    ("email" . "bi bi-envelope-fill")
    ("user" . "bi bi-person-vcard-fill")))

(setf *dtype-icon-alist*
      (loop for dtype in (starintel.canonical:document-types)
            collect (cons dtype (or (cdr (assoc dtype *dtype-icon-alist* :test #'equal))
                                    "bi bi-file-text-fill"))))

(defparameter *keys-alist*
  (loop for (dtype . fields) in *document-types*
        for names = (mapcar (lambda (field) (getf field :field-name)) fields)
        collect (cons dtype (list :doc-render names :result names
                                  :result-chips '("dataset" "updatedAt")))))

(defparameter *icon-alist* '(("content" . "bi bi-chat-left-dots-fill")
                             ("user" . "bi bi-person-vcard-fill")
                             ("dataset" . "bi bi-database-fill")
                             ("createdAt" . "bi bi-calendar-event-fill")
                             ("updatedAt" . "bi bi-calendar-event-fill")
                             ("group" . "bi bi-people-fill")))





(defgeneric document-header (document)
  (:documentation "Define how to set the header phrase for a document element"))


(defgeneric document-render-card (document)
  (:documentation "Render a document info card "))


(defgeneric document-render-search-result (document)
  (:documentation "Render a document search result"))


(defun create-card (container &key title (subtitle nil) content (class nil) (icon-class nil) (chips nil))
  (let* ((card (create-div container :class (format nil "card ~A" (or class ""))))
         (card-header (create-div card :class "card-header"))
         (header-row (create-div card-header :class "columns"))
         (icon-col (create-div header-row :class "column col-auto"))
         (icon (when icon-class (create-phrase icon-col :i :class (format nil "icon ~A" icon-class))))
         (title-col (create-div header-row :class "column"))
         (card-title (create-div title-col :class "card-title h5" :content title))
         (card-subtitle (when subtitle
                          (create-div title-col :class "card-subtitle text-gray" :content subtitle)))
         (card-body (create-div card :class "card-body"))
         (card-content (create-p card-body :class "text-break" :content content))
         (card-footer (create-div card :class "card-footer")))
    (when chips
      (loop for chip in chips
            do (create-span card-footer :class "chip" :content chip)))
    card))


(defun create-search-result (container &rest arguments)
  (apply #'create-card container arguments))

(defun create-search-result-large (container &key title subtitle content class icon-class chips)
  (let* ((card (create-div container :class (format nil "card ~A" (or class ""))))
         (card-header (create-div card :class "card-header"))
         (header-row (create-div card-header :class "columns"))
         (icon-col (create-div header-row :class "column col-auto"))
         (icon (create-phrase icon-col :i :class (format nil "icon ~A" icon-class)))
         (title-col (create-div header-row :class "column"))
         (card-title (create-div title-col :class "card-title h5" :content title))
         (card-subtitle (when subtitle
                          (create-div title-col :class "card-subtitle text-gray" :content subtitle)))
         (card-body (create-div card :class "card-body"))
         (card-content (create-p card-body :class "text-break" :content content))
         (card-footer (create-div card :class "card-footer")))
    (when chips
      (loop for chip in chips
            do (create-span card-footer :class "chip" :content chip)))
    card))

(defgeneric document-render-search-result-small (document container)
  (:documentation "Render a small search result for the document"))

(defgeneric document-render-search-result-large (document container)
  (:documentation "Render a large card-style search result for the document"))

(defmethod document-render-search-result-small ((doc spec:message) container)
  (create-search-result container
                        :title (document-header doc)
                        :subtitle (format nil "~A · ~A"
                                          (spec:message-group doc)
                                          (spec:message-channel doc))
                        :content (spec:message-content doc)
                        :icon-class "icon-message"))

(defmethod document-render-search-result-large ((doc spec:message) container)
  (let ((chips (list (spec:message-platform doc)
                     (spec:message-group doc)
                     (spec:message-channel doc))))
    (when (spec:message-is-reply doc)
      (push "Reply" chips))
    (create-search-result-large
     container
     :title (document-header doc)
     :subtitle (format nil "~A · ~A"
                       (spec:message-group doc)
                       (spec:message-channel doc))
     :content (spec:message-content doc)
     :icon-class "icon-message"
     :chips chips)))

(defmethod document-render-search-result-small ((doc spec:document) container)
  (create-search-result container
                        :title (document-header doc)
                        :subtitle (format nil "Type: ~A" (spec:doc-type doc))
                        :content "No preview available"
                        :icon-class "icon-file"))

(defmethod document-render-search-result-large ((doc spec:document) container)
  (create-search-result-large
   container
   :title (document-header doc)
   :subtitle (format nil "Type: ~A" (spec:doc-type doc))
   :content "No preview available"
   :icon-class "icon-file"
   :chips (list (spec:doc-type doc))))



(defmethod document-header ((document spec:document))
  (format nil "~a: ~a" (spec:doc-type document) (spec:doc-id document)))

(defun create-document-form (container dtype &optional document (editable t) on-document)
  (let ((form (create-form container :class "form-horizontal"))
        (inputs nil)
        (status (create-div container :class "toast")))
    (dolist (field (document-fields dtype))
      (let* ((name (getf field :field-name))
             (type (field-value-type (getf field :contract)))
             (fixed (member name '("dtype" "schemaVersion") :test #'equal))
             (wire (when document (as-json document)))
             (value (cond (fixed (if (equal name "dtype") dtype (starintel.canonical:schema-version)))
                          (wire (multiple-value-bind (value present) (gethash name wire)
                                  (when present (if (equal type "string") value
                                                    (com.inuoe.jzon:stringify value)))))
                          (t "")))
             (group (create-div form :class "form-group")))
        (create-label group :content (format nil "~a~a" name (if (getf field :required) " *" "")))
        (let ((input (create-form-element group
                         (if (equal type "string") :text :textarea)
                         :class "form-input" :name name :value (or value "")
                         :placeholder (if (equal type "string") name "JSON value"))))
          (when (or fixed (not editable)) (setf (attribute input "readonly") "readonly"))
          (push (cons name input) inputs))))
    (when editable
      (create-button form :content "Add Document" :class "btn btn-primary")
      (set-on-submit form
        (lambda (obj)
          (declare (ignore obj))
          (handler-case
              (let ((result (document-from-fields dtype
                              (mapcar (lambda (input) (cons (car input) (value (cdr input)))) inputs))))
                (setf (text status) "Document validated")
                (when on-document (funcall on-document result)))
            (error (condition) (setf (text status) (princ-to-string condition)))))))
    form))

(defun create-document-card (container document)
  (let ((card (create-div container :class "card")))
    (create-div card :class "card-header" :content (document-header document))
    (create-document-form card (spec:doc-type document) document nil)
    card))
