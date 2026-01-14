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

(defparameter *keys-alist*
  '(("user" . (:doc-render ("dateAdded" "dataset" "name" "url" "platform")
               :result-chips ("dateUpdated" "platform" "dataset")
               :result ("bio")))
    ("socialmpost" . (:doc-render ("dateAdded" "dataset" "user" "group" "channel" "content")
                      :result-chips ("dateUpdated" "platform" "group" "user")
                      :result ("content")))
    ("message" . (:doc-render ("dateAdded" "dataset" "user" "group" "channel" "content")
                  :result-chips ("dateUpdated" "platform" "group" "channel" "user")
                  :result ("content")))))


(defparameter *icon-alist* '(("content" . "bi bi-chat-left-dots-fill")
                             ("user" . "bi bi-person-vcard-fill")
                             ("dataset" . "bi bi-database-fill")
                             ("dateAdded" . "bi bi-calendar-event-fill")
                             ("dateUpdated" . "bi bi-calendar-event-fill")
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



(defmacro define-object-form ((class-name &key (bind-slots t)) &body forms)
  `(cons ,(symbol-name class-name)
    (list
     ,@(loop for form in forms
             collect
             `(list :slot-name ',(car form)
                    :field-name ,(or (getf (cdr form) :field-name)
                                     (string-capitalize (symbol-name (car form))))
                    :field-type ',(getf (cdr form) :field-type)
                    :parse-fn ,(getf (cdr form) :parse-fn `(lambda (value) value))
                    ,@(when bind-slots '(:bind t)))))))

(defun create-key-val-input (form &optional (create-btn nil))
  (let* ((group-input (create-div form :class "input-group"))
         (key-input (create-form-element group-input "text" :class "form-input-sm form-inline"))
         (val-input (create-form-element group-input "text" :class "form-input form-inline"))
         (delete-key (when create-btn (create-button group-input :content "x" :class "btn btn-error input-group-brn form-inline")))
         (add-new (when create-btn (create-button group-input :content "+" :class "btn btn-success input-group-brn form-inline"))))
    group-input))

(defun create-field-input (container field-name field-type &optional value)
  (let* ((form-group (create-div container :class "form-group"))
         (input (case field-type
                  (:key-value (create-key-val-input form-group t))
                  (:textarea (create-form-element form-group :textarea :placeholder field-name :value (or value "i") :class "form-input"))
                  (:checkbox (progn
                               (create-label form-group :content (string-capitalize field-name) :class "form-label")
                               (create-form-element form-group :checkbox :placeholder field-name :checked (or value "") :class "form-checkbox")))
                  (otherwise (create-form-element form-group field-type :placeholder field-name :value (or value "") :class "form-input")))))
    (setf (attribute input "name") field-name)
    input))




(defun create-document-form (container doc-type &optional document (new-form t))
  (let ((form (create-form container :class "form-horizontal"))
        (fields (cdr (assoc doc-type *document-types* :test #'string=))))
    (loop for field in fields
          for slot-name = (getf field :slot-name)
          for field-name = (getf field :field-name)
          for field-type = (getf field :field-type)
          for parse-fn = (getf field :parse-fn)
          for value = (when document (slot-value document slot-name))
          for input = (create-field-input form field-name field-type value)
          do (when document
               (log:info slot-name)
               (log:info field-name)
               (log:info field-type)
               (log:info form)
               ;; How bad is this?
               (eval `(link-form-element-to-slot input document
                       (lambda (obj) (slot-value ,obj ,slot-name))
                       :transform ,parse-fn))))
    (create-button form :content "Add Document" :class "btn btn-primary")
    form))

(defun create-document-card (container document)
  (let* ((card (create-div container :class "card" :style "margin-bottom: 10px;"))
         (card-header (create-div card :class "card-header"))
         (card-body (create-div card :class "card-body" :style "display: none;"))
         (form  (create-document-form card-body (spec:doc-type document) document)))

    (set-on-click card-header
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (style card-body "display")
                          (if (string= (style card-body "display") "none")
                              "block" "none"))))
    card))
