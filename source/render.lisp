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



(defparameter *document-types*
  '(("target" . (("actor" . :text)
                 ("target" . :text)
                 ("delay" . :text)
                 ("recurring" . :checkbox)
                 ("options" . :textarea)))
    ("person" . (("fname" . :text)
                 ("mname" . :text)
                 ("lname" . :text)
                 ("bio" . :textarea)
                 ("dob" . :date)
                 ("region" . :text)))
    ("org" . (("reg" . :text)
              ("name" . :text)
              ("bio" . :textarea)
              ("country" . :text)
              ("website" . :url)))
    ("email" . (("user" . :email)
                ("domain" . :text)
                ("password" . :password)))
    ("user" . (("url" . :url)
               ("name" . :text)
               ("platform" . :text)
               ("bio" . :textarea)
               ("misc" . :key-value)))
    ("message" . (("content" . :textarea)
                  ("platform" . :text)
                  ("user" . :text)
                  ("is-reply" . :checkbox)
                  ("message-id" . :text)
                  ("reply-to" . :text)
                  ("group" . :text)
                  ("channel" . :text)))))

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

(defgeneric document-render-search-result-small (document)
  (:documentation "Render a small search result for the document"))

(defgeneric document-render-search-result-large (document)
  (:documentation "Render a large card-style search result for the document"))

(defmethod document-render-search-result-small ((doc spec:message))
  (lambda (container)
    (create-search-result container
                          :title (document-header doc)
                          :subtitle (format nil "~A · ~A"
                                            (spec:message-group doc)
                                            (spec:message-channel doc))
                          :content (spec:message-content doc)
                          :icon-class "icon-message")))

(defmethod document-render-search-result-large ((doc spec:message))
  (lambda (container)
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
       :chips chips))))

(defmethod document-render-search-result-small ((doc spec:document))
  (lambda (container)
    (create-search-result container
                          :title (document-header doc)
                          :subtitle (format nil "Type: ~A" (spec:doc-type doc))
                          :content "No preview available"
                          :icon-class "icon-file")))

(defmethod document-render-search-result-large ((doc spec:document))
  (lambda (container)
    (create-search-result-large
     container
     :title (document-header doc)
     :subtitle (format nil "Type: ~A" (spec:doc-type doc))
     :content "No preview available"
     :icon-class "icon-file"
     :chips (list (spec:doc-type doc)))))


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
