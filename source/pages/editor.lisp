(in-package :star.app)


(defclass editor-app (star-app)
  ((documents :initform (serapeum:dict) :accessor editor-documents)
   (dataset :type string :initform "" :accessor editor-default-dataset)))


(defmethod add-document ((app editor-app) document)
  (setf (editor-documents app) (serapeum:dict* (editor-documents app)
                                               (spec:doc-id document)
                                               document)))


(defmethod add-document ((app editor-app) document)
  (setf (editor-documents app) (serapeum:dict* (editor-documents app)
                                               (spec:doc-id document)
                                               document)))


(defvar *editor-app* (make-instance 'editor-app
                                    :client (api-client *app*)
                                    :app-name "Document Editor"))



(defparameter *document-types*
  (list
   (define-object-form (spec:target)
       (actor :field-type :text)
     (target :field-type :text)
     (delay :field-type :text)
     (recurring-p :field-name "Recurring" :field-type :checkbox)
     (options :field-type :textarea))

   (define-object-form (spec:person)
       (fname :field-type :text)
     (mname :field-type :text)
     (lname :field-type :text)
     (bio :field-type :textarea)
     (dob :field-name "Date of Birth" :field-type :date)
     (region :field-type :text))

   (define-object-form (spec:org)
       (reg :field-type :text)
     (name :field-type :text)
     (bio :field-type :textarea)
     (country :field-type :text)
     (website :field-type :url))

   (define-object-form (spec:email)
       (user :field-type :email)
     (domain :field-type :text)
     (password :field-type :password))

   (define-object-form (spec:user)
       (url :field-type :url)
     (name :field-type :text)
     (platform :field-type :text)
     (bio :field-type :textarea)
     (misc :field-type :key-value))

   (define-object-form (spec:message)
       (content :field-type :textarea)
     (platform :field-type :text)
     (user :field-type :text)
     (is-reply :field-name "Is Reply" :field-type :checkbox)
     (message-id :field-name "Message ID" :field-type :text)
     (reply-to :field-name "Reply To" :field-type :text)
     (group :field-type :text)
     (channel :field-type :text))))


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

(defun create-document-type-select (container)
  (let ((select (create-select container :class "form-select" :style "width: 100%; margin-bottom: 15px;")))
    (create-option select :content "Search for Documents")
    (loop for (type . _) in *document-types*
          do (create-option select :content (string-capitalize type) :value type))
    select))

(defun on-document-editor (body)
  (on-page body "Document Editor")
  (let* ((container (create-div body :class "container" :style "margin-top: 20px;"))
         (row (create-div container :class "columns"))
         (left-pane (create-div row :class "column col-3" :style "border-right: 1px solid #e7e9ed;"))
         (right-pane (create-div row :class "column col-9"))
         (type-select (create-document-type-select left-pane))
         (new-doc-form (create-div left-pane))
         (doc-grid (create-div right-pane :class "columns")))

    (labels ((update-new-doc-form ()
               (setf (inner-html new-doc-form) "")
               (create-document-form new-doc-form (value type-select)))

             (add-document-to-grid (document)
               (let ((card-container (create-div doc-grid :class "column col-3")))
                 (add-document *editor-app* document)
                 (log:info (format nil "ID; ~a" (spec:doc-id document)))
                 (create-document-card card-container document))))

      (update-new-doc-form)
      (set-on-change type-select (lambda (obj) (declare (ignore obj)) (update-new-doc-form)))

      (set-on-submit new-doc-form
                     (lambda (obj)
                       (declare (ignore obj))
                       (let* ((doc-type (value type-select))
                              (class-name (find-symbol (string-upcase doc-type) :spec))
                              (document (make-instance class-name)))
                         (log:info (form-get-data new-doc-form))
                         ;; (loop for
                         ;;       for name = (attribute input "name")
                         ;;       for value = (value input)
                         ;;       for field = (find name (cdr (assoc doc-type *document-types* :test #'string=))
                         ;;                         :key (lambda (f) (getf f :field-name))
                         ;;                         :test #'string=)
                         ;;       when field
                         ;;         do (setf (slot-value document (getf field :slot-name))
                         ;;                  (funcall (getf field :parse-fn) value)))
                         (add-document-to-grid document))))

      ;; Example: Add some initial documents to the grid
      (let ((example-message (from-json (jsown:parse (star.api.client:get-document (api-client *app*) "01J93C097G9KDS2XCYXDTYKYYN")) 'spec:message))
            (example-org (make-instance 'spec:org :name "ACME Corp" :country "USA")))
        (add-document-to-grid example-message)
        (add-document-to-grid example-org)))

    (run body)))
