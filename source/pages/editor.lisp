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



(defparameter *document-forms*
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




(defun create-document-type-select (container)
  (let ((select (create-select container :class "form-select" :style "width: 100%; margin-bottom: 15px;")))
    (create-option select :content "Search for Documents")
    (loop for (type . _) in *document-forms*
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
                         ;;       for field = (find name (cdr (assoc doc-type *document-forms* :test #'string=))
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
