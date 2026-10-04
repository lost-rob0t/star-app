(require :asdf)
(asdf:load-system :star-app)
(in-package :star.app)

(let* ((fixture (com.inuoe.jzon:parse (pathname (uiop:getenv "STAR_APP_FIXTURES"))))
       (documents (gethash "documents" fixture))
       (app (new-star-app :api-url (uiop:getenv "STAR_APP_TEST_API"))))
  (assert (= 60 (length documents) (length *document-types*)))
  (loop for wire across documents
        for dtype = (gethash "dtype" wire)
        for document = (from-json (com.inuoe.jzon:stringify wire))
        do (assert (eq (symbol-package (type-of (spec:document-binding document)))
                       (find-package :org.starintel.core.v1)))
           (assert (equalp wire (as-json document)))
           (let* ((fields (document-fields dtype))
                  (values (loop for field in fields
                                for name = (getf field :field-name)
                                for type = (field-value-type (getf field :contract))
                                when (nth-value 1 (gethash name wire))
                                  collect (cons name (if (equal type "string") (gethash name wire)
                                                        (com.inuoe.jzon:stringify (gethash name wire)))))))
             (assert (equalp wire (as-json (document-from-fields dtype values)))))
           (submit-canonical-document app document))
  ;; Unknown/legacy fields, bad versions/references and scalar constraints fail
  ;; at the actual UI boundary before any HTTP request.
  (loop for invalid across (gethash "invalid" fixture)
        do (assert (handler-case (progn (submit-canonical-document app (from-json invalid)) nil)
                     (starintel::starintel-validation-error () t))))
  ;; An invalid modified binding must also fail before the client transport.
  (let ((document (from-json (aref documents 0))))
    (setf (slot-value (spec:document-binding document) 'org.starintel.core.v1::schemaversion) "0.10.2")
    (assert (handler-case (progn (submit-canonical-document app document) nil)
              (starintel::starintel-validation-error () t))))
  (format t "60 actual generated bindings, source-derived forms, lossless codec and HTTP submission; ~d invalid boundaries rejected PASS~%"
          (1+ (length (gethash "invalid" fixture)))))
