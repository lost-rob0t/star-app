(defpackage :star.app.documents
  (:use :cl)
  (:export #:document #:message #:target #:document-binding #:document-wire
           #:parse-document #:doc-id #:doc-type #:doc-updated #:field
           #:message-user #:message-content #:message-platform #:message-group
           #:message-channel #:message-is-reply #:target-target
           #:relation-source #:relation-target #:relation-note))
(in-package :star.app.documents)

;; Presentation wrappers hold actual generated bindings. They do not declare
;; wire fields, types, or inheritance for the StarLang document contracts.
(defclass document ()
  ((binding :initarg :binding :reader document-binding)))
(defclass message (document) ())
(defclass target (document) ())

(defun parse-document (input &optional expected-type)
  (let* ((wire (etypecase input
                 (string (com.inuoe.jzon:parse input))
                 (hash-table input)))
         (binding (starintel.canonical:decode-document wire))
         (dtype (gethash "dtype" wire))
         (class (cond ((equal dtype "message") 'message)
                      ((equal dtype "target") 'target)
                      (t 'document))))
    (when (and expected-type
               (not (or (eq expected-type 'document) (eq expected-type class))))
      (error "Expected ~a document; received ~a" expected-type dtype))
    (make-instance class :binding binding)))

(defun document-wire (document)
  (starintel.canonical:encode-document (document-binding document)))
(defun field (document name &optional default)
  (gethash name (document-wire document) default))
(defun doc-id (document) (field document "id"))
(defun doc-type (document) (field document "dtype"))
(defun doc-updated (document) (field document "updatedAt" (field document "createdAt" 0)))
(defun message-user (document)
  (let ((value (field document "user")))
    (if (hash-table-p value) (gethash "id" value) "")))
(defun message-content (document) (field document "message" ""))
(defun message-platform (document) (field document "platform" ""))
(defun message-group (document) (field document "group" ""))
(defun message-channel (document) (field document "channel" ""))
(defun message-is-reply (document) (nth-value 1 (gethash "replyTo" (document-wire document))))
(defun target-target (document) (field document "target"))
(defun relation-source (document) (gethash "id" (field document "source")))
(defun relation-target (document) (gethash "id" (field document "destination")))
(defun relation-note (document) (field document "note" ""))
