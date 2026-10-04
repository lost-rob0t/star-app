(in-package :star.app)

(defun document-fields (dtype)
  (let* ((schema (starintel.canonical:load-schema))
         (definition (starintel.canonical::document-definition dtype schema))
         (required (coerce (gethash "required" definition) 'list)))
    (loop for key being the hash-keys of (gethash "properties" definition)
          using (hash-value contract)
          collect (list :field-name key :contract contract
                        :required (member key required :test #'equal)))))
(defparameter *document-types*
  (loop for dtype in (starintel.canonical:document-types)
        collect (cons dtype (document-fields dtype))))

(defun field-value-type (contract)
  (let ((reference (gethash "$ref" contract)))
    (if reference
        (field-value-type (gethash (subseq reference (length "#/$defs/"))
                                  (gethash "$defs" (starintel.canonical:load-schema))))
        (gethash "type" contract))))

(defun document-from-fields (dtype values)
  "Create a generated document from exact source wire names and typed inputs."
  (let ((wire (make-hash-table :test #'equal)))
    (setf (gethash "dtype" wire) dtype
          (gethash "schemaVersion" wire) (starintel.canonical:schema-version))
    (dolist (field (cdr (assoc dtype *document-types* :test #'equal)))
      (let* ((name (getf field :field-name))
             (text (cdr (assoc name values :test #'equal)))
             (type (field-value-type (getf field :contract))))
        (unless (member name '("dtype" "schemaVersion") :test #'equal)
          (when (and text (or (getf field :required) (plusp (length text))))
            (setf (gethash name wire)
                  (if (equal type "string") text (com.inuoe.jzon:parse text)))))))
    (from-json wire)))
