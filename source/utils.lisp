(in-package :star.app)

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
