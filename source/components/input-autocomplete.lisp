(in-package :star.app)

(defun create-tile (clog-obj &key (class "tile tile-centered") (icon nil) content))

(defclass auto-complete-form ()
  ((items :initarg :items :initform nil :accessor form-completion-data)
   (current-input :initarg :input :initform "" :accessor current-input)
   (input :initform nil :initarg :input-element :accessor form-input)
   (completion-list :initform nil :accessor completion-list)
   (complete-min :initform 2 :initarg :complete-min-chars :accessor complete-min)
   (on-select :initform nil :initarg :on-select :accessor on-select-callback))
  (:documentation "Auto-complete form class"))

(defmethod add-item ((form auto-complete-form) item)
  (setf (form-completion-data form) (remove-duplicates (append (form-completion-data form) (list item)) :test #'string=)))




(defmethod render-items ((form auto-complete-form))
  (let ((menu (completion-list form)))
    (setf (inner-html menu) "")
    (loop for item in (form-completion-data form)
          when (and (> (length (current-input form)) (1- (complete-min form)))
                    (search (current-input form) item :test #'char-equal))
            do (let ((item-element (create-element menu "li" :class "menu-item")))
                 (setf (inner-html item-element) (format nil "<a href=\"#\">~A</a>" item))

                 (set-on-click item-element
                               (lambda (obj)
                                 (declare (ignore obj))
                                 (setf (value (form-input form)) item)
                                 (setf (current-input form) item)
                                 (setf (hidden menu) t)
                                 (when (on-select-callback form)
                                   (funcall (on-select-callback form) item))))))))

(defun create-auto-complete-form (clog-obj &key (items nil) (style "form-input") (placeholder "") (on-select nil))
  (let* ((container (create-div clog-obj :class "form-autocomplete"))
         (input-container (create-div container :class "form-autocomplete-input"))
         (input (create-form-element input-container :text :class style :placeholder placeholder))
         (menu (create-element container "ul" :class "menu"))
         (form (make-instance 'auto-complete-form
                              :items items
                              :input-element input
                              :complete-min-chars 2
                              :on-select on-select)))
    (setf (completion-list form) menu)
    (setf (hiddenp menu) t)

    (set-on-input input
                  (lambda (obj)
                    (setf (current-input form) (value obj))
                    (if (>= (length (current-input form)) (complete-min form))
                        (progn
                          (render-items form)
                          (setf (hidden menu) nil))
                        (setf (hidden menu) t))))

    (set-on-blur input
                 (lambda (obj)
                   (declare (ignore obj))
                   (sleep 0.2)
                   (setf (hiddenp menu) t)))

    form))
