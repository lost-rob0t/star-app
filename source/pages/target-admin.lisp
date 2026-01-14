(in-package :star.app)

(defclass target-admin (star-app)
  ((targets :type list :accessor taget-admin-targets)
   (dataset :type string :initform "" :accessor target-admin-dataset)))


(defmethod render-target-item ((target spec:target) clog-obj)
  (let* ((card (create-div clog-obj :class "card" :style "white-space: normal; width: 100%; margin-bottom: 10px;"))
         (card-body (create-div card :class "card-body" :style "display: flex; flex-direction: row;"))
         (chip-container (create-div card-body :style "width: 150px; padding-right: 25px; border-right: 1px solid #e7e9ed;"))
         (content-container (create-div card-body :style "flex: 1; padding-left: 15px;"))
         (content (format nil "~a" (spec:target-target target))))

    ;; Main content on the right
    (let ((type-container (create-div content-container :style "display: flex; align-items: center; margin-bottom: 5px;")))
      (create-span type-container :content "Target" :style "font-weight: bold; font-size: 0.9em;"))

    (create-div content-container :class "text-break"
                                  :style "overflow: hidden; text-overflow: ellipsis; display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; font-size: 0.9em;"
                                  :content content)

    card))
