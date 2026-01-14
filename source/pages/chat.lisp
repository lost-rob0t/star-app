(in-package :star.app)

(defclass chat-channel ()
  ((channel-name :initform "" :type string :accessor channel-name)
   (messages :initform (serapeum:dict) :accessor channel-messages)))


(defclass chat-app (star-app)
  ((groups :initform (serapeum:dict) :accessor chat-groups)
   (current-group :initform nil :accessor current-group)
   (current-channel :initform nil :accessor current-channel)
   (messages :initform nil :accessor chat-messages)
   (last-message-id :initform nil :accessor last-message-id)))

(defvar *chat-app* (make-instance 'chat-app
                                  :client (api-client *app*)
                                  :app-name "Chat View"))


(defun render-message-title (doc)
  (format nil "~a ~a" (spec:message-user doc) (local-time:format-timestring nil (local-time:unix-to-timestamp (spec:doc-updated doc)) :format (list :month "/" :day "/" :year "@" :hour ":" :min))))


(defmethod render-chat-message ((doc spec:message) root-obj)
  (let ((container (clog:create-div root-obj)))
    (create-card container
                 :title (render-message-title doc)
                 :subtitle ""
                 :content (if (= 0 (length (spec:message-content doc))) "No Content" (spec:message-content doc))
                 :icon-class "icon-message")))

(defun fetch-groups (app &key (group-name nil))
  (loop for skip from 0 by 100
        for result = (jsown:parse (if group-name
                                      (star.api.client:groups (api-client app) :start-key group-name :limit 100 :skip skip)
                                      (star.api.client:groups (api-client app) :limit 100 :skip skip)))
        while (car (jsown:val-safe result "result"))
        do (log:info result)
        do
           (mapcar (lambda (x)
                     (setf (chat-groups app) (serapeum:dict* (chat-groups app) (jsown:val x "key") (remove-duplicates (jsown:val x "value") )))) result)))


(defun fetch-channels (app group)
  (remove-duplicates (serapeum:@ (chat-groups app) group) :test #'string=))

(defun fetch-messages (app group channel &key (limit 200) (start-key nil))
  (setf (chat-messages app)

        (loop for doc in (jsown:parse (messages-by-channel (api-client app) group channel
                                                           :limit limit
                                                           :start-key start-key))
              collect (from-json doc 'spec:message))))


(defun on-chat-view (body)
  (on-page body "Chat View")
  (let* ((app *chat-app*)
         (container (create-div body :class "container-fluid" :style "height: 100vh;"))
         (row (create-div container :class "columns" :style "height: 100%;"))
         (left-pane (create-div row :class "column col-3" :style "border-right: 1px solid #e7e9ed; height: 100%; overflow-y: auto;"))
         (right-pane (create-div row :class "column col-9" :style "height: 100%; display: flex; flex-direction: column;"))
         (group-filter (create-form-element left-pane :text :class "form-input" :placeholder "Filter groups..."))
         (group-list (create-div left-pane :class "menu"))
         (message-container (create-div right-pane :style "flex-grow: 1; overflow-y: auto;"))
         (message-input (create-form-element right-pane :text :class "form-input" :placeholder "Type a message...")))

    (setf (style group-list "menu") "loading-lg")
    (setf (style group-filter "menu") "loading-lg")
    (setf (style message-container "menu") "loading-lg")
    (fetch-groups app)
    (log:debug (chat-groups app))
    (labels ((render-groups ()
               (setf (inner-html group-list) "")
               (serapeum:do-hash-table (group channels (chat-groups app))
                 (let* ((group-item (create-div group-list :class "menu-item"))
                        (group-name (create-a group-item :content group))
                        (channel-list (create-div group-item :class "menu" :style "display: none;")))
                   (set-on-click group-name
                                 (lambda (obj)
                                   (declare (ignore obj))
                                   (setf (style channel-list "display")
                                         (if (string= (style channel-list "display") "none")
                                             "block" "none"))
                                   (when (string= (style channel-list "display") "block")
                                     (render-channels group channel-list)))))))

             (render-channels (group container)
               (setf (inner-html container) "")
               (loop for channel in (fetch-channels app group)
                     do (let ((channel-item (create-a container :content channel :class "menu-item")))
                          (set-on-click channel-item
                                        (lambda (obj)
                                          (declare (ignore obj))
                                          (setf (current-group app) group)
                                          (setf (current-channel app) channel)
                                          (fetch-messages app group channel :limit 500)
                                          (render-messages))))))

             (render-messages ()
               (setf (inner-html message-container) "")
               (let ((messages (chat-messages app)))
                 (log:info messages)

                 (loop for message in (sort messages #'string< :key #'spec:doc-id)
                       do (render-chat-message message message-container))))

             (fetch-new-messages ()
               (when (and (current-group app) (current-channel app))
                 (let* ((messages (fetch-messages (api-client app)
                                                  (getf (current-group app) :id)
                                                  (getf (current-channel app) :id)
                                                  :limit 200
                                                  :start-key (last-message-id app)))
                        (new-messages (remove-if (lambda (msg)
                                                   (member msg (gethash (current-channel app) (chat-messages app))
                                                           :test #'string=
                                                           :key #'spec:doc-id))
                                                 messages)))
                   (when new-messages
                     (setf (gethash (list (current-group app) (current-channel app)) (chat-messages app))
                           (append (gethash (list (current-group app) (current-channel app)) (chat-messages app))
                                   new-messages))
                     (setf (last-message-id app) (spec:doc-id (first new-messages)))
                     (render-messages))))))

      (render-groups)

      (set-on-input group-filter
                    (lambda (obj)
                      (let ((filter-text (value obj)))
                        (fetch-groups app :group-name filter-text))))
      ;; (loop for child across (children group-list)
      ;;       do (setf (style child "display")
      ;;                (if (search filter-text (inner-html child) :test #'char-equal)
      ;;                    "block" "none")))


      (set-on-click message-input
                    (lambda (obj)
                      (declare (ignore obj))
                      (when (and (current-group app) (current-channel app))
                        (let ((message-text (value message-input)))
                          (when (not (string= message-text ""))
                            ;; Implement sending message to the API
                            (setf (value message-input) "")
                            (fetch-new-messages))))))))


  (run body))

