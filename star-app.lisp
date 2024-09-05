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
    ("email" . "bi bi-envelope-fill")
    ("user" . "bi bi-person-vcard-fill")))

(defparameter *keys-alist*
  '(("message" . (:doc-render ("dateAdded" "dataset" "user" "group" "channel" "content")
                  :result-chips ("dateUpdated" "platform" "group" "channel" "user")
                  :result ("content")))))


(defparameter *icon-alist* '(("content" . "bi bi-chat-left-dots-fill")
                             ("user" . "bi bi-person-vcard-fill")
                             ("dataset" . "bi bi-database-fill")
                             ("dateAdded" . "bi bi-calendar-event-fill")
                             ("dateUpdated" . "bi bi-calendar-event-fill")
                             ("group" . "bi bi-people-fill")))

(defun render-doc-result (doc results)
  (let* ((dtype (jsown:val doc "dtype"))
         (render-info (assoc dtype *dtype-icon-alist* :test #'equal))
         (keys-info (cdr (assoc dtype *keys-alist* :test #'equal)))
         (result-keys (getf keys-info :result))
         (result-chip-keys (getf keys-info :result-chips)))
    (when (and render-info keys-info)
      (let* ((card (create-div results :class "card" :style "white-space: normal; width: 100%; height: 150px; overflow: hidden; display: flex; flex-direction: column;"))
             (card-header (create-div card :class "card-header"))
             (card-img (create-phrase (create-div card-header :class "card-image") :i :class (cdr render-info)))
             (card-body (create-div card :class "card-body" :style "height: 100%; overflow: hidden; display: flex; flex-direction: column;"))
             (content (format nil "~{~a~^ · ~}" (loop for key in result-keys collect (jsown:val doc key))))
             (content-container (create-div card-body :style "flex: 1; overflow: hidden;")))
        (when (> (length content) 0)
          (create-div content-container :class "text-break" :style "max-height: 100%; overflow: hidden; text-overflow: ellipsis; display: -webkit-box; -webkit-line-clamp: 3; -webkit-box-orient: vertical;" :content content)
          (when result-chip-keys
            (let ((chip-container (create-div card-body :class "chip-container" :style "margin-top: auto;")))
              (loop for key in result-chip-keys
                    for value = (jsown:val doc key)
                    when value
                      do (let* ((chip (create-span chip-container :class "chip"))
                                (icon (create-phrase chip :i :class (cdr (assoc key *icon-alist* :test #'equal))))
                                (text (create-span chip :content value)))
                           (declare (ignore icon text))))))
          card)))))

(defun render-doc (doc root-obj)
  (let* ((dtype (jsown:val doc "dtype"))
         (render-info (assoc dtype *dtype-icon-alist* :test #'equal))
         (keys-info (cdr (assoc dtype *keys-alist* :test #'equal)))
         (doc-render-keys (getf keys-info :doc-render)))
    (when (and render-info keys-info)
      (let* ((modal (create-div root-obj :class "modal active"))
             (container (create-div modal :class "modal-container"))
             (header (create-div container :class "modal-title h5" :content (format nil "Veiwing ~a" (jsown:val doc "_id"))))
             (close (create-a header :class "btn btn-clear float-right" :content "close")))

        (set-on-click close (lambda (obj)
                              (setf (css-class-name obj) "modal")))
        (create-div container :class "modal-body")
        (apply #'render-doc-tiles doc root-obj *icon-alist* doc-render-keys)
        modal))))

(defun render-doc-tiles (doc root-obj icon-alist &rest keys)
  (let* ((column1 (create-div root-obj :class "columns column col-6"))
         (column2 (create-div root-obj :class "columns column col-6")))
    (dolist (key keys)
      (let ((value (jsown:val doc key)))
        (when value
          (create-phrase column1 :i :style (cdr (assoc key icon-alist :test #'equal)))
          (let* ((tile (create-div column2 :class "tile tile-centered"))
                 (tile-content (create-div tile :class "tile-content")))
            (create-span tile-content :class "tile-subtitle" :content value)
            (create-div tile-content :class "tile-title" :content (str:title-case key))
            tile))))))


(defclass app ()
  ((current-query :accessor current-query :initform "")
   (fts-bookmark :accessor fts-bookmark :initform nil)))

(defun on-search-page (body)
  (let ((app (make-instance 'app)))
    (load-css (html-document body) "https://unpkg.com/spectre.css/dist/spectre.min.css")
    (load-css (html-document body) "https://unpkg.com/spectre.css/dist/spectre-exp.min.css")
    (load-css (html-document body) "https://unpkg.com/spectre.css/dist/spectre-icons.min.css")
    (load-css (html-document body) "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css")
    (setf (title (html-document body)) "Search")

    (let* ((container (create-div body :class "container" :style "text-align: center;"))
           (search-form (create-form container :method :get :action "http://127.0.0.1:5000/search" :class "input-group" :style "max-width: 500px; margin: 0 auto;"))
           (search-input (create-form-element search-form "text" :name "q" :placeholder "Search..." :class "form-input"))
           (search-button (create-button search-form :class "btn btn-primary input-group-btn" :content "Search"))
           (cols (create-div container :class "columns" :style "margin-top: 20px;"))
           (results-col (create-div cols :class "column col-9" :style "margin: 0 auto;"))
           (results (create-div results-col :class "container results-container"))
           (load-more-button (create-button container :class "btn btn-primary" :content "Load More Results" :style "margin-top: 20px;")))


      (set-on-submit search-form
                     (lambda (obj)
                       (declare (ignore obj))
                       (let* ((query (current-query app))
                              (response (dex:get (quri:make-uri :host "127.0.0.1" :port 5000 :scheme "http" :path "/search" :query (quri:url-encode-params (list (cons "q" query) '("limit" . 2) '("update" . "false"))))))
                              (parsed-response (jsown:parse response))
                              (rows (jsown:val-safe parsed-response "rows"))
                              (bookmark (jsown:val parsed-response "bookmark")))
                         (setf (fts-bookmark app) bookmark)
                         (loop for row in rows
                               for doc = (jsown:val row "doc")
                               do (render-doc-result doc results app))
                         (force-output t))))

      (set-on-click load-more-button
                    (lambda (obj)
                      (declare (ignore obj))
                      (let* ((query (current-query app))
                             (response (dex:get (quri:make-uri :host "127.0.0.1" :port 5000 :scheme "http" :path "/search" :query (quri:url-encode-params (list (cons "q" query) '("limit" . 2) (cons "bookmark" (fts-bookmark app)) '("update" . "false"))))))
                             (parsed-response (jsown:parse response))
                             (rows (jsown:val parsed-response "rows"))
                             (bookmark (jsown:val parsed-response "bookmark")))

                        (setf (fts-bookmark app) bookmark)
                        (loop for row in rows
                              for doc = (jsown:val row "doc")
                              do (render-doc-result doc results app))
                        (force-output t)))))))







(defun main ()
  (initialize 'on-search-page :port 2233)
  (set-on-new-window 'on-search-page :path "/search")

  (open-browser))


(defun render-doc-result (doc results app)
  (let* ((dtype (jsown:val doc "dtype"))
         (render-info (assoc dtype *dtype-icon-alist* :test #'equal))
         (keys-info (cdr (assoc dtype *keys-alist* :test #'equal)))
         (result-keys (getf keys-info :result))
         (result-chip-keys (getf keys-info :result-chips)))
    (when (and render-info keys-info)
      (let* ((card (create-div results :class "card" :style "white-space: normal; width: 100%; height: 200px; overflow: hidden; display: flex; flex-direction: column;"))
             (card-header (create-div card :class "card-header"))
             (card-img (create-phrase (create-div card-header :class "card-image") :i :class (cdr render-info)))
             (card-body (create-div card :class "card-body" :style "height: 100%; overflow: hidden; display: flex; flex-direction: column;"))
             (content (format nil "~{~a~^ · ~}" (loop for key in result-keys collect (jsown:val doc key))))
             (content-container (create-div card-body :style "flex: 1; overflow: hidden;")))
        (when (> (length content) 0)
          (create-div content-container :class "text-break" :style "max-height: 100%; overflow: hidden; text-overflow: ellipsis; display: -webkit-box; -webkit-line-clamp: 3; -webkit-box-orient: vertical;" :content content)
          (when result-chip-keys
            (let ((chip-container (create-div card-body :class "chip-container" :style "margin-top: auto;")))
              (loop for key in result-chip-keys
                    for value = (jsown:val doc key)
                    when value
                      do (let ((chip (create-span chip-container :class "chip" :html-id key)))
                           (setf (text chip) value)
                           (set-on-click chip
                                         (lambda (obj)
                                           (let* ((search-input (create-jquery body "input.form-input"))
                                                  (current-query (current-query app))
                                                  (new-query (format nil "~a ~a:~a" current-query key value)))
                                             (setf (current-query app) new-query))))))))
          card)))))
