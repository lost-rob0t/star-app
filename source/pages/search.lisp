(in-package :star.app)

(defclass search-app (star-app)
  ((current-query :accessor current-query :initform nil)
   (fts-bookmark :accessor fts-bookmark :initform nil)
   (host :accessor app-host :initform "127.0.0.1")
   (port :accessor app-port :initform 5000)))


(defvar *search-app* (make-instance 'search-app))





(defun render-doc (doc root-obj)
  (let* ((dtype (gethash "dtype" doc))
         (render-info (assoc dtype *dtype-icon-alist* :test #'equal))
         (keys-info (cdr (assoc dtype *keys-alist* :test #'equal)))
         (doc-render-keys (getf keys-info :doc-render)))
    (when (and render-info keys-info)
      (let* ((modal (create-div root-obj :class "modal active"))
             (container (create-div modal :class "modal-container"))
             (header (create-div container :class "modal-title h5" :content (format nil "Veiwing ~a" (gethash "id" doc))))
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
      (let ((value (gethash key doc)))
        (when (nth-value 1 (gethash key doc))
          (log-console (clog:window root-obj) (cdr (assoc key icon-alist :test #'equal)))
          (create-phrase column1 :i :style (cdr (assoc key icon-alist :test #'equal)))
          (let* ((tile (create-div column2 :class "tile tile-centered"))
                 (tile-content (create-div tile :class "tile-content")))
            (create-span tile-content :class "tile-subtitle" :content (display-value value))
            (create-div tile-content :class "tile-title" :content (str:title-case key))
            tile))))))


;; (defmethod query ((app app) query))


(defun on-search-page (body)
  (on-page body "Search")
  (let* ((search-app *search-app*)
         (container (create-div body :class "container" :style "text-align: center;"))
         (search-form (create-form container :method :get :action "/search" :class "input-group" :style "max-width: 500px; margin: 0 auto;"))
         (search-input (create-form-element search-form :text :name "q" :placeholder (or (current-query search-app) "message:hello") :class "form-input" :html-id "search-bar"))
         (cols (create-div container :class "columns" :style "margin-top: 20px;"))
         (results-col (create-div cols :class "column col-9" :style "margin: 0 auto;"))
         (results (create-div results-col :class "container results-container"))
         (load-more-button (create-button container :class "btn btn-primary" :content "Load More Results" :style "margin-top: 20px;")))

    (when (current-query search-app)
      (setf (text-value search-input) (current-query search-app)))

    (set-on-submit search-form
                   (lambda (obj)
                     (declare (ignore obj))
                     (let* ((query (clog:text-value search-input))
                            (response (fts (api-client search-app) :q query :limit 50))
                            (parsed-response (com.inuoe.jzon:parse response))
                            (rows (gethash "rows" parsed-response))
                            (bookmark (gethash "bookmark" parsed-response)))

                       ;; Clear previous results
                       (setf (inner-html results-col) "")
                       (setf results (create-div results-col :class "container results-container"))

                       (setf (current-query search-app) query)
                       (setf (fts-bookmark search-app) bookmark)
                       (clog:log-console (clog:window body)
                                         (format nil "url: ~a~%Results count:~a~%Bookmark: ~a~%Current query: ~a~%Search bar: ~a" (base-url (api-client search-app)) (length rows) (fts-bookmark search-app) (current-query search-app) query))
                       (loop for row across rows
                             for doc = (gethash "doc" row)
                             do (render-doc-result body doc results search-app))
                       (force-output t))))

    (set-on-click load-more-button
                  (lambda (obj)
                    (declare (ignore obj))
                    (let* ((query (clog:value search-input))
                           (response (fts (api-client search-app) :q query :limit 50
                                          :bookmark (fts-bookmark search-app)))
                           (parsed-response (com.inuoe.jzon:parse response))
                           (rows (gethash "rows" parsed-response))
                           (bookmark (or (gethash "bookmark" parsed-response) (fts-bookmark search-app))))

                      (setf (fts-bookmark search-app) bookmark)
                      (when rows
                        (loop for row across rows
                              for doc = (gethash "doc" row)
                              do (place-inside-bottom-of results  (render-doc-result body doc results search-app))))
                      (force-output t))))
    (run body)))


(defun render-doc-result (body doc results search-app)
  (setf doc (as-json (from-json doc)))
  (let* ((dtype (gethash "dtype" doc))
         (render-info (assoc dtype *dtype-icon-alist* :test #'equal))
         (keys-info (cdr (assoc dtype *keys-alist* :test #'equal)))
         (result-keys (getf keys-info :result))
         (result-chip-keys (getf keys-info :result-chips)))
    (when (and render-info keys-info)
      (let* ((card (create-div results :class "card" :style "white-space: normal; width: 100%; margin-bottom: 10px;"))
             (card-body (create-div card :class "card-body" :style "display: flex; flex-direction: row;"))
             (chip-container (create-div card-body :style "width: 150px; padding-right: 25px; border-right: 1px solid #e7e9ed;"))
             (content-container (create-div card-body :style "flex: 1; padding-left: 15px;"))
             (content (format nil "~{~a~^ · ~}" (loop for key in result-keys collect (display-value (gethash key doc))))))

        ;; Chips on the left
        (when result-chip-keys
          (loop for key in result-chip-keys
                for value = (gethash key doc)
                when value
                  do (let ((chip (create-div chip-container :class "chip" :style "margin-bottom: 5px; cursor: pointer; font-size: 0.8em;")))
                       (create-phrase chip :i :class (cdr (assoc key *icon-alist* :test #'equal)) :style "margin-right: 5px;")
                       (create-span chip :content (display-value value))
                       (set-on-click chip
                                     (lambda (obj)
                                       (unless value
                                         (format t "wtf"))
                                       (let* ((current-query (current-query search-app))
                                              (new-query (format nil "~a AND ~a:~a" current-query key value)))
                                         (setf (current-query search-app) new-query)))))))

        ;; Main content on the right
        (let ((type-container (create-div content-container :style "display: flex; align-items: center; margin-bottom: 5px;")))
          (create-phrase type-container :i :class (cdr render-info) :style "margin-right: 10px;")
          (create-span type-container :content (string-capitalize dtype) :style "font-weight: bold; font-size: 0.9em;"))

        (create-div content-container :class "text-break"
                                      :style "overflow: hidden; text-overflow: ellipsis; display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; font-size: 0.9em;"
                                      :content content)

        card))))





;; (defun render-doc-result (body doc results search-app)
;;   (let* ((dtype (gethash "dtype" doc))
;;          (render-info (assoc dtype *dtype-icon-alist* :test #'equal))
;;          (keys-info (cdr (assoc dtype *keys-alist* :test #'equal)))
;;          (result-keys (getf keys-info :result))
;;          (result-chip-keys (getf keys-info :result-chips)))
;;     (when (and render-info keys-info)
;;       (let* ((card (create-div results :class "card" :style "white-space: normal; width: 100%; height: 200px; overflow: hidden; display: flex; flex-direction: column;"))
;;              (card-header (create-div card :class "card-header"))
;;              (card-img (create-phrase (create-div card-header :class "card-image") :i :class (cdr render-info)))
;;              (card-body (create-div card :class "card-body" :style "height: 100%; overflow: hidden; display: flex; flex-direction: column;"))
;;              (content (format nil "~{~a~^ · ~}" (loop for key in result-keys collect (display-value (gethash key doc)))))
;;              (content-container (create-div card-body :style "flex: 1; overflow: hidden;")))
;;         (when (> (length content) 0)
;;           (create-div content-container :class "text-break" :style "max-height: 100%; overflow: hidden; text-overflow: ellipsis; display: -webkit-box; -webkit-line-clamp: 3; -webkit-box-orient: vertical;" :content content)
;;           (when result-chip-keys
;;             (let ((chip-container (create-div card-body :class "chip-container" :style "margin-top: auto;")))
;;               (loop for key in result-chip-keys
;;                     for value = (gethash key doc)
;;                     when value
;;                       do (let ((chip (create-span chip-container :class "chip" :html-id key)))
;;                            (setf (text chip) value)
;;                            (set-on-click chip
;;                                          (lambda (obj)
;;                                            (let* ((current-query (current-query search-app))
;;                                                   (new-query (format nil "~a ~a:~a" current-query key value)))
;;                                              (setf (current-query search-app) new-query))))))))
;;           card)))))
