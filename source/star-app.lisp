(in-package :star.app)

(defun main ()
  (initialize 'on-search-page
              :port 2233
              :static-root (merge-pathnames "static/"
                                           (asdf:system-source-directory :star-app)))
  (set-on-new-window 'on-search-page :path "/search")
  (initialize 'on-document-editor :port 2233)
  (set-on-new-window 'on-document-editor :path "/editor")
  (initialize 'on-chat-view :port 2233)
  (set-on-new-window 'on-chat-view :path "/chat")
  (open-browser)
  ;; Keep the server running
  (loop (sleep 1)))
