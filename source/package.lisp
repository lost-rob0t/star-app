(defpackage   :star.app
  (:use       #:clog #:cl #:starintel-gserver-client)
  (:local-nicknames (#:spec #:star.app.documents))
  (:documentation "doc")
  (:export
   #:*dtype-icon-alist*
   #:*keys-alist*
   #:*icon-alist*
   #:main))
