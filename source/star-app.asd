(asdf:defsystem :star-app
  :version      "0.1.0"
  :description  "starintel ui"
  :author       " <unseen@flake>"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "documents")
                 (:file "package")
                 (:file "client")
                 (:file "contract")
                 (:file "utils")
                 (:file "templates/base")
                 (:file "templates/nav")
                 (:file "render")
                 (:file "components/input-autocomplete")
                 (:file "pages/editor")
                 (:file "pages/search")
                 (:file "pages/chat")
                 (:file "pages/graph")
                 (:file "pages/target-admin")
                 (:file "star-app"))
  :depends-on   (#:dexador
                 #:clog
                 #:starintel-0101
                 #:com.inuoe.jzon
                 #:serapeum
                 #:local-time
                 #:jsown
                 #:log4cl
                 #:str
                 #:starintel-gserver-client))
