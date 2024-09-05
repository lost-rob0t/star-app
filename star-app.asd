(asdf:defsystem :star-app
  :version      "0.1.0"
  :description  "starintel ui"
  :author       " <unseen@flake>"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "package")
                 (:file "star-app"))
  :depends-on   (#:dexador #:clog #:starintel #:jsown))
