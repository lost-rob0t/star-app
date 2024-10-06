(in-package :star.app)

(defun on-nav (body &key (search t))
  (let* ((navbar (create-element body :header :class "navbar"))
         (nav-sections (create-section navbar :section))
         (index (create-a nav-sections :class "navbar-brand mr-2" :content "StarIntel Web UI" :link "/chat"))
         (messages (create-a nav-sections :class "btn btn-link" :content "Messages"))
         (users (create-a nav-sections :class "btn btn-link" :content "users"))
         (orgs (create-a nav-sections :class "btn btn-link" :content "Organizations"))
         (people (create-a nav-sections :class "btn btn-link" :content "People"))
         (datasets (create-a nav-sections :class "btn btn-link" :content "datasets")))

    (when search
      ;; eww
      (let* ((nav-search-section (create-section navbar :section))
             (nav-search-form (create-div nav-search-section :class "input-group input-inline"))
             (nav-search-input (create-element nav-search-form "text" :class "form-input input-inline" :placeholder "content:hello"))
             (submit-button (create-button nav-search-form :class "btn btn-primary input-group-btn")))))))
