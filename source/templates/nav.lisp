(in-package :star.app)

(defun on-nav (body &key (search t))
  (let* ((navbar (create-element body :header :class "navbar"))
         (nav-sections (create-section navbar :section))
         (index (create-a nav-sections :class "navbar-brand mr-2" :content "StarIntel Web UI" :link "/chat"))
         (messages (create-a nav-sections :class "btn btn-link" :content "Messages"))
         (users (create-a nav-sections :class "btn btn-link" :content "users"))
         (orgs (create-a nav-sections :class "btn btn-link" :content "Organizations"))
         (people (create-a nav-sections :class "btn btn-link" :content "People"))
         (datasets (create-a nav-sections :class "btn btn-link" :content "Datasets")))

    (when search
      ;; eww
      (let* ((nav-search-section (create-section navbar :section))
             (nav-search-form (create-div nav-search-section :class "input-group" :style "min-width: 300px;"))
             (nav-search-input (create-element nav-search-form "text" :class "form-input" :placeholder "content:hello"))
             (submit-button (create-button nav-search-form :class "btn btn-primary input-group-btn" :style "min-width: 40px;"))
             (search-icon (create-element submit-button "i" :class "bi bi-search")))))))
