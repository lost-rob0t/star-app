(in-package :star.app)

(defparameter *app-name* "Star Intel")

(defun load-community-footer (body)
  "Load the shared StarIntel community footer once per browser document."
  (js-execute
   body
   "(() => {
      if (document.querySelector('script[data-starintel-community-footer]')) return;
      const script = document.createElement('script');
      script.src = '/js/starintel-community-footer.js';
      script.defer = true;
      script.dataset.starintelCommunityFooter = 'true';
      document.head.appendChild(script);
    })();"))

(defun on-load-init (body page-title)
  "Load CSS sources and shared site UI."
  (load-css (html-document body) "https://unpkg.com/spectre.css/dist/spectre.min.css")
  (load-css (html-document body) "https://unpkg.com/spectre.css/dist/spectre-exp.min.css")
  (load-css (html-document body) "https://unpkg.com/spectre.css/dist/spectre-icons.min.css")
  (load-css (html-document body) "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css")
  ;; TODO load custom css
  (load-css (html-document body) "/css/synthwave.css")
  (load-community-footer body)
  (setf (title (html-document body)) (format nil "~a - ~a" *app-name* page-title)))

(defun on-page (body page-title &key (search t))
  (on-load-init body page-title)
  (on-nav body :search search))

;; TODO
;; (defmacro defpage ((html-body &key (title "Star Intel")) &body body)
;;   `(let ((navbar (on-nav-bar ,html-body))
;;          )))
;;          A simple macro to include the navbar and other stuff needed.
