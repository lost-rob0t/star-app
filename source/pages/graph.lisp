(in-package :star.app)

(defclass graph-editor-app (star-app)
  ((documents :initform (make-hash-table :test 'equal) :accessor editor-documents)
   (relations :initform nil :accessor editor-relations)
   (graph :initform nil :accessor editor-graph)))

(defvar *graph-editor* (make-instance 'graph-editor-app
                                      :client (api-client *app*)
                                      :app-name "Graph Editor"))

(defun on-graph-editor-page (body)
  (on-page body "Graph Editor")
  (let* ((container (create-div body :class "container"))
         (canvas (create-canvas container :width 800 :height 600))
         (new-doc-btn (create-button container :content "New Document" :class "btn btn-primary")))

    (js-execute body "
      function loadScript(url) {
        return new Promise((resolve, reject) => {
          const script = document.createElement('script');
          script.src = url;
          script.onload = resolve;
          script.onerror = reject;
          document.head.appendChild(script);
        });
      }

      Promise.all([
        loadScript('https://cdnjs.cloudflare.com/ajax/libs/vis-network/9.1.2/vis-network.min.js'),
        loadScript('https://cdnjs.cloudflare.com/ajax/libs/vis-data/7.1.4/vis-data.min.js')
      ]).then(() => {
        const nodes = new vis.DataSet([]);
        const edges = new vis.DataSet([]);
        const container = document.getElementById('graph-container');
        const data = { nodes, edges };
        const options = {};
        const network = new vis.Network(container, data, options);
        window.graphEditor = { network, nodes, edges };
      });
    ")

    (setf (editor-graph *graph-editor*) canvas)

    (set-on-click new-doc-btn
                  (lambda (obj)
                    (declare (ignore obj))
                    (open-document-modal body)))

    (run body)))

(defun open-document-modal (body)
  (let* ((modal (create-div body :class "modal active"))
         (modal-container (create-div modal :class "modal-container"))
         (modal-body (create-div modal-container :class "modal-body"))
         (form (create-form modal-body))
         (type-select (create-select form))
         (form-container (create-div form))
         (submit-btn (create-button form :content "Create" :class "btn btn-primary")))

    (place-inside-top-of modal-body (create-button form :content "Create" :class "btn btn-primary"))
    (loop for doc-type in *document-types*
          do (create-option type-select :content (car doc-type) :value (car doc-type)))

    (flet ((update-form ()
             (let* ((selected-type (value type-select))
                    (document (make-instance (find-symbol (string-upcase selected-type) :spec))))
               (setf (inner-html form-container) "")
               (create-document-form form-container selected-type document))))

      (set-on-change type-select (lambda (obj) (declare (ignore obj)) (update-form)))
      (update-form)

      (set-on-click submit-btn
                    (lambda (obj)
                      (declare (ignore obj))
                      (let* ((selected-type (value type-select))
                             (document (make-instance (find-symbol (string-upcase selected-type) :spec))))
                        (loop for input in (collect-children form-container)
                              for name = (attribute input "name")
                              for value = (value input)
                              for slot = (find-symbol (string-upcase name) :spec)
                              when slot
                                do (setf (slot-value document slot) value))
                        (add-document-to-graph document)
                        (setf (visiblep modal) nil)))))

    (setf (visiblep modal) t)))

(defun add-document-to-graph (document)
  (let* ((id (spec:doc-id document))
         (label (format nil "~A: ~A" (type-of document) id))
         (node (jsown:new-js
                 ("id" id)
                 ("label" label))))
    (setf (gethash id (editor-documents *graph-editor*)) document)
    (js-execute (editor-graph *graph-editor*)
                (format nil "window.graphEditor.nodes.add(~A)" (jsown:to-json node)))))

(defun add-relation-to-graph (relation)
  (let ((edge (jsown:new-js
                ("from" (spec:relation-source relation))
                ("to" (spec:relation-target relation))
                ("label" (spec:relation-note relation)))))
    (push relation (editor-relations *graph-editor*))
    (js-execute (editor-graph *graph-editor*)
                (format nil "window.graphEditor.edges.add(~A)" (jsown:to-json edge)))))

(defun open-document-viewer (body document)
  (let* ((modal (create-div body :class "modal active"))
         (modal-container (create-div modal :class "modal-container"))
         (modal-body (create-div modal-container :class "modal-body"))
         (close-btn (create-button modal-container :content "Close" :class "btn")))

    (create-document-form modal-body (type-of document) document nil)

    (set-on-click close-btn
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (visiblep modal) nil)))

    (setf (visiblep modal) t)))

(defun load-relations (relations)
  (loop for relation in relations
        do (add-relation-to-graph relation)))
