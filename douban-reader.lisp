(in-package :douban-reader)

(defparameter *following* (make-hash-table))
(defparameter *user-contacts-uri* "http://api.douban.com/people/")
(defparameter *max-result* 50)

(defun fetch-xml (uri id
                  &optional (start-index 1) (max-result *max-result*))
  (let  ((request  (concatenate 'string uri id "/contacts?start-index="
                                (write-to-string start-index)
                                "&max-results=" (write-to-string max-result))))
    (cxml:parse
     (drakma:http-request request)
     (cxml-dom:make-dom-builder))))

(defun find-child-element (element name)
  (loop for e across (dom:child-nodes element)
       if (equal (dom:node-name e) name)
       return e))

(defun child-node-value (node name)
  (let ((e (find-child-element node name)))
    (dom:node-value
     (find-child-element e "#text"))))

(defun get-following (id)
  (let* ((doc (fetch-xml *user-contacts-uri* id))
        (entries (dom:get-elements-by-tag-name doc "entry")))
    (loop for e across entries do
         (setf (gethash (child-node-value e "title") *following*)
               (child-node-value e "id")))
    *following*))

(defun print-following ()
  (maphash #'(lambda (user uri)
               (format t "~a ~a~%" user uri))
           *following*))
