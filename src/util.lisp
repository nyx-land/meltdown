;; This has all the business logic for figuring out what to do with
;; the contents of a markdown file/string after it's been passed
;; through the toplevel parser method.

(in-package :meltdown)

(defun cleanup-title (s)
  "Remove extraneous characters from the title of a heading."
  (remove #\Space
          (remove #\# s)))

(defun collect-paragraphs (in-stream)
  "Collect the raw string for the paragraph(s) that belong to a heading."
  (loop for x = (read-char in-stream nil)
        until (or (eql x #\#)
                  (null x))
        collect (princ x) into text
        finally (return (concatenate 'string text))))

(defun split-paragraphs (text)
  "Split a string into paragraphs."
  (remove-if (lambda (x) (equalp "" x))
             (uiop:split-string text
                                :separator (format nil "~%"))))

(defun ret-node (depth parent)
  "If the heading has a parent, return that along with its depth."
  (let ((ret `(:depth ,depth)))
    (if parent (append ret `(:parent ,parent))
        ret)))

(defun make-heading-param (in-stream &key (parent nil))
  "Determine the heading's depth and whether or not it has a parent."
  (loop for c = (read-char in-stream nil)
        for d = 1 then (incf d)
        when (not (eql c #\#))
          return (ret-node d parent)))

(defun make-heading (in-stream &key (parent nil))
  "A special object constructor to handle making a heading. Appends
the unevaluated parameters to the constructor before making the object."
  (eval (append
         '(make-instance (quote heading))
         (make-heading-param in-stream :parent parent))))

(defun heading-handler (in-node depth pos)
  "A handler when a heading is received to figure out where the
heading belongs within the document's hierarchy."
  (cond ((< depth (depth pos))
         (setf (parent in-node) pos))
        ((= depth (depth pos))
         (if (parent pos)
             (setf (parent in-node) (parent pos))))))

(defun make-obj (c &key in-stream (parent nil))
  "Make an object that corresponds to a node in the markdown
document based off the character that is received by the parser
method. Return the doc if it reaches the end of the stream and
by default assume a character belongs to a paragraph."
  (flet ((lc (cx) (equalp c cx))
         (ret (obj) `(make-instance (quote ,obj))))
    (cond ((lc #\#) (make-heading in-stream
                                  :parent parent))
          (:eof (eval (ret 'doc)))
          (t (eval (ret 'section))))))

(defun eof-handler (doc)
  (let ((final-doc (make-instance
                    'doc
                    :nodes (doc-raw-nodes doc))))
    (read-char (doc-raw-raw doc) nil final-doc)))
