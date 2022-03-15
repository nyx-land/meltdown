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

(defun make-heading-param (doc)
  "Determine the heading's depth."
  (loop for c = (read-char (raw doc) nil)
        for d = 0 then (incf d)
        when (not (eql c #\#))
          return `(:depth ,d)))

(defun make-heading (doc)
  "A special object constructor to handle making a heading. Appends
the unevaluated parameters to the constructor before making the object."
  (eval (append
         '(make-instance (quote heading))
         (make-heading-param doc))))

(defun heading-handler (in-node doc)
  "A handler when a heading is received to figure out where the
heading belongs within the document's hierarchy."
  (cond ((null (pos doc))
         (progn
           (setf (pos doc) in-node)
           (setf (parent in-node) (final doc))))
        ((< (depth in-node) (depth (pos doc)))
         (setf (parent in-node) (pos doc)))
        ((= (depth in-node) (depth (pos doc)))
         (setf (parent in-node) (parent (pos doc)))))
  in-node)

(defun make-obj (c doc) ;; &key (parent nil))
  "Make an object that corresponds to a node in the markdown
document based off the character that is received by the parser
method. Return the doc if it reaches the end of the stream and
by default assume a character belongs to a paragraph."
  (flet ((lc (cx) (equalp c cx))
         (ret (obj) `(make-instance (quote ,obj))))
    (cond ((lc #\#) (heading-handler
                     (make-heading doc)
                     doc))
          (:eof (final doc))
          (t (eval (ret 'section))))))

