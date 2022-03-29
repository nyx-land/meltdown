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
  (loop for x = (read-char in-stream nil :eof)
        until (or (eq x #\#)
                  (eq x :eof))
        collect (princ x) into text
        finally (return (concatenate 'string text))))

(defun split-paragraphs (text)
  "Split a string into paragraphs."
  (remove-if (lambda (x) (equalp "" x))
             (uiop:split-string text
                                :separator (format nil "~%"))))

(defun make-heading (doc)
  "A special object constructor to handle making a heading. Appends
the unevaluated parameters to the constructor before making the object."
  (loop for c = (read-char (raw doc) nil :eof)
        for d = 0 then (incf d)
        when (not (eql c #\#))
          return (make-instance
                  'heading
                  :depth d)))

(defun set-relationship (p c)
  (with-slots (parent) c
    (with-slots (children) p
      (setf parent p)
      (setf children (push c children)))))

(defun heading-handler (in-node prev doc)
  "A handler when a heading is received to figure out where the
heading belongs within the document's hierarchy."
  (cond ((null prev)
         (set-parent (final doc) in-node))
        ((equalp (type-of (parent prev))
                 'doc)
         (set-relationship prev in-node))
        ((> (depth in-node) (depth prev))
         (set-relationship prev in-node))
        ((= (depth in-node) (depth prev))
         (set-relationship (parent prev) in-node))
        ((< (depth in-node) (depth (pos doc)))
         (heading-handler in-node (parent prev) doc)))
  in-node)

;; TODO: handle cases where all the highest nodes are
;; the same level but not at 1
(defun check-toplevel (in-node doc)
  "Check if a heading should be in the toplevel doc nodes."
  (if (or (= (depth in-node) 1)
          (null (pos doc)))
      (setf (nodes doc)
            (push in-node (nodes doc)))))

(defun make-obj (c doc) ;; &key (parent nil))
  "Make an object that corresponds to a node in the markdown
document based off the character that is received by the parser
method. Return the doc if it reaches the end of the stream and
by default assume a character belongs to a paragraph."
  (flet ((lc (cx) (equalp c cx))
         (ret (obj) `(make-instance (quote ,obj))))
    (cond ((eq c :eof)
           (final doc))
          ((lc #\#) (heading-handler
                     (make-heading doc)
                     (pos doc) doc))
          ((alpha-char-p c)
           (make-instance 'section))
          ((or (lc #\Newline)
               (lc #\Return))
           (make-obj (read-char nil nil :eof)
                     doc))
          )))
