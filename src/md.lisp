(in-package :meltdown)

(defparameter *parse-test*
  (make-string-input-stream
"# heading

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

## subheading

Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?

At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat.")
  "A test markdown document. TODO: make proper tests for this")

(defclass body ()
  ((text
    :initarg :text
    :initform nil
    :accessor text))
  (:documentation "Some unstructured text."))

(defclass blk (body)
  ((parent
    :initarg :parent
    :initform nil
    :accessor parent))
  (:documentation "A text block. The lowest point in the hierachy of
a document."))

(defclass doc (body)
  ((nodes
    :initarg :nodes
    :initform nil
    :accessor nodes))
  (:documentation "The whole markdown document."))

(defclass node (blk)
  ((children
    :initarg :children
    :initform nil
    :accessor children))
  (:documentation "A node is something that marks a change in the
hierachy of a document."))

(defclass heading (node)
  ((title
    :initarg :title
    :accessor title)
   (depth
    :initarg :depth
    :initform 1
    :accessor depth))
  (:documentation "A heading is a type of node that has a title
associated with it and a depth."))

(defclass blkquote (blk) ())

(defclass blist (blk)
  ((members
    :initarg :members
    :initform nil
    :accessor members))
  (:documentation "A bulleted list within the body of a document."))

(defclass horizontal-rule (blk) ())

(defclass code-blk (blk)
  ((language
    :initarg :language
    :initform :fundamental
    :accessor language)))

(defclass paragraph (blk) ()
  (:documentation "A placeholder class for a type of blk."))

(defclass span (body)
  ((meta
    :initarg :meta
    :accessor meta
    :documentation "Describes how the text is being presented."))
  (:documentation "Special text within the body of a blk element."))

(defclass link (span)
  ((src
    :initarg :src
    :accessor src)
   (title
    :initarg :title
    :initform nil
    :accessor title)
   (meta
    :initform 'link)))

(defclass image (link)
  ((meta :initform 'image)))

(defclass bold (span)
  ((meta :initform 'bold)))

(defclass italic (span)
  ((meta :initform 'italic)))

(defclass strikethrough (span)
  ((meta :initform 'strikethrough)))

(defclass code (span)
  ((meta :initform 'code)))

(defclass doc-raw (doc)
  ((pos
    :initarg :pos
    :initform nil
    :accessor pos)
   (raw
    :initarg :raw
    :accessor raw)
   (final
    :initarg :final
    :initform (make-instance 'doc)
    :accessor final))
  (:documentation "A document begins its lifecycle as a raw document with some
additional information to track the position within it for parsing."))

(defgeneric parse (input &key doc)
  (:documentation "The main entrypoint for parsing markdown. Can take
a file or string."))

(defgeneric span-parse (input doc)
  (:documentation "A special parser method for stuff within the body of a paragraph."))

(defgeneric blk-parse (input doc)
  (:documentation "A special parser for blocks within the document nodes."))

;; TODO: make this into a more generic function
(defmethod span-parse ((input bold) doc)
  (read-char (raw doc))
  (with-slots (text) input
    (loop for c = (read-char (raw doc) nil :eof)
          for n = (peek-char nil (raw doc) nil)
          until (or (eq n #\*)
                    (eq n #\_))
          collect c into chars
          finally (setf text (concatenate 'string chars))))
  (dotimes (n 2) (read-char (raw doc) nil :eof))
  input)

(defmethod span-parse ((input italic) doc)
  (with-slots (text) input
    (loop for c = (read-char (raw doc) nil :eof)
          for n = (peek-char nil (raw doc) nil)
          until (or (eq n #\*)
                    (eq n #\_))
          collect c into chars
          finally (setf text (concatenate 'string chars)))
    (read-char (raw doc) nil :eof)
    input))

(defun link-text (obj s)
  (with-slots (text) obj
    (loop for c = (read-char s nil :eof)
          until (eq c #\])
          collect c into chars
          finally (setf text (concatenate 'string chars)))))

(defun link-title (obj s)
  (with-slots (title) obj
    (loop for c = (read-char s nil :eof)
          until (eq c #\")
          collect c into chars
          finally (setf title (concatenate 'string chars)))))

(defun link-src (obj s)
  (with-slots (src) obj
    (loop for c = (read-char s nil :eof)
          until (eq c #\))
          if (eq c #\")
            do (link-title obj s)
          else
        collect c into chars
          finally (setf src (concatenate 'string chars)))))

(defun link-parse (input doc)
  (link-text input (raw doc))
  (read-char (raw doc) nil :eof)
  (link-src input (raw doc))
  input)

(defmethod span-parse ((input link) doc)
  (link-parse input doc))

(defmethod span-parse ((input image) doc)
  (read-char (raw doc) nil :eof)
  (link-parse input doc))

(defmethod span-parse ((input character) doc)
  (let ((n (peek-char nil (raw doc) nil)))
    (if (eq input #\\)
        (read-char (raw doc) nil :eof))
    (unless (eq input #\\)
      (cond ((or (and (eq input #\*)
                      (eq n #\*))
                 (and (eq input #\_)
                      (eq input #\_)))
             (span-parse
              (make-instance 'bold) doc))
            ((or (eq input #\*)
                 (eq input #\_))
             (span-parse
              (make-instance 'italic) doc))
            ((and (eq input #\!)
                  (eq n #\[))
             (span-parse
              (make-instance 'image) doc))
            ((eq input #\[)
             (span-parse
              (make-instance 'link) doc))
            (t "nothing to parse")))))

(defmethod blk-parse ((input character) doc)
  (let ((c (read-char (raw doc) nil :eof))
        (n (peek-char nil (raw doc) nil :eof)))
    (cond ((eq c #\>)
           (blk-parse
            (make-instance 'blkquote) doc))
          ((eq c #\`)
           (blk-parse
            (make-instance 'code-blk) doc))
          ((or (and (digit-char-p c)
                    (eq n #\.))
               (and (or (eq c #\*)
                        (eq c #\-)
                        (eq c #\+))
                    (eq n #\space)))
           (blk-parse
            (make-instance 'blist) doc))
          (t (blk-parse
              (make-instance 'paragraph :parent (pos doc)) doc)))))

(defmethod parse ((input paragraph) &key doc)
  (with-slots (text parent) input
    (push input (children parent))
    (loop for c = (read-char (raw doc) nil :eof)
          for n = (peek-char nil (raw doc) nil)
          if (and (eq c #\newline) (eq n #\newline))
            do (parse doc)
          else
            do (span-parse input doc))))

(defmethod parse ((input heading) &key doc)
  (with-slots (title parent depth) input
    (setf title (cleanup-title
                 (read-line (raw doc) nil)))
    (check-toplevel input doc)
    (setf (pos doc) input)
    (parse doc)))

(defmethod parse ((input doc-raw) &key doc)
  (declare (ignore doc))
  (parse (make-obj (peek-char nil (raw input) nil)
                   input)
         :doc input))

(defmethod parse ((input stream) &key doc)
  (declare (ignore doc))
  (parse (make-instance 'doc-raw :raw input)))

(defmethod parse ((input string) &key doc)
  (declare (ignore doc))
  (let ((in-stream (make-string-input-stream input)))
    (parse in-stream)))

(defmethod parse ((input pathname) &key doc)
  (declare (ignore doc))
  (with-open-file (f input)
    (parse f)))

(defmethod parse ((input doc) &key doc)
  (setf (nodes input)
        (nodes doc))
  input)
