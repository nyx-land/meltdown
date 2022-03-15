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
    :accessor text))
  (:documentation "Some unstructured text."))

(defclass section (body)
  ((parent
    :initarg :parent
    :initform nil
    :accessor parent))
  (:documentation "The lowest point in the hierachy of a document."))

(defclass doc (body)
  ((nodes
    :initarg :nodes
    :initform nil
    :accessor nodes))
  (:documentation "The whole markdown document."))

(defclass node (section)
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

(defclass paragraph (section) ()
  (:documentation "A placeholder class for a type of section."))

(defclass markup (body)
  ((meta
    :initarg :meta
    :accessor meta
    :documentation "Describes how the text is being presented."))
  (:documentation "A special bit of text that has more information associated
with it than just the text itself, such as a presentation type or a link."))

(defclass link (markup)
  ((src
    :initarg :src
    :accessor src))
  (:documentation "Points to some other text."))

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

(defmethod parse ((input section) &key doc)
  (with-slots (paragraphs parent) input
    (if (doc-raw-pos doc)
        (progn
          (setf parent (doc-raw-pos doc))
          (push (sections parent) input)))
    (setf paragraphs (split-paragraphs
              (collect-paragraphs (doc-raw-raw doc))))
    (parse doc)))

(defmethod parse ((input heading) &key doc)
  (with-slots (title parent depth) input
    (if (doc-raw-pos doc)
        (heading-handler input depth (doc-raw-pos doc))
        (setf (doc-raw-pos doc) input))
    (setf title (cleanup-title
                 (read-line (doc-raw-raw doc))))
    (push input (doc-raw-nodes doc))
    (parse doc)))

(defmethod parse ((input doc-raw) &key doc)
  (declare (ignore doc))
  (parse (make-obj (eof-handler input)
                   :in-stream (doc-raw-raw input)
                   :parent (doc-raw-pos input))
         :doc input))

(defmethod parse ((input stream) &key doc)
  (declare (ignore doc))
  (parse (make-doc-raw :raw input)))

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
        (doc-raw-nodes doc))
  input)
