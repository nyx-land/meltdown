(defparameter *parse-test*
  (make-string-input-stream
"# heading

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

## subheading

Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?

At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat."))

;;(defclass token ()
;;  ((c
;;    :initarg :char
;;    :accessor char)))

(defclass node ()
  ((parent
    :initarg :parent
    :accessor parent)
   (children
    :initarg :children
    :initform nil
    :accessor children)))

(defclass heading (node)
  ((title
    :initarg :title
    :accessor title)
   (depth
    :initarg :depth
    :initform 1
    :accessor depth)
   (sections
    :initarg :sections
    :initform nil
    :accessor sections)))

(defclass section (node)
  ((paragraphs
    :initarg :paragraphs
    :initform nil
    :accessor paragraphs)))

(defparameter *char-lookup*
  (vy:dict
   #\# '(make-instance (quote heading))
   t '(make-instance (quote section))))

(defun getchar (c)
  (gethash c *char-lookup*
           (gethash t *char-lookup*)))

(defun make-obj (c &key (eval? t))
  (let ((obj (getchar c)))
    (if eval?
        (eval obj)
        obj)))

(defun cleanup-title (s)
  (remove #\Space
          (remove #\# s)))

(defun add-subheading (parent)
  (push (eval (append
               (make-obj #\# :eval? nil)
               `(:depth ,(+ 1 (depth parent))
                 :parent ,parent)))
        (children parent)))

(defun collect-paragraphs (in-stream)
  (loop for x = (read-char in-stream nil)
        until (eql x #\#)
        collect x into text
        finally (concatenate 'string text)))

(defun split-paragraphs (text)
  (remove-if (lambda (x) (equalp "" x))
             (uiop:split-string text
                                :separator (format nil "~%"))))

(defgeneric parse (input &key &allow-other-keys)
  (:documentation "Parser for markdown"))

(defmethod parse ((input section) &key in-stream
                                    prev-node)
  (with-accessors (paragraphs parent) input
    (setf parent prev-node)
    (setf paragraphs (split-paragraphs
                      (collect-paragraphs in-stream)))
    (parse in-stream)))

(defmethod parse ((input heading) &key in-stream)
  (let ((next-char (peek-char nil input)))
    (with-accessors (depth title) input
      (if (eql next-char #\#)
          (parse (car (add-subheading input))
                 :in-stream in-stream)
          (progn
            (setf title (cleanup-title
                         (read-line in-stream)))
            (parse (make-obj next-char)
                   :in-stream in-stream
                   :prev-node input))))))

(defmethod parse ((input stream))
  (let ((next-char (peek-char nil input)))
    (parse (make-obj next-char) :in-stream input)))
