(in-package :meltdown)

(defun getchar (c)
  (flet ((lc (cx) (equalp c cx))
         (ret (obj) `(make-instance (quote ,obj))))
    (cond ((lc #\#) (ret 'heading))
          (t (ret 'section)))))

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
        collect (princ x) into text
        finally (return (concatenate 'string text))))

(defun split-paragraphs (text)
  (remove-if (lambda (x) (equalp "" x))
             (uiop:split-string text
                                :separator (format nil "~%"))))
