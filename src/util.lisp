(in-package :meltdown)

(defun cleanup-title (s)
  (remove #\Space
          (remove #\# s)))

(defun collect-paragraphs (in-stream)
  (loop for x = (read-char in-stream nil)
        until (eql x #\#)
        collect (princ x) into text
        finally (return (concatenate 'string text))))

(defun split-paragraphs (text)
  (remove-if (lambda (x) (equalp "" x))
             (uiop:split-string text
                                :separator (format nil "~%"))))

(defun ret-node (depth parent)
  (let ((ret `(:depth ,depth)))
    (if parent (append ret `(:parent ,parent))
        ret)))

(defun make-heading-param (in-stream &key (parent nil))
  (loop for c = (read-char in-stream nil)
        for d = 1 then (incf d)
        when (not (eql c #\#))
          return (ret-node d parent)))

(defun make-heading (in-stream &key (parent nil))
  (eval (append
         '(make-instance (quote heading))
         (make-heading-param in-stream :parent parent))))

(defun heading-handler (in-node depth pos)
  (cond ((< depth (depth pos))
         (setf (parent in-node) pos))
        ((= depth (depth pos))
         (if (parent pos)
             (setf (parent in-node) (parent pos))))))

(defun make-obj (c &key in-stream (parent nil))
  (flet ((lc (cx) (equalp c cx))
         (ret (obj) `(make-instance (quote ,obj))))
    (cond ((lc #\#) (make-heading in-stream
                                  :parent parent))
          (t (ret 'section)))))
