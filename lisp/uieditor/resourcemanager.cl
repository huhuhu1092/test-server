;;; Code for the form named :resourcemanager of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)

(defclass my-resourcemanager-outline (outline) ())
(defmethod range-on-open ((outline my-resourcemanager-outline) item-value range)
  )
(defun resourcemanager-load-file-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  
  t)
