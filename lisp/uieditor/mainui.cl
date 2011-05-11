;;; Code for the form named :mainui of class frame-window.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
(defclass my-project-management-area-outline (outline) ())

(defmethod redisplay-window ((project-management-area my-project-management-area-outline) &optional my-box)
  (let ((projectname (get-projectname))
        )
    (format t "redisplay~%")
    ;;(setf (range project-management-area) projectname) 
    )
  )
;;(defun get-projectname ()
;;  (uie:list-project-name)
;;  )
(defun sui-create-project (window)
  (format t "click ~a~%" window)
  (project-ui)
  ;;(make-project-ui)
  (format t "after project ui~%")
  )
(defun sui-delete-project (window)
  (format t "delete project ~a~%" window)
  )
