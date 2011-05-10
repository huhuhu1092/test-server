;;; Code for the form named :project-ui of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
(defstruct uie-project name path)
(defun project-ui-button-selectpath-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (let* ((pathname (ask-user-for-directory :prompt "Current Directory" :stream dialog))
         (pathstr (namestring (if (null pathname) "" pathname)))
         (projectdir-component (find-component :project-directory dialog))
         )
    (format t "~a~%" pathstr)
    (setf (value projectdir-component) pathstr)
    )
  t)

(defun project-ui-button-ok-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (let ((projectdir-component (find-component :project-directory dialog))
        (projectfile-component (find-component :project-file-name dialog))
        (projectfile-prompt (find-component :static-text-6 dialog))
        )
    (format t "~a~a~%" (value projectdir-component) (value projectfile-component))

    (if (and (value projectdir-component) (value projectfile-component)) 
        (make-uie-project :name (value projectfile-component) :path (value projectdir-component)) 
        (progn (if (value projectdir-component) (popup-message-box "Must select path directory") (popup-message-box "Must input project name")) nil)
      ) 
    )
  )
