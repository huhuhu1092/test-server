;;; Code for the form named :project-ui of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
(defclass project-ui-dialog (dialog) ())
(defmethod close :before ((window project-ui-dialog) &key)
  (format t "close project ui ~%")
  (update-project-area)
  )
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
(defun popup-message-box (window string)
  (pop-up-message-dialog window "Warning" string nil "OK")
  )
(defun print-project-num ()
  (format t "project num : ~a ~%" (uie:project-num))
  )

(defun add-to-uie-project (dialog name path)
  (let ((project (uie:find-project name path))
        )
      (if (null project) (progn (uie:add-project name path) t)
	(progn (popup-message-box dialog
				      (format nil "project name \"~a\" , project path \"~a\"  has been added please use another name or path ~%" name path))
	       nil
	  )
	)
    )
  )
(defun project-ui-button-ok-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (let* ((projectdir-component (find-component :project-directory dialog))
         (projectfile-component (find-component :project-file-name dialog))
         (projectfile-value (value projectfile-component))
         (projectdir-value (value projectdir-component))
        )
    (format t "~a~a~%" projectdir-value projectfile-value)

    (if (and (is-not-null-string projectdir-value) (is-not-null-string projectfile-value)) 
        (if (add-to-uie-project dialog projectfile-value projectdir-value) (close dialog)) 
        (progn (if (is-null-string projectdir-value) 
                   (popup-message-box dialog "Must select path directory") 
                 (popup-message-box dialog "Must input project name")) 
          nil)
      )
    )
  )
(defun project-ui-cancel-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (close dialog)
  t)
