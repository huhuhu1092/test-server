;;; Code for the form named :project-ui of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
(defclass project-ui-dialog (dialog) ())
(defmethod close :after ((window project-ui-dialog) &key)
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
(defun is-null-string (str)
  (string= str "")
  )
(defun is-not-null-string (str)
  (not (is-null-string str))
  )
(defun add-project (name path)
  (let ((project-num (uie:project-num))
        (canAdd (not (or (uie:is-contain-project-name name) (uie:is-contain-project-path path))))
        )
      (format t "project num: ~a , ~a ~% $$ add: ~a, ~a~%" project-num uie::*projects* name path)
      (if canAdd (uie:add-project name path) (format t "the \"~a\" project or the path \"~a\" has been added~%" name path))
    )
  )
(defun get-projectname ()
  (uie:list-project-name)
  )
(defun create-range (projectname)
  (mapcar #'(lambda (name) (make-instance 'outline-item :value name :state :closed :selected nil :range nil)) projectname)
  )
(defun update-dialog-item (dialog-item range)
  (setf (range dialog-item) range) 
  )
(defun update-project-area ()
  (let* ((mainui (find-window :mainui))
         (projectarea (if mainui (find-window :PROJECT-MANAGEMENT-AREA mainui) nil))
         (projectname (get-projectname))
         (rangev (create-range projectname))
         )   
    (when projectarea (progn (format t "update-project-area : ~a , ~a ~%" mainui projectarea) 
                        (update-dialog-item (dialog-item projectarea) rangev) (invalidate projectarea)))
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
        (progn (add-project projectfile-value projectdir-value) (close dialog)) 
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
