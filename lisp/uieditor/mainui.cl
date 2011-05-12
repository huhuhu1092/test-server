;;; Code for the form named :mainui of class frame-window.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
(defclass my-project-management-area-outline (outline) ())

;(defclass project-name-outline-item (outline-item) ())

(defmethod initialize-instance :after ((myoutline my-project-management-area-outline) &rest initarg &key &allow-other-keys  otherkey)
  (let ((range (create-range (get-all-project-name))))
    (setf (range myoutline) range)
    )
  )
(defun mainui-project-management-area-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
    (let ((textarea (find-window :PROJECT-PATH-NAME (find-window :mainui)))
	(project-name (value widget))
          )
      #+debug (format t "~a , ~a ~%" textarea (get-project-path project-name))
      (setf (value (dialog-item textarea)) (get-project-path project-name))
      (invalidate textarea)
    )
  )
(defun sui-create-project (window)
  (format t "click ~a~%" window)
  (project-ui)
  ;;(make-project-ui)
  (format t "after project ui~%")
  )
(defun sui-delete-project (window)
  (format t "delete project ~a~%" window)
  )
(defun create-outline-item-range (projectname)
  (let* ((component-value-comment (get-project-component-value-and-comment projectname))
         (component-valuelist (mapcar #'(lambda (v) (car v)) component-value-comment)))
	(mapcar #'(lambda (component-value) (make-instance 'outline-item :value component-value :state :closed :selected nil
							   :range nil)) component-valuelist)
     )
  )
(defun create-range (projectnamelist)
  "create range for outline.
   projectname is a list of the name in uie project
   precondition: True
   postcondition: return nil or list of outline-item
  "
  (mapcar #'(lambda (name) (make-instance 'outline-item :value name :state :closed :selected nil :range (create-outline-item-range name))) projectnamelist)
  )

(defun update-dialog-item (dialog-item range)
  "
    set the range of the dialog-item
  "
  (setf (range dialog-item) range) 
  )
(defun update-project-area ()
  (let* ((mainui (find-window :mainui))
         (projectarea (if mainui (find-window :PROJECT-MANAGEMENT-AREA mainui) nil))
         (projectname (get-all-project-name))
         (rangev (create-range projectname))
         )
    (print-project-num)
    (when projectarea (progn (format t "update-project-area : ~a , ~a ~%" mainui projectarea) 
                        (update-dialog-item (dialog-item projectarea) rangev) (invalidate projectarea)))
    )
        
  )


