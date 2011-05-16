;;; Code for the form named :mainui of class frame-window.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
(defclass my-project-management-area-outline (outline) ())

;(defclass project-name-outline-item (outline-item) ())

(defmethod initialize-instance :after ((myoutline my-project-management-area-outline) &rest initarg &key &allow-other-keys  otherkey)
  (let ((range (create-range (uie:get-all-project))))
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
      #+debug (format t "project name : ~a ~a ~%" project-name widget)
      ;#+debug (format t "~a , ~a ~%" textarea (uie:get-project-path project-name))
      (setf (value (dialog-item textarea)) project-name)
      (let ((tokens (uie:tokens project-name #'uie:constituent 0)))
	(if (= (length tokens) 2)
	    (let ((project (uie:find-project (car tokens) (cadr tokens))))
	      (uie:set-curr-project project))))
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
  (format t "delete project ~a ~%" window)
  (if (uie:get-curr-project) (uie:delete-project (uie:get-project-name (uie:get-curr-project)) (uie:get-project-path (uie:get-curr-project))))
  (update-project-area)

  )
(defun create-outline-item-range (project)
  (let* ((component-value-comment (uie:component-value-and-comment project))
         (component-valuelist (mapcar #'(lambda (v) (car v)) component-value-comment)))
	(mapcar #'(lambda (component-value) (make-instance 'outline-item :value component-value :state :closed :selected nil
							   :range nil)) component-valuelist)
     )
  )
(defun project-to-string (project)
  (concatenate 'string (uie:get-project-name project) "  " (uie:get-project-path project))
  )
(defun create-range (projects)
  "create range for outline.
   projectname is a list of the name in uie project
   precondition: True
   postcondition: return nil or list of outline-item
  "
  (mapcar #'(lambda (p) (make-instance 'outline-item :value (project-to-string p) :state :closed :selected nil :range (create-outline-item-range p))) projects)
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
         (projects (uie:get-all-project))
         (rangev (create-range projects))
         )
    (print-project-num)
    (when projectarea (progn (format t "update-project-area : ~a , ~a ~%" mainui projectarea) 
                        (update-dialog-item (dialog-item projectarea) rangev) (invalidate projectarea)))
    )
        
  )



(defun mainui-project-management-area-on-double-click (dialog widget)
  (declare (ignorable dialog widget))
  
  t)
