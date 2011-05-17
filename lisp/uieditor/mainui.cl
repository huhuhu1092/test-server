;;; Code for the form named :mainui of class frame-window.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
(defclass my-project-management-area-outline (outline) ())

(defgeneric handle-outline-item-click (item widget dialog) (:documentation "handle outline item click"))
(defclass pbase-outline-item (outline-item) ((name :initarg :name :initform nil :accessor pbase-outline-item-name)))
(defclass project-outline-item (pbase-outline-item) ())
(defclass pitem-outline-item (pbase-outline-item) ())
(defmethod initialize-instance :after ((myoutline my-project-management-area-outline) &rest initarg &key &allow-other-keys otherkey)
  (declare (ignorable initarg otherkey))
  (let ((range (create-range (uie:get-all-project))))
    (setf (range myoutline) range)
    )
  )
(defmethod handle-outline-item-click ((item project-outline-item) widget dialog)
  (declare (ignorable widget dialog))
  (set-textarea dialog (value item))
  (set-curr-project item)
  )
(defun extract-project-name-path (project-name)
  (uie:tokens project-name #'uie:constituent 0)
  )
(defmethod handle-outline-item-click ((item pitem-outline-item) widget dialog)
  (declare (ignorable widget dialog))
  (set-textarea dialog (value (parent item)))
  (set-curr-project (parent item))
  (let* ((project-name-path (extract-project-name-path (value (parent item))))
         (directory (uie:concatenate-string-with-path-sep (cadr project-name-path) (car project-name-path) (value item)))
         (file-list (uie:get-directory-children-string directory))
         )
    #+debug (format t "directory : ~a ~%" directory)
    #+debug (format t "file-list : ~a ~%" file-list)
    (set-contentlist-window dialog file-list))
         
  )
(defun set-curr-project (item)
    (let* ((project-name (value item))
           (tokens (extract-project-name-path project-name)))
	(if (= (length tokens) 2)
	    (let ((project (uie:find-project (car tokens) (cadr tokens))))
	      (uie:set-curr-project project))))
  )
(defun set-contentlist-window (dialog file-list)
  (let ((contentlist (find-window :CONTENT-LIST dialog)))
    (setf (range (dialog-item contentlist)) file-list)
    )
  )
(defun set-textarea (dialog str)
  (let ((textarea (find-window :PROJECT-PATH-NAME dialog)))
    (setf (value (dialog-item textarea)) str)
    (invalidate textarea)
    )
  )
(defun mainui-project-management-area-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
    (let ((selected-item (selected-outline-item widget))
          )
      #+debug (format t "project name : ~a ~a ~%" selected-item widget)
      (handle-outline-item-click selected-item widget dialog)
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
	(mapcar #'(lambda (component-value) (make-instance 'pitem-outline-item :value component-value :state :closed :selected nil
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
  (mapcar #'(lambda (p) (make-instance 'project-outline-item :value (project-to-string p) :state :closed :selected nil :range (create-outline-item-range p))) projects)
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

(defun mainui-button10-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (load "E:\\programs\\test_win32\\Debug\\test_win32.dll")
  (ff:def-foreign-call (StartUI "StartUI") (c))
  (StartUI 1)
  ;;(unload-foreign-library "E:\\programs\\test_win32\\Debug\\test_win32.dll")
  t)
