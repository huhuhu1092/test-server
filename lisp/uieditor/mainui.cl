;;; Code for the form named :mainui of class frame-window.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
;;;; test for Grapher begin

;;;; test for Grapher end
(defclass my-project-management-area-outline (outline) ())
(defclass project-single-item-list (single-item-list) ())
(defclass project-single-item-list-pane (single-item-list-pane) ())
(defgeneric handle-outline-item-click (item widget dialog) (:documentation "handle outline item click"))
(defclass pbase-outline-item (outline-item) ((name :initarg :name :initform nil :accessor pbase-outline-item-name)))
(defclass project-outline-item (pbase-outline-item) ())
(defclass pitem-outline-item (pbase-outline-item) ())

(defclass imagetable-outline-item (pbase-outline-item) ())
(defclass imageitem-outline-item (pbase-outline-item) ())
(defclass image-outline-item (pbase-outline-item) ())
(defvar *curr-component* nil)
(defmethod initialize-instance :after ((myoutline my-project-management-area-outline) &rest initarg &key &allow-other-keys otherkey)
  (declare (ignorable initarg otherkey))
  (let ((range (create-range (uie:get-all-project))))
    (setf (range myoutline) range)
    )
  )
(defmethod handle-outline-item-click ((item project-outline-item) widget dialog)
  (declare (ignorable widget dialog))
  (set-textarea dialog (value item))
  (setf *curr-component* nil)
  (set-curr-project item)
  )
(defun extract-project-name-path (project-name)
  (uie:tokens project-name #'uie:constituent 0)
  )
(defun create-element-graph ()
  t
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
    (setf *curr-component* (value item))
    (set-contentlist-window dialog file-list))
         
  )
(defgeneric handle-list-item (item-value component) (:documentation "handle click on single item list"))
(defgeneric create-project-item-list-menu (item component) (:documentation "create project item list"))
(defmethod create-project-item-list-menu (item (component (eql :element)))
  (if (null item)
      (list (make-instance 'menu-item :title "Create" :value #'(lambda (window) (element-ui))) 
            (make-instance 'menu-item :title "View" :value 'view))
    (list (make-instance 'menu-item :title "Edit" :value #'(lambda (window) (create-element-graph)))
          (make-instance 'menu-item :title "View" :value #'(lambda (window) (create-element-graph))))
    )
  )

(defmethod handle-list-item (item-value (component (eql :element)))
  #+debug (format t "item = ~a ~%" item-value)
  (create-project-item-list-menu item-value component)
  )
(defmethod handle-list-item (item-value component)
  (if (null component)
      (format t "list item type is nil~%")
    )
  )
(defmethod widget-device ((dialog-item project-single-item-list) dialog)
  (declare (ignorable dialog))
  'project-single-item-list-pane)

(defmethod shortcut-commands ((window project-single-item-list-pane) menu)
  (declare (ignorable menu))
  (let ((si (value (dialog-item window))))
    (handle-list-item si (uie:component-tag *curr-component*))
    )
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
      (when selected-item (handle-outline-item-click selected-item widget dialog))
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
  (format t "curr selected image: ~a~%" *curr-selected-image*)
  (send-to-browser)
  (setf *curr-selected-image* nil)
  t)
(defun create-image-range (images)
  (mapcar #'(lambda (image) (make-instance 'image-outline-item :value (uie:image-id image) :state :closed :selected nil :range nil)) images)
  )
(defun create-imageitem-range (imageitems)
  (mapcar #'(lambda (imageitem) (make-instance 'imageitem-outline-item :value (uie:imageitem-id imageitem) :state :closed :selected nil :range (create-image-range (uie:imageitem-images imageitem)))) imageitems)
  )
(defun create-imagetable-range (file-path-name)
  (let ((imagetablelist (progn (uie:clear-imagetablelist) (uie:parse-image-table file-path-name))))
      (mapcar #'(lambda (imagetable) (make-instance 'imagetable-outline-item :value (uie:imagetable-id imagetable) :state :closed :selected nil :range (create-imageitem-range (uie:imagetable-imageitems imagetable)))) imagetablelist)    
    )
  )
(defun setcontent-to-imagemanager (window file-path-name)
  (let ((imagetable-widget (find-window :image-table window))
	(imagepath-widget (find-window :image-path-name window))
	)
    (setf (range (dialog-item imagetable-widget)) (create-imagetable-range file-path-name))
    (setf (value (dialog-item imagepath-widget)) file-path-name)
    )
  )
(defgeneric process-double-click-content-list (value compoent) (:documentation "process double click contentlist"))
(defmethod process-double-click-content-list (value (component (eql :imagetable)))
   (let ((window (image-manager-ui)))
    (setcontent-to-imagemanager window value)
    )
  )
(defmethod process-double-click-content-list (value (component (eql :image)))
  (format t "process image ~a ~%" value)
  (setf *curr-selected-image* value)
  )
(defun mainui-content-list-on-double-click (dialog widget)
  (declare (ignorable dialog widget))
  (format t "double click ~a ~%" (value widget))
  (format t "curr-compoent: ~a ~%" *curr-component*)
  (process-double-click-content-list (value widget) (uie:component-tag *curr-component*))
  t)
(defmethod process-click-content-list (value (component (eql :image)))
  (format t "process-click : ~a ~%" value)
  (setf *curr-selected-image* value)
  )
(defmethod process-click-content-list (value (component (eql :imagetable)))
  (format t "~a ~a ~%" value component)
  )
(defun mainui-content-list-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  #+debug (format t "content list: ~a ~%" (value widget))
  (when (value widget) (process-click-content-list (value widget) (uie:component-tag *curr-component*)))
  t)
