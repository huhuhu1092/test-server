(in-package :uie)
;;element definition 
(defclass element ()
  "
  type -> :null-element :element-node :image-element :action-element :sequence-element :coloreffect-element
  layout-param -> :wrap-content :fill-parent :tile-parent :exact
  visibility -> :visible :invisible
  freeze -> nil t
  "
  ((name :initarg :name :initform nil)
   (x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (w :initarg :w :initform 0)
   (h :initarg :h :initform 0)
   (pivotx :initarg :pivotx :initform 0)
   (pivoty :initarg :pivoty :initform 0)
   (layer :initarg :layer :initform 0)
   (type :initarg :type :initform :null-element)
   (can-pointed :initarg :can-pointed :initform nil)
   (layout-param :initarg :layer-type :initform :wrap-content)
   (layout-modifier :initarg :layout-modifier :initform nil)
   (visibility :initarg :visibility :initform :visible)
   (freeze :initarg :freeze :initform nil)
   )
  )
(defclass element-node (element)
  "
  parent -> parent of this element node
  children -> children of this element node
  "
   ((parent :initarg :parent :initform nil)
    (children :initarg :children :initform nil))  
  )
(defclass image ()
  "
  name -> the name of this image
  x y w h -> the region in this image
  "
  ((name :initarg :name :initform nil)
   (x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (w :initarg :w :initform 0)
   (h :initarg :h :initform 0)
   )
  )
(defgeneric key-frame-value-less-then (v1 v2) (:documentation "return true if v1 less than v2 otherwise return false"))
(defclass key-frame-value ()
  )
(defclass sequence-key-frame-value (key-frame-value)
  )
(defclass action-key-frame-value (key-frame-value)
  ((layer :initarg :layer :initform 0)
   (translate :initarg :translate :initform nil)
   (scale :initarg :scale :initform nil)
   (rotate :initarg :rotate :initform nil)
   (mirror :initarg :mirror :initform nil)
   (content :initarg :content :initform nil)
   )
  )
(defclass mark ()
  "
  fn -> :multiply :add
  "
  ((texture :initarg :texture :initform nil)
   (fn :initarg :fn :initform :multiply)
   (alpha :initarg :alpha :initform 255)
   (color :initarg :color :initform 0)
   (color2 :initarg :color2 :initform 0)
   )
  )
(defclass coloreffect-key-frame-value (key-frame-value)
  ((background :initarg :background :initform nil)
   (channel :initarg :channel :initform nil)
   (main-alpha :initarg :main-alpha :initform 255)
   (marka :initarg :marka :initform nil)
   (markr :initarg :markr :initform nil)
   (markg :initarg :markg :initform nil)
   (markb :initarg :markb :initform nil)
   )
  )
(defclass key-frame ()
  "key -> the time point of key frame
   value -> the content at this time point it is the type of key-frame-value"
  ((key :initarg :key :initform 0)
   (value :initarg :value :initform nil)
   )
  )
(defclass sequence-frame ()
  "
  contents -> list of key frame
  sequence-frame is used to orgnize frame for sequence-element, action-element and coloreffect-element
  "
  ((contents :initarg :contents :initform nil) ;; contents is a list of key-frame
   )
  )
(defclass image-element (element)
  ((image :initarg :image :initform nil))
  )
(defclass sequence-frame-element (element-node)
  ((sequence :initarg :sequence :initform nil))
  )
(defclass action-element (element-node)
  ((sequnce :initarg :action :initform nil))
  )
(defclass coloreffect-element (element-node)
  ((coloreffect :initarg :coloreffect :initform nil))
  )
(defstruct rotate-data angle axis)
(defstruct scale-data sx sy)
(defstruct trans-data tx ty)
(defclass layout-modifier ()
  ((rotate :initarg :rotate :initform nil) ;; rotate-data
   (scale :initarg :scale :initform nil) ;; scale-data
   (translate :initarg :translate :initform nil) ;; translate-data
   (mirror :initarg :mirror :initform :no);; mirror -> :x :y :no
   )
  )
;; help funtion
(defmethod key-frame-value-less-then ((v1 sequence-key-frame-value) (v2 sequence-key-frame-value))
  t
  )
(defmethod key-frame-value-less-then ((v1 action-key-frame-value) (v2  action-key-frame-value))
  (< (slot-value v1 'layer) (slot-value v2 'layer))
  )
(defun value-less-than (v1 v2)
  (key-frame-value-less-than v1 v2)
  )

;;;;;;;;;;;;;;;;;
(defun create-element (name type x y w h layer can-pointed)
  (cond ((eq type :image-element) (make-image-element :name name :type type :x x :y y :w w :h h :layer layer :can-pointed can-pointed))
	((eq type :action-element) (make-action-element :name name :type type :x x :y y :w w :h h :layer layer :can-pointed can-pointed))
	((eq type :sequence-frame-element) (make-sequence-element :name name :type type :x x :y y :w w :h h :layer layer :can-pointed can-pointed))
	((eq type :coloreffect-element) (make-coloreffect-element :name name :type type :x x :y y :w w :h h :layer layer :can-pointed can-pointed))
	((eq type :element-node) (make-element-node :name name :type type :x x :y y :w w :h h :layer layer :can-pointed can-pointed))
	(t (error "can not support this element type"))
	)
  )
(defgeneric set-image (element) (:documentation "set image to element"))
(defgeneric move (element x y) (:documentation "move element to new position"))
(defgeneric move-to (element x y) (:documentation "move element to new position"))
(defmethod set-image ((e image-element) (img image))
  (setf (slot-value e 'image) img)
  )
(defmethod move ((e element) x y)
  (let ((cx (slot-value e 'x))
	(cy (slot-value e 'y)))
    (setf (slot-value e 'x) (+ cx x) (slot-value e 'y) (+ cy y))
    )
  )
(defmethod move-to ((e element) x y)
  (setf (slot-value e 'x) x (slot-value e 'y) y)
  )
(defun create-keyframe (key frame-value)
  (make-key-frame :key key :frame frame-value)
  )

(defun add-sequence (sequence key-frame)
  "
  add-sequence will add key-frame to sequence contents, it will
  sort the key frame in contents. The first sort criteria is the key
  the other is the value sort criteria
  "
  (let ((cont-list (slot-value sequence 'contents)))
    (push key-frame cont-list)
    (let ((sorted-list (sort cont-list #'(lambda (k1 k2) (or (< (slot-value k1 'key) (slot-value k2 'key))
							     (and (= (slot-value k1 'key) (slot-value k2 'key))
								  (value-less-than (slot-value k1 'value) (slot-value k2 'value))))))))
      (setf (slot-value sequence 'contents) sorted-list)
      )
    )
  )
