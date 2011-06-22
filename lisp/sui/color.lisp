(in-package :sui)
;;type := <:rgb16 :rgb24 :rgb32>
(defclass Color () ((r :initarg :r :initform 0 :accessor color-r)
		    (g :initarg :g :initform 0 :accessor color-g)
		    (b :initarg :b :initform 0 :accessor color-b)
		    (a :initarg :a :initform 0 :accessor color-a)
		    (type :initarg :type :initform :rgb32 :accessor color-type)
		    ))

(defun make-color (r g b a type)
  (make-instance 'Color :r r :g g :b b :a a :type type)
  )