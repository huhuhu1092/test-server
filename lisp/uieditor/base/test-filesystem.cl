(in-package :uie)
(format t "start test-filesystem~%")
(deftest ((setf a (directory-end-with-path-sep "E:\\aa")) (print a))
  ((string= a "E:\\aa\\")) nil)
(deftest ((setf b (directory-exist-p "E:\\aa")))
  ((equal b nil)) nil)
(deftest ((setf c (directory-exist-p "E:\\bb\\")))
  ((equal c nil)) nil)

(deftest ((setf d (directory-end-with-path-sep "E:\\cc\\")))
  ((equal d "E:\\cc\\")) nil)

(deftest ((setf k (file-exist-p "E:\\test1")))
  ((equal k nil)) nil)

(deftest ((setf m (create-directory "E:\\test")))
  ((equal m t)) ((excl:delete-directory (make-pathname :device "E" :directory '(:absolute "test")))))
(format t "end test-filesystem ~%")