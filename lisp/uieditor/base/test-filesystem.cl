(in-package :uie)
(format t "start test-filesystem~%")
(deftest ((setf a (directory-end-with-path-sep "c:\\aak")) (print a))
  ((string= a "c:\\aak\\")) nil)

(deftest ((setf c (directory-exist-p "c:\\bbk\\")))
  ((equal c nil)) nil)


(deftest ((setf k (file-exist-p "c:\\test1")))
  ((equal k nil)) nil)

(deftest ((setf m (create-directory "c:\\test")))
  ((equal m t)) ((excl:delete-directory (make-pathname :device "E" :directory '(:absolute "test")))))
(format t "end test-filesystem ~%")