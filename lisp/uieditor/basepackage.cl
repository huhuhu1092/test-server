(defpackage :uie
  (:use :common-lisp)
  (:export :add-project
	   :delete-project
	   :list-project
	   :list-project-name
	   :list-project-path
	   :is-contain-project-name
	   :is-contain-project-path
	   :project-num
	   ))
(load "C:\\myprogram\\simpleengine\\test-server\\lisp\\uieditor\\base\\uieditor.cl")