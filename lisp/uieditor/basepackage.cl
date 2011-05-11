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
#+mswindows (load "C:\\myprogram\\simpleengine\\test-server\\lisp\\uieditor\\base\\uieditor.cl")
#+mswindows (load "C:\\myprogram\\simpleengine\\test-server\\lisp\\uieditor\\baseinterface.cl")
#-mswindows (load "~/program/streamserver_git/test-server/lisp/uieditor/base/uieditor.cl")
#-mswindows (load "~/program/streamserver_git/test-server/lisp/uieditor/baseinterface.cl")