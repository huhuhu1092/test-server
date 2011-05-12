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
	   :get-project-path
	   :component-value-and-comment
	   ))
(pushnew :debug *features*)
#+(and mswindows officepc) (defvar *codedirectory* "C:\\myprogram\\simpleengine\\test-server\\lisp\\uieditor\\")
#+(and mswindows homepc) (defvar *codedirectory* "E:\\programs\\streamserver_git\\test-server\\lisp\\uieditor\\")
#+linux (defvar *codedirectory* "~/program/streamserver_git/test-server/lisp/uieditor/")

;#+mswindows (load (concatenate 'string *codedirectory* "\\base\\uieditor.cl"))
;#+mswindows (load (concatenate 'string *codedirectory* "\\base\\test-uieditor.cl"))
;#+mswindows (load (concatenate 'string *codedirectory* "\\baseinterface.cl"))


;#-mswindows (load (concatenate 'vector *codedirectory* "/base/uieditor.cl"))
;#-mswindows (load (concatenate 'vector *codedirectory* "/base/test-uieditor.cl"))
;#-mswindows (load (concatenate 'vector *codedirectory* "/baseinterface.cl"))