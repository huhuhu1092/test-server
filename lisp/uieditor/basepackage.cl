(defpackage :uie
  (:use :common-lisp)
  (:export :add-project
	   :delete-project
	   :get-all-project
	   :find-project
	   :find-project-by-name
	   :find-project-by-path
	   :get-project-name-with-path
	   :get-project-path-with-name
	   :get-all-project-name
	   :get-all-project-path
	   :project-num
	   :component-value-and-comment
	   :get-project-name
	   :get-project-path
	   :clear-project
	   :clear-all-project
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