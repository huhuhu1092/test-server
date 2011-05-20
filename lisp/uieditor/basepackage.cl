(defpackage :uie
  (:use :common-lisp)
  (:export :add-project
	   :delete-project
	   :get-all-project
	   :get-curr-project
	   :set-curr-project
	   :find-project
	   :find-project-by-name
	   :find-project-by-path
	   :get-project-name-with-path
	   :get-project-path-with-name
	   :get-all-project-name
	   :get-all-project-path
	   :get-all-project-name-path
	   :project-num
	   :component-value-and-comment
	   :get-project-name
	   :get-project-path
	   :delete-project
	   :delete-all-project
	   :directory-exist-p
	   :file-exist-p
	   :directory-end-with-path-sep
	   :get-file-path
	   :get-directory-children-string
	   :get-directory-children
	   :create-directory
	   :last-string-with-path-sep
	   :concatenate-string-with-path-sep
	   :contain-name-in-directory?
	   :tokens
           :constituent
           :parse-xml
           :parse-element-list
           :parse-image-table
	   :imagetable-id
	   :imageitem-id
	   :image-id
	   :imageitem-images
	   :imagetable-imageitems
	   :get-image-item
	   :imageitem-image-file-path
	   :clear-imagetablelist
	   :image-component-name
	   :path-sep-change
	   :get-image
	   :image-x
	   :image-y
	   :image-mirror
	   :image-width
	   :image-height
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