(load "cchessserver.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-case()
  (add-user "aa" "aa")
  (add-user "bb" "bb")
  (setf aa (get-user "aa"))
  (setf bb (get-user "bb"))
  (string= (www-user-name aa) "aa")
  (string= (www-user-name bb) "bb")
  (add-chess-session "aa" "bb")
  (setf s (get-session 'cchess))
  (format t "~a, ~a ~%" s (www-world-sessionlist *world*))
  (if (equal (www-chess-session-red (car s)) "aa") (format t "~a~%" "red in"))
  (if (equal (www-chess-session-black (car s)) "bb") (format t "~a~%" "black in"))
  (setf au (get-all-user))
  (format t "~a ~a~%" au (listp au))
  (if (member "aa" au :test #'equal) (format t "~a~%" "aa in"))
  (if (member "bb" au :test #'equal) (format t "~a~%" "bb in"))
  (setf command '(("posted-content" . "aa aa")))
  (format t "handle login result: ~a~%" (handle-command "/cchess/login" command))
  (setf command '(("posted-content" . "bb bb")))
  (format t "handle login result: ~a~%" (handle-command "/cchess/login" command))
  (setf command '(("posted-content"."aa bb")))
  (setf start-back (handle-command "/cchess/start" command))
  (format t "handle start result ~a~%" start-back)
  (setf command '(("posted-content" . "bb")))
  (setf opponent (handle-command "/cchess/getmessage" command))
  (format t "opponent message: ~a ~%" opponent)
  (setf command '(("posted-content" . "aabb red 0919")))
  (setf move-back (handle-command "/cchess/move" command))
  (format t "move-back : ~a ~%" move-back)
  (setf command '(("posted-content" . "bb")))
  (setf opponent (handle-command "/cchess/getmessage" command))
  (format t "opponent message: ~a ~%" opponent)
  (pop-all-message aa)
  (pop-all-message bb)
)
(test-case)
