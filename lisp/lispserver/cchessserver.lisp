(defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position delimiter seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position delimiter seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

(defun split-sequence-if (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (when key-supplied 
		      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if predicate seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if predicate seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

(defun split-sequence-if-not (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
(CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."				; Emacs syntax highlighting is broken, and this helps: "
  (let ((len (length seq))
	(other-keys (when key-supplied 
		      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if-not predicate seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if-not predicate seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Server code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct www-message-queue
           messagelist
)
(defstruct www-message
           from
           op
           content
)
(defstruct www-user-state
           life
)
(defstruct www-user
           name
           password
           state
           messagequeue
)
(defstruct www-world 
           userlist ;; the user list
           sessionlist ;; the session set 
)
(defstruct www-chess-session
           red
           black
)
(defparameter *world* nil)
(defconstant +MOVE+ 'move)
(defconstant +SAY+ 'say)

(setq *world* (make-www-world :userlist nil))
;;;;;;;;;;;;;;;;;;;
;;; implement
;;;;;;;;;;;;;;;;;;;
(defun send-message (message to)
    (let ((wuser (get-user to)))
      (when wuser 
        (let ((mq (www-user-messagequeue wuser)))
          (setf (www-message-queue-messagelist mq) (cons message (www-message-queue-messagelist mq)))
        )
      )
    )
)
(defun get-top-message (wuser)
  (let ((mq (www-user-messagequeue wuser)))
    (let ((ml (www-message-queue-messagelist mq)))
      (car (last ml))))
)
(defun pop-all-message (wuser)
  (let ((mq (www-user-messagequeue wuser)))
    (let ((ml (www-message-queue-messagelist mq)))
      (setf (www-message-queue-messagelist mq) nil)
    )
  )

)
(defun pop-message (wuser)
  (let ((mq (www-user-messagequeue wuser)))
    (let ((ml (www-message-queue-messagelist mq)))
      (setf (www-message-queue-messagelist mq) (reverse (cdr (reverse ml))))
    )
  )
)
(defun user-in (name world)
  (let ((userlist (www-world-userlist world)))
    (member-if #'(lambda (n) (string= (www-user-name n) name)) userlist))
)  
(defun add-user (name password)
  (if name
  (let ((cuser (make-www-user :name name :password password :messagequeue (make-www-message-queue))))
    (let ((userlist (www-world-userlist *world*)))
      (setf (www-world-userlist *world*) (cons cuser userlist)))) )
)
(defun get-user (name)
  (let ((u (user-in name *world*)))
    (car u)
    )
)
(defun login (name password)
    (unless (user-in name *world*) (add-user name password))
)
(defun make-chess-sym (red black)
  (with-output-to-string (s)
    (format s "~a~a" (if red red "") (if black black ""))
  )
)
(defun make-chess-session (red black)
  (let ((sym (make-chess-sym red black))
        (sym1 (make-chess-sym black red)))
      (if (find-if #'(lambda (n) (or (equal sym (cadr n)) (equal sym1 (cadr n)))) (www-world-sessionlist *world*))
        nil
       (list 'cchess sym (make-www-chess-session :red red :black black)))
  )
)
(defun session-exist? (sym sessionlist)
    (find-if #'(lambda (s) (if (equal sym (cadr s)) t nil)) sessionlist)
)
(defun add-chess-session (red black)
  (let* (
          (s (make-chess-session red black))
          (sym (cadr s))
        )
    (if (null sym)
        (list 'existed-sym (make-chess-sym black red))
        (progn (if (not (session-exist? sym (www-world-sessionlist *world*)))
                   (push s (www-world-sessionlist *world*))
               )
               (list 'not-exsited-sym sym)
        )
    )
  )
)
(defun get-session-by-name (session-name)
  (let (
        (session (mapcar #'(lambda (s) (if (equal session-name (cadr s)) (caddr s))) (www-world-sessionlist *world*)))
       )
       (format t "~a~%" session)
       (first session)
  )
)
(defun get-session (session-type)
  (mapcar #'(lambda (s) (if (equal session-type (car s)) (caddr s))) (www-world-sessionlist *world*))
)
(defun get-all-user ()
    (mapcar #'(lambda (u) (www-user-name u)) (www-world-userlist *world*))
)
(defun get-all-user-to-string ()
    (with-output-to-string (s)

         (dolist (obj (get-all-user))
            (format s "~a " obj)
        )
    )
)
;;;;;;;;;;;
(defun handle-login (content)
  (let ((name-password (split-sequence #\Space content)))
      (let ((name (car name-password)) (password (cadr name-password)))
          (login name password)
          (get-all-user-to-string)
      )
  )
)
(defun get-chess-user-by-tag (tag session)
  (cond ((equal tag "red") (www-chess-session-red session))
        ((equal tag "black") (www-chess-session-black session))
        (t nil)
  )
)
(defun opponent-chess-tag (tag)
  (cond ((equal tag "red") "black")
        ((equal tag "black") "red")
        (t nil)
  )
)
;; the first of content is the session string
;; the second of content is the red or black
;; the third of content is move directive
(defun handle-move (content)
  (let* (
        (content-list (split-sequence #\Space content)) 
        (session-name (car content-list)) 
        (tag (cadr content-list))
        (directive (caddr content-list))
        (session (get-session-by-name session-name)) 
        (fromusername (get-chess-user-by-tag tag session)) 
        (tousername (get-chess-user-by-tag (opponent-chess-tag tag) session))
       )
    (format t "to user : ~a ~a ~a~%" tag tousername directive)
    (send-message (make-www-message :from fromusername :op 'move :content directive) tousername)
    "send move ok" 
  )
)
;; first is my own user
;; second is opponent
;; return: sessionname  (red or black)
(defun handle-start (content)
    (let ((self-opponent (split-sequence #\Space content)))
      (let ((self (car self-opponent)) (opponent (cadr self-opponent)))
         (let ((sym (add-chess-session self opponent)))
           (if (equal (car sym) 'not-exsited-sym)
             (progn (send-message (make-www-message :from self :op 'start :content (list (cadr sym) 'black)) opponent)
                   (with-output-to-string (s)
                     (format s "~a ~a" (cadr sym) "red")
                   )
             )
             (with-output-to-string (ms)
                 (format ms "~a ~a" (cadr sym) "black")
             )
          )
       ) 
     )
    )
)
(defun message-content-to-string (op content)
    (cond (
           (equal op 'start) 
           (with-output-to-string (s)
             (dolist (obj content)
               (format s "~a " obj)
             )
           )
          )
          ((equal op 'move) content)
    )
)
(defun message-op-to-string (op)
  (cond ((equal op 'move) "move")
        ((equal op 'start) "start")
        (t "nil")
  )
)
(defun message-to-string (message s)
  (format s "~a ~a ~a" (message-op-to-string (www-message-op message)) (www-message-from message) (message-content-to-string (www-message-op message) (www-message-content message)))
)
;;first is user name
;;second is command: getuser, getallmessage
(defun handle-getmessage (content)
    (let ((message (split-sequence #\Space content)))
       (cond ((equal "getuser" (car message)) (get-all-user-to-string))
        ((equal "getallmessage" (car message))
         (let* (
                 (wuser (get-user (cadr message))) 
                 (mq (if wuser (www-user-messagequeue wuser) nil)) 
                 (ml (if mq (www-message-queue-messagelist mq) nil))
                 (command (car message))
             )
            (progn
                (if ml
                    (progn (pop-all-message wuser)
                        (with-output-to-string (s)
                            (format s "~a " (length ml))
                            (dolist (obj ml)
                                (message-to-string obj s)
                                (format s "~%")
                            )
                        )
                    )
                    nil
                )
            )
           )
        )
        (t "is not getmessage command")
   )
)
)
(defun remove-session-from-world (username)
  (let ((leftsession (remove-if #'(lambda (session) (or (equal (www-chess-session-red session) username) (equal (www-chess-session-black session) username))) (www-world-sessionlist *world*))))
      (setf (www-world-sessionlist *world*) leftsession)
   )
)
(defun handle-logout (content)
    (remove-user-from-world content)
    (remove-session-from-world content)
)
(defun posted-content (command)
  (let (s (cdr (assoc "posted-content" command :test #'string=)))
    (format t "~a~%" s)
    s
  )
)
(defun handle-command (url command)
  (cond ((equal url "/cchess/login") (handle-login (posted-content command)))
        ((equal url "/cchess/move") (handle-move (posted-content command)))
        ((equal url "/cchess/start") (handle-start (posted-content command)))
        ((equal url "/cchess/getmessage") (handle-getmessage (posted-content command)))
        ((equal url "/cchess/logout") (handle-logout (posted-content command)))
        (t "can not handle this command")
  )
)
(defun handle-get-login (command-list)
   (login (car command-list) (cadr command-list))
   (get-all-user-to-string)
)
(defun handle-get-move (content-list)
  (let* ( 
          (session-name (car content-list)) 
          (tag (cadr content-list))
          (directive (caddr content-list))
          (session (get-session-by-name session-name)) 
          (fromusername (get-chess-user-by-tag tag session)) 
          (tousername (get-chess-user-by-tag (opponent-chess-tag tag) session))
        )
    (format t "to user : ~a ~a ~a~%" tag tousername directive)
    (send-message (make-www-message :from fromusername :op 'move :content directive) tousername)
    "send move ok" 
    )
)
(defun handle-get-getmessage (message)
  (cond ((equal "getuser" (car message)) (get-all-user-to-string))
        ((equal "getallmessage" (car message))
         (let* (
                 (wuser (get-user (cadr message))) 
                 (mq (www-user-messagequeue wuser)) 
                 (ml (www-message-queue-messagelist mq))
                 (command (car message))
             )
            (progn
                (pop-all-message wuser)
                (with-output-to-string (s)
                    (format s "~a~%" (length ml))
                    (dolist (obj ml)
                      (message-to-string obj s)
                      (format s "~%")
                    )
                )
            ))
        )
        (t "is not getmessage command")
   )

)
(defun remove-user-from-world (username)
    (let* ((removeduser (remove-if #'(lambda (n) (equal username (www-user-name n))) (www-world-userlist *world*)))
          )
          (setf (www-world-userlist *world*) removeduser)
    )
)
(defun handle-get-logout (message)
    (let* ((wusername message)
          )
          (remove-user-from-world wusername)
          (remove-session-from-world wusername)
    )
)
(defun handle-get-command (command)
    (let* ((url-content (cdr (assoc "url" command :test #'string=)))
           (url-list (split-sequence #\? url-content))
           (command-list (split-sequence #\& (cadr url-list)))
           (command (car command-list))
          )
          (cond ((equal command "login") (handle-get-login (cdr command-list)))
                ((equal command "logout") (handle-get-logout (cdr comand-list)))
                ((equal command "move") (handle-get-move (cdr command-list)))
                ((equal command "getmessage") (handle-get-getmessage (cdr command-list)))
                (t "can not handle this command")
          )
    )
)
