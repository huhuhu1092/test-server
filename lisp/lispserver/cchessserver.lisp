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
(defun pop-message (wuser)
  (let ((mq (www-user-messagequeue wuser)))
    (let ((ml (www-message-queue-messagelist mq)))
      (setf (www-message-queue-messagelist) (reverse (cdr (reverse ml))))))
)
(defun user-in (name world)
  (let ((userlist (www-world-userlist world)))
    (member-if #'(lambda (n) (string= (www-user-name n) name)) userlist))
)  
(defun add-user (name password)
  (let ((cuser (make-www-user :name name :password password :messagequeue (make-www-message-queue))))
    (let ((userlist (www-world-userlist *world*)))
      (setf (www-world-userlist *world*) (cons cuser userlist))))
)
(defun get-user (name)
  (let ((u (user-in name *world*)))
    (car u)
    )
)
(defun login (name password)
    (unless (user-in name *world*) (add-user name password))
)  
(defun make-chess-session (red black)
  (list 'cchess (make-www-chess-session :red red :black black))
)
(defun add-chess-session (red black)
  (push (make-chess-session red black) (www-world-sessionlist *world*))
)
(defun get-session (session-type)
  (mapcar #'(lambda (s) (if (equal session-type (car s)) (cadr s))) (www-world-sessionlist *world*))
)
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
  (format t "~a, ~a" s (www-world-sessionlist *world*))
  (equal (www-chess-session-red (car s)) "aa")
  (equal (www-chess-session-black (car s)) "bb")
)
(test-case)
