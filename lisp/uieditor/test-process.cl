(defparameter *event-queue* nil)
(defparameter *event-queue-lock* (mp:make-process-lock :name "queue"))
(defparameter *exit* 0)
(defconstant +EXIT+ 0)
(defun reverse-tree (l)
  (cond ((null l) nil)
        ((atom l) l)
        (t (let ((first (reverse-tree (cdr l)))
                 (second (reverse-tree (car l))))
             (cond ((and (null first) (atom second)) (list second)) 
                   ((and (null first) (consp second)) (cons second nil))
                   ((not (null first)) (append first (list second)))
                   (t (error "can not go to here"))
		   )
	     )
	   )
	)
  )
(defstruct xqueue
  queue
  lock
  gate
  process
  )
(defun xqueue-add (xq item)
  (mp:with-process-lock ((xqueue-lock xq))
			(setf (xqueue-queue) (nconc (xqueue-queue xq) (list item)))
			(mp:open-gate (xqueue-gate xq))
			)
  )
(defun xqueue-pop (xq)
  (mp:
  )
(defstruct socket-reader
  sock
  queue
  lock
  process
  gate
  )
(defstruct socket-writer
  sock
  queue
  lock
  process
  gate
  )
(defun add-event (e)
  (mp:with-process-lock (*event-queue-lock*)
			(push e *event-queue*)
			)
  )
(defun pop-event ()
  (mp:with-process-lock (*event-queue-lock*)
			(let ((e (car (last *event-queue*))))
			  (when e (setf *event-queue* (subseq *event-queue* 0 (- (length *event-queue*) 1))) e)
			  )
			)
  )
(defun my-handle-data (e)
  (format t "data = ~a ~%" e)
  (when (= e 3) (setf *exit* 2))
  )

(defun my-loop ()
  (loop (when (= *exit* 2) (return-from my-loop 1))
	(sleep 3)
	(let ((e (pop-event)))
	  (if e (my-handle-data e))
	  )
	)
  )
(defun create-my-loop ()
  (mp:process-run-function "test" #'my-loop)
  )
(defun socket-reader-loop (sock)
  (loop 
  )
(defun read-data-from-socket (sock)
  (let* ((buffer (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0))
	 (num (read-sequence buffer sock))
	 )
      (subseq buffer 0 num)
    )
  )
(defun connect-to-tcp-server (server-ip server-port)
  (let ((s (socket:with-pending-connect
	    (sys:with-timeout (10 (error "connect failed"))
			      (socket:make-socket :remote-host server-ip
						  :remote-port server-port
						  :type :stream
						  :address-family :internet)))))
    (mp:process-run-function "socketreader" #'socket-reader-loop s)
    )
  )