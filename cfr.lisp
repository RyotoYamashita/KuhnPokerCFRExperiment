(defvar *N-ACTIONS* 2)
(defvar *N-CARDS* 3)

(defun main ()
  (let ((i-map (make-hash-table :test #'equal))
        (n-iterations 10000)
        (expected-game-value))
    (setf expected-game-value
	  (loop for i below n-iterations
	     sum (cfr i-map)
	     do (loop for key being the hash-key of i-map
		   using (hash-value info-set)
		   do (next-strategy info-set))))
    (setf expected-game-value
	  (/ expected-game-value n-iterations))
    (display-results expected-game-value i-map)
    i-map))

(defun check-cfr ()
(let ((i-map (make-hash-table :test #'equal))
        (n-iterations 10000)
        (expected-game-value))
    (setf expected-game-value
	  (loop for i below n-iterations
	     sum (cfr i-map)
	     do (loop for key being the hash-key of i-map
		   using (hash-value info-set)
		   do (next-strategy info-set))
	     if (member i '(0 10 100 1000 10000))
	     do (display-results (/ -1 18) i-map)
	       ))
    (setf expected-game-value
	  (/ expected-game-value n-iterations))
    (display-results expected-game-value i-map)
    i-map)
  )

(defun print-i-map (i-map)
  (loop for key being each hash-key of i-map
     using (hash-value info-set)
     do (format t "~a~%~a~%" key (describe info-set))))

;;;Counterfactual regret minimization algorithm
;;;
;;;Parameters
;;;
;;;i_map: hash-table
;;;    Dictionary of all information sets
;;;hisory: ({'r', 'c', 'b'}), str
;;;    A string representation of the game tree path we have taken
;;;    Each character of the string represents a single action:
;;;
;;;        'r': random chance action
;;;        'c': check action
;;;        'b': bet action 
;;;card-1 : (0, 2), int
;;;    player A's card
;;;card-2 : (0, 2), int
;;;    player B's card
;;;pr-1 : (0, 1.0), float
;;;    The probability that player A reaches `history`.
;;;pr-2 : (0, 1.0), float
;;;    The probability that player B reaches `history`.
;;;pr-c: (0, 1.0), float
;;;    The probability contribution of chance events to reach `history`.
;;; todo optional
(defun cfr (i-map &optional (history "") (card-1 -1) (card-2 -1)
			     (pr-1 1) (pr-2 1) (pr-c 1))
  (cond
    ((is-chance-node history) (chance-util i-map))
    ((is-terminal history) (terminal-util history card-1 card-2))
    (t (let* ((is-player-1 (evenp (length history)))
	      (card-player (if is-player-1 card-1 card-2))
	      (info-set (get-info-set i-map card-player history))
	      (strategy (strategy info-set))
	      ;; Counterfactual utility per action.
	      (action-utils
	       (loop for i from 0 below *N-ACTIONS* collect 0))
	      (util)
	      (regrets)
	      )
	 (setf (reach-pr info-set)
	       (if is-player-1
		   (+ (reach-pr info-set) pr-1)
		   (+ (reach-pr info-set) pr-2)))
	 (loop for i from 0 below *N-ACTIONS*
	    and action in '("c" "b")
	    as next-history = (format nil "~a~a" history action)
	    if is-player-1
	    do (setf (nth i action-utils)
		     (* -1
			(cfr i-map next-history card-1 card-2
			     (* pr-1 (nth i strategy))
			     pr-2 pr-c)))
	    else
	    do (setf (nth i action-utils)
		     (* -1
			(cfr i-map next-history card-1 card-2
			     pr-1
			     (* pr-2 (nth i strategy))
			     pr-c))))
	 (setf util
	       (loop for action-util in action-utils
		  and stra-prob in strategy
		  sum (* action-util stra-prob)))

	 (setf regrets (loop for action-util in action-utils
			  collect (- action-util util)))

	 (setf (regret-sum info-set)
	       (if is-player-1
		   (loop for regret in regrets
		      and regret-sum-elm in (regret-sum info-set)
		      collect (+ regret-sum-elm
				 (* pr-2 pr-c regret)))
		   (loop for regret in regrets
		      and regret-sum-elm in (regret-sum info-set)
		      collect (+ regret-sum-elm
				 (* pr-1 pr-c regret)))))
	 util))))
 
(defun is-chance-node (history)
  (equal "" history))

(defun chance-util (i-map)
  (let* ((n-possibilities 6)
         (expected-value
          (loop for i below *N-CARDS*
              sum (loop for j below *N-CARDS*
                      sum (if (not (equal i j))
                              (cfr i-map "rr" i j 1 1
				   (/ 1.0 n-possibilities))
                                         0)))))
    (/ expected-value n-possibilities)))

(defun is-terminal (history)
  (let ((possibilities (list "rrcc" "rrcbc" "rrcbb" "rrbc" "rrbb")))
    (if (member history possibilities :test #'equal) t nil)))

(defun terminal-util (history card-1 card-2)
  (let* ((n (length history))
         (card-player (if (evenp n) card-1 card-2))
         (card-opponent (if (evenp n) card-2 card-1))
         )
    (cond ((or (equal "rrcbc" history) (equal "rrbc" history)) 1)
          ((equal "rrcc" history) 
           (if (> card-player card-opponent) 1 -1))
          ((or (equal "rrcbb" history) (equal "rrbb" history))
           (if (> card-player card-opponent) 2 -2))
          (t nil))))

(defun card-str (card)
  (cond ((equal card 0) "J")
        ((equal card 1) "Q")
        ((equal card 2) "K")
	(t (format t "error ~a is not a card number" card))))

(defun get-info-set (i-map card history)
  (let ((key (format nil "~a ~a" (card-str card) history))
	(info-set))
    (unless (gethash key i-map)
      (setf info-set (make-instance 'information-set
				    :key key))
      (setf (gethash key i-map) info-set)
      info-set)
    (gethash key i-map)))

(defclass information-set ()
  ((key :initarg :key :accessor key)
   (regret-sum
    :initarg :regret-sum
    :initform (loop for i below *N-ACTIONS* collect 0.0)
    :accessor regret-sum)
   (strategy-sum
    :initarg :strategy-sum
    :initform (loop for i below *N-ACTIONS* collect 0.0)
    :accessor strategy-sum)
   (strategy
    :initarg :strategy
    :initform (loop for i below *N-ACTIONS* collect (/ 1.0 *N-ACTIONS*))
    :accessor strategy)
   (reach-pr :initarg :reach-pr  :initform 0.0 :accessor reach-pr)
   (reach-pr-sum :initarg :reach-pr-sum
		 :initform 0.0 :accessor reach-pr-sum)))

(defmethod next-strategy ((info-set information-set))
  (setf (strategy-sum info-set)
	(mapcar #'(lambda (str-sum str)
		    (+ (* (reach-pr info-set) str)
		       str-sum))
		(strategy-sum info-set) (strategy info-set)))
  (setf (strategy info-set) (calc-strategy info-set))
  (setf (reach-pr-sum info-set)
	(+ (reach-pr info-set) (reach-pr-sum info-set)))
  (setf (reach-pr info-set) 0.0))

;;;Calculte current strategy from the sum of regret.
(defmethod calc-strategy ((info-set information-set))
  (let* ((strategy (make-positive (regret-sum info-set)))
	 (total (reduce '+ strategy)))
    (if (> total 0)
	(setf strategy (divide-by-const strategy total))
	(setf strategy (loop for i below *N-ACTIONS*
			  collect (/ 1.0 *N-ACTIONS*))))
    strategy))

(defun divide-by-const (lst const)
  (mapcar #'(lambda (elm) (/ elm const)) lst))

(defun make-positive (lst &optional (least 0))
  (mapcar #'(lambda (x) (if (> x least) x 0)) lst))

;;;Calculate average strategy over all iterations. 
;;;This is the Nash equilibrium strategy.
(defmethod get-average-strategy ((info-set information-set))
  (let* ((strategy
	  (divide-by-const (strategy-sum info-set) (reach-pr-sum info-set)))
	 ;; purify to remove actions that are likely a mistake
	 (strategy (make-positive strategy 0.001))
	 (total (reduce '+ strategy))
	 (strategy
	  (mapcar #'(lambda (v) (/ v total)) strategy)))
    strategy))

(defun display-key-strategy (key strategy)
  (format t "  ~a ~a~%" key strategy))

(defun display-results (exp-val i-map)
  (let ((sorted-items
	 (stable-sort
	  (loop for key being each hash-key of i-map
	       collect key)
	  #'string-lessp)))
    ;(format t "sorted-items: ~a~%" sorted-items)
    (format t "player 1 expected value: ~a~%" exp-val)
    (format t "player 2 expected value: ~a~%" (* -1 exp-val))
    (format t "~%")
    (format t "player 1 strategies:~%")
    (loop for key in sorted-items
       as info-set = (gethash key i-map)
       if (evenp (length key))
       do (display-key-strategy
	   key (get-average-strategy info-set)))
    (format t "~%")
    (format t "player 2 strategies:~%")
    (loop for key in sorted-items
       as info-set = (gethash key i-map)
       and ave-stra = nil
       if (oddp (length key))
       do (display-key-strategy
	   key (get-average-strategy info-set)))))
