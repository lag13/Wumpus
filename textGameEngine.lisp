;;;; A text game engine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How this game engine is structured ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro main-game-loop (&key (continue-game-p T) check-before check-after 
			    pre-comp handle-input post-comp)
  `(tagbody
      (defun skip-pre-comp () (go skip-pre-comp))
      (defun skip-post-comp () (go start))
      (defun skip-pre-post-comp () (go before-handle-input))
      (defun end-loop () (go end))
    start
      (unless ,continue-game-p (go end))
      ,@pre-comp
    before-handle-input
      ,@(when check-before `((unless ,countinue-game-p (go end))))
      ,@handle-input
      ,@(when check-after `((unless ,continue-game-p (go end))))
      ,@post-comp
      (go start)
    skip-pre-comp
      ,@post-comp
      (go before-handle-input)
    end))

(defun n-rand-unique (n max) 
  "Returns a list of n random unique numbers in the range [0, max)."
  (loop with arr = (make-array max 
			       :initial-contents (loop for i from 0 below max collect i))
     with lowerBound = (- max n)
     for i from (1- max) downto lowerBound
     do (rotatef (aref arr i) (aref arr (random (1+ i))))
     collect (aref arr i)))

(defun random-list-elem (lst)
  "Returns a random element from a list."
  (nth (random (length lst)) lst))

(defun explode-seq (delimiter seq &optional remove-empty-subseqs-p)
  "Breaks up a sequence into a list of tokens based on the delimiter character."
  (loop for i = 0 then (1+ j)
     for j = (position delimiter seq :start i)
     collect (subseq seq i j) into tokens
     while j
     finally
       (return 
	 (if remove-empty-subseqs-p
	     (remove-if (lambda (x) (zerop (length x))) tokens)
	     tokens))))

