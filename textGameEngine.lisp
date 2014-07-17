;;; A text game engine
(defstruct command sym func num-params description)

(defparameter *command-list* nil
  "List of possible commands that the player can execute.")

;; We must know how many parameters a user created command/function
;; accepts so that the main loop can read in the appropriate number
;; of arguments. To prevent the user from having to manually enter 
;; the number I created this macro which defines a function and
;; adds it to the command list.
;; I'm not entirely sure if it is a good use of a macro. Is it a good
;; idea to define a function inside of a macro? Is it a good idea to
;; change global state inside of a macro? Does the function still get 
;; defined if this is called within another function?
(defmacro add-command (command-sym command-desc command-func arg-list &body body)
  "Adds a command that a player could execute in a game."
  (push (make-command :sym command-sym
		      :func command-func
		      :num-params (length arg-list)
		      :description command-desc)
	*command-list*)
  `(defun ,command-func ,arg-list ,@body))

(defun main-game-loop (game-continuesp print-prompt print-game-view &optional (extra-bookkeeping (lambda () )))
  "The main game loop for these types of games."
  (loop with command with args 
	while (funcall game-continuesp)
	do
	(funcall print-game-view)
       	(setf command nil)
	(loop until command
	      do
	      (funcall print-prompt)
	      (setf command (find (read) *command-list* :key (lambda (c) (command-sym c))))
	      (unless command (format t "Command not found.~%")))
	(setf args (loop repeat (command-num-params command) 
			 collect (if (listen)
				     (read)
				   (progn
				     (format t "> ")
				     (read)))))
	; Clears any remaining input before calling the function.
	(when (listen) (clear-input)) 
	(apply (command-func command) args)
	(funcall extra-bookkeeping)))

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

