;;; A text game engine
(defstruct command sym func num-params desc)

(defparameter *command-list* nil
  "List of possible commands that the player can execute.")

(defmacro add-command (command-sym command-desc command-func arg-list &body body)
  "Adds a command that a player could execute in a game."
  (push (make-command :sym command-sym
		      :func command-func
		      :num-params (length arg-list)
		      :desc command-desc)
	*command-list*)
  `(defun ,command-func ,arg-list ,@body))

;; All text games will have this command.
(add-command ? "Displays the list of possible commands and what they do." 
	     display-commands ()
	     (format t "~%")
	     (loop for c in *command-list*
		   do
		   (format t "~a - ~a~%" (command-sym c) (command-desc c))))

(defun main-game-loop (won-gamep lost-gamep print-prompt print-game-view &optional (extra-bookkeeping (lambda () )))
  "The main game loop for these types of games."
  (loop with command with args 
	while (and (not (funcall won-gamep)) (not (funcall lost-gamep)))
	do
	(funcall print-game-view)
       	(setf command nil)
	(loop until command
	      do
	      (funcall print-prompt)
	      (setf command (find (read) *command-list* :key (lambda (c) (command-sym c))))
	      (unless command (format t "Command not found.~%")))
	(setf args (loop repeat (command-num-params command) collect (read)))
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

(defun explode (delimiter str)
  "Breaks up a string into a list of tokens based on the delimiter character."
  (loop with token
	for i = 0 then (1+ j)
	for j = (position delimiter str :start i)
	when (> (length (setf token (subseq str i j))) 0)
	collect token
	while j))
