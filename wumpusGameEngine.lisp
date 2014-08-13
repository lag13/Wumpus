;;;; A game engine for "Hunt the Wumpus" type games.

;; The main two things that this text game engine does is define an
;; environment in which the "core" of a game's code could be run and
;; defines a way for coders to add new commands to the game. Besides
;; that it just defines a couple of general utility functions.

;; In my case, this "environment" takes the form of a plain old
;; function which I've called "main-game-loop". This function's
;; parameters are all parameterless functions that will be called by
;; the game loop and are, naturally, specific to whatever game is
;; being created. What this function essentially does is read player
;; input, match it with a command, and execute the function associated
;; with that command.

;; A command consists of a symbol (which is what the user enters to
;; execute the command), a function corresponding to the symbol (which
;; ACTUALLY carries out the command), the number of parameters the
;; function requires (so the main-game-loop can read those in), and a
;; string describing what the command does. The only reason I have
;; that "description" slot is because I liked the idea of being able
;; to display to a player a list of possible commands and what they
;; do.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How this game engine is structured ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When thinking about making a wumpus game engine, this was the main
;; thought that inspired my decisions: The basic game of hunt the
;; wumpus is turn based. The player takes an action and then the
;; hazards could respond depending on what action was taken.

;; Right off the bat I liked the idea of all hazards being represented
;; as objects. You could keep them in a list and if you needed them to
;; react to a player's action then you could just loop over the list
;; and call the appropriate "reaction" method. This would make
;; extending hazard functionality easy because you could just create
;; new "reaction" methods. To satisfy my OO based idea, all hazards
;; are derived classes of a base class "hazard". Since defining
;; "hazard reaction methods" would probably become a common occurrence
;; I created a macro to help make it a little simpler. In addition to
;; helping define the "reaction" methods, this macro does a couple
;; other things which I think are quite powerful (more on that
;; below). Since looping over the hazards would get done a lot I also
;; abstracted that code into a function.

;; To be honest, setting up this framework for how hazards work is
;; probably the most imporant part of this file. The other things it
;; does are rather minor but I'll list them nonetheless.

;; I define a "player" class.
;; I define a couple subroutines related to the cave (i.e. graph)
;; that the player moves around in.
;; I define some default predicate functions that give winning and
;; losing conditions. 

(load "textGameEngine.lisp")

(defparameter *command-list* nil
  "List of possible commands that the player can execute.")

(defstruct command sym func num-params description)

;; We must know how many parameters a user created command/function
;; accepts so that the main loop can read in the appropriate number of
;; arguments. To prevent the user from having to manually enter the
;; number I created this macro which will define a function and add it
;; to the command list.
(defmacro defcommand (command-sym command-desc command-func arg-list &body body)
  "Adds a command that a player could execute in a game."
  `(progn 
     (push (make-command :sym ',command-sym
			 :func ',command-func
			 :num-params ,(length arg-list)
			 :description ,command-desc)
	   *command-list*)
     (defun ,command-func ,arg-list ,@body)))

(defun handle-input (print-prompt)
  "How input is handled for these types of games."
  (loop with command with args 
     until command
     do
       (funcall print-prompt)
       (if (setf command (find (read) *command-list* :key #'command-sym))
	   (progn	   
	     (setf args (loop repeat (command-num-params command) 
			   collect (if (listen)
				       (read)
				       (progn
					 (format t "> ")
					 (read)))))
	     (when (listen) (clear-input)) 
	     (apply (command-func command) args))
	   (format t "COMMAND NOT FOUND~%"))))

(defvar *player* nil
  "The player character.")

(defvar *hazards* nil
  "A list of hazards.")

(defvar *hazard-reaction-order* nil
  "For each player action, some hazards may have a reaction. You can specify in what order the hazards will react in. That information is stored in this variable.")

(defvar *cave* nil
  "The cave of tunnels.")

;; The player is defined by location, number of arrows, and 
;; amount of health.
(defstruct player location arrows health)

;; All hazards will be derived from the hazard class
;; All wumpus's will be derived from the wumpus class
(defstruct hazard location)
(defstruct (wumpus (:include hazard)) health)

;; One thing I noticed with these types of games, at least the way I'm
;; choosing to implement them, is that whenever you define "hazard
;; reaction methods" some patterns come up. ONE is that the first
;; parameter will ALWAYS signify the type of the hazard that this
;; method will apply to. It must be the first parameter because the
;; "hazards-react" function expects it that way. TWO is that all the
;; functions will have the same parameters following the first
;; one. THREE is that all the "hazard reaction methods" have the same
;; names. Now these three things are rather minor and aren't that hard
;; to remember. However! This is a syntactic pattern that might be an
;; annoyance to remember/write when you're coding.

;; Just as functions can be viewed as abstractions for computation,
;; lisp macros can be viewed as an abstraction for syntax. So I can
;; create a macro that helps abstract away that common pattern
;; described above. Maybe it's not strictly necessary but it might
;; make the code clearer and easier to maintain. And I need
;; practice. And it's just fun to do stuff like this!

;; It turns out there were two more uses to writing this
;; macro. Another challenge I was having with my setup of hazards
;; "reacting" after you take action is that maybe, depending on the
;; action, some hazards should react before others. So for each action
;; that inspires a reaction there should be an order to how we loop
;; over the hazards. Now if this macro didn't exist the coder would
;; have separately define all the reaction methods and then have to
;; remember to do something (like modify some global state) that would
;; indicate the order that they WANT the hazards to react in. That
;; seems a bit tedious. Instead, we can embded that extra step of
;; defining an order right into the macro! In particular, the order
;; that the hazards are listed in will be the order that they will
;; react. That (I think) is very concise and powerful. And one more
;; minor thing I added onto this macro is to define a method with the
;; same signature as the others but applying to "hazard" types. This
;; defined method does nothing. So now if the coder thinks a particular
;; hazard shouldn't react to a given action he can just not write
;; it. 
;; An exmple call:
;; (def-hazard-reactions shoot-reaction (h shoot-loc)
;;    (pit (when (= shoot-loc (hazard-location h))
;;           (princ "WOO")))
;;    (bats (princ "I'm a bat") 
;;          (princ "HERE I AM"))
;;    (wumpus (princ "I'm a scary wumpus")
;;       (if (= shoot-loc (hazard-location h))
;;           (princ "NOOOOO")
;;         (princ "FEAR ME"))))
(defmacro def-hazard-reactions (func-name (&rest args) &body body)
  "Defines hazard reaction methods for any and all hazards you specify.
The first parameter will hold the \"value\" of the hazard."
  `(progn
     (defmethod ,func-name ((,(car args) hazard) ,@(cdr args)))
     ,@(loop for i in body collect
	    `(defmethod ,func-name ((,(car args) ,(car i)) ,@(cdr args)) 
	       ,@(cdr i)))
     (push (cons #',func-name 
		 ',(loop for i from 0
		      for b in body 
		      collect (cons (car b) i)))
		 *hazard-reaction-order*)))

;; This game is turn based, you take an action and then the hazards
;; react.  So my idea was that the hazards will all have "reaction"
;; methods to your actions. Then, after completing your action, you
;; could just loop over the hazards and call their reaction
;; methods. This function does this looping. One initial challenge
;; with this approach was dealing with the super bats which carry you
;; to a new location. Since you are no longer in the location which
;; prompted the looping in the first place then we shouldn't keep
;; looping over the hazards. To get around this I let those reaction
;; methods change the looping behavior of this function. So in the
;; case of the bats, they could essentially tell the loop to stop. To
;; change how the loop steps the reaction methods can return a cons
;; pair containing the symbol CHANGE-STEP and a function representing
;; the new step. This function will also sort the list of hazards
;; before looping over them.
(defun hazards-react (hazard-react-method &rest args)
  "Loops over the list of hazards and calls their reaction methods."
  (loop with step = #'cdr 
     with lst = (setf *hazards* (sort *hazards*
				      (let ((ordering (cdr (assoc hazard-react-method
								  *hazard-reaction-order*))))
					(lambda (x y)
					  (let ((x (assoc (type-of x) ordering))
						(y (assoc (type-of y) ordering)))
					    (cond
					      ((null x) nil)
					      ((null y) T)
					      (T (< (cdr x) (cdr y)))))))))
     with h with return-val
     while (setf h (car lst))
     do
       (fresh-line)
       (setf return-val (apply hazard-react-method h args))
       (when (and (consp return-val) 
		  (eq (car return-val) 'change-step))
	 (setf step (cdr return-val)))
       (setf lst (funcall step lst))))

;; In my setup, caves are a list of lists. The car of each inner list
;; is the node and the cdr is the nodes adjacent to that node.
(defun adjacent-tunnels (location) 
  "Returns the list of tunnels adjacent to location."
  (cdr (assoc location *cave*)))

(defun within-one (loc1 loc2)
  "Returns true if loc2 is within one move away from loc1."
  (member loc2 (adjacent-tunnels loc1)))

(defun within-two (loc1 loc2)
  "Returns true if loc2 is within two moves away from loc1."
  (or (within-one loc1 loc2)
      (some (lambda (u)
	      (within-one u loc2))
	    (adjacent-tunnels loc1))))

(defun shortest-path (source dest)
  "Returns a list which is the shortest path from source to dest. The weight of each edge is just 1."
  (do* ((min-queue (let ((inf (length *cave*)))
		     (mapcar (lambda (x)
			       (list :vert x :pred nil :shortest-est inf))
			     (remove source (mapcar #'car *cave*))))
		   (sort min-queue #'< :key (lambda (x) (getf x :shortest-est))))
	(verts (list (list :vert source :pred nil :shortest-est 0))
	       (cons (pop min-queue) verts)))
       ((null min-queue) (do ((u (find dest verts :key (lambda (x) (getf x :vert)))
				 (find (getf u :pred) verts :key (lambda (x) (getf x :vert))))
			      (path nil (cons (getf u :vert) path)))
			     ((null u) path)))
    (let* ((u (car verts))
	   (new-dist (1+ (getf u :shortest-est))))
      (mapc (lambda (v)
	      (let ((v (find v min-queue :key (lambda (x) (getf x :vert)))))
		(when (< new-dist (getf v :shortest-est))
		  (setf (getf v :shortest-est) new-dist)
		  (setf (getf v :pred) (getf u :vert)))))
	    (set-difference (adjacent-tunnels (getf u :vert)) 
			    (mapcar (lambda (x) (getf x :vert)) verts))))))

(defun dodec-cave ()
  "Returns a dodecahedron cave like in the original game of hunt the wumpus"
  '((0 1 4 7)	
    (1 0 2 9)
    (2 1 3 11)
    (3 2 4 13)
    (4 0 3 5)
    (5 4 6 14)
    (6 5 7 16)
    (7 0 6 8)
    (8 7 9 17)
    (9 1 8 10)
    (10 9 11 18)
    (11 2 10 12)
    (12 11 13 19)
    (13 3 12 14)
    (14 5 13 15)
    (15 14 16 19)
    (16 6 15 17)
    (17 8 16 18)
    (18 10 17 19)
    (19 12 15 18)))

(defun regular-cave (width height &optional toroidal)
  "Returns a regular cave."
  (let ((num-rooms (* width height)) (bottom-left-corner (- (* width height) width)))
    (flet ((adj-rooms (room) ;; Returns a list of adjacent rooms for a particular room
	     (let ((result nil))
	       (cond
		 ((zerop room)  ;; Top left corner
		  (push 1 result)
		  (push (+ room width) result)
		  (when toroidal
		    (push (1- width) result)
		    (push bottom-left-corner result)))
		 ((= room (1- width)) ;; Top right corner
		  (push (+ room width) result)
		  (push (1- room) result)
		  (when toroidal
		    (push 0 result)
		    (push (1- num-rooms) result)))
		 ((= room bottom-left-corner) ;; Bottom left corner
		  (push (1+ room) result)
		  (push (- room width) result)
		  (when toroidal
		    (push 0 result)
		    (push (1- num-rooms) result)))
		 ((= room (1- num-rooms)) ;; Bottom right corner
		  (push (1- room) result)
		  (push (- room width) result)
		  (when toroidal
		    (push (1- width) result)
		    (push bottom-left-corner result)))
		 ((< room width) ;; Top edge
		  (push (1+ room) result)
		  (push (+ room width) result)
		  (push (1- room) result)
		  (when toroidal
		    (push (+ bottom-left-corner room) result)))
		 ((zerop (mod room width)) ;; Left edge
		  (push (1+ room) result)
		  (push (+ room width) result)
		  (push (- room width) result)
		  (when toroidal
		    (push (1- (+ room width)) result)))
		 ((zerop (mod (1+ room) width)) ;; Right edge
		  (push (+ room width) result)
		  (push (1- room) result)
		  (push (- room width) result)
		  (when toroidal
		    (push (1+ (- room width)) result)))
		 ((>= room bottom-left-corner) ;; Bottome edge
		  (push (1+ room) result)
		  (push (1- room) result)
		  (push (- room width) result)
		  (when toroidal
		    (push (- room bottom-left-corner) result)))
		 (T ;; Everything else
		  (push (1+ room) result)
		  (push (+ room width) result)
		  (push (1- room) result)
		  (push (- room width) result)))
	       result)))
      (loop for room from 0 below num-rooms
	 collect (cons room (adj-rooms room))))))

;; If the user wants different winning or losing conditions they could
;; make such functions and use those instead.
(defun lost-gamep ()
  "Default way to lose a game."
  (or 
   (<= (player-health *player*) 0) 
   (<= (player-arrows *player*) 0)))

(defun won-gamep ()
  "Default way to win a game is to kill all wumpus's."
  (loop for h in *hazards* 
     always (if (subtypep (type-of h) 'wumpus)
		(<= (wumpus-health h) 0)
		T)))
