;;The (almost) classic game of Hunt the Wumpus.
;; TODO: Convert some of the looping functions to use the loop
;; macro instead of recursion. Play with the shoot handling
;; function. I want to have each of the hazards "react" but
;; I also want the arrow to stop when it hits something...
;; Or maybe I don't stop the arrow when it hits something...
;; http://letoverlambda.com/textmode.cl/guest/chap2.html

;; Global variables representing the player and hazards
(defparameter *player* nil)
(defparameter *hazards* nil)

;;The original layout of the cave is a dodecahedron
(defparameter *caves* '((1 2 5 8)	
			(2 1 3 10)
			(3 2 4 12)
			(4 3 5 14)
			(5 1 4 6)
			(6 5 7 15)
			(7 6 8 17)
			(8 1 7 9)
			(9 8 10 18)
			(10 2 9 11)
			(11 10 12 19)
			(12 3 11 13)
			(13 12 14 20)
			(14 4 13 15)
			(15 6 14 16)
			(16 15 17 20)
			(17 7 16 18)
			(18 9 17 19)
			(19 11 18 20)
			(20 13 16 19)
			)
)

;; The function you call to play my version of hunt the wumpus
(defun hunt-the-wumpus ()
	(format t "WELCOME TO HUNT THE WUMPUS~%")
	(let ((player nil) (hazards nil) (repeat-game nil))
		(labels ((game-loop () 
				(setf player (init-player player))
				(setf hazards (init-hazards hazards))
				(main-game-loop)

				(if (wumpus-alive)
					(format t "~%HA HA HA YOU LOSE")
					(format t "~%HE HE HE THE WUMPUS'LL GET YOU NEXT TIME")
				)
				(format t "~%WOULD YOU LIKE TO PLAY AGAIN (Y-N)? ")
				(if (eq 'n (read))
					'thanks-for-playing!
					(progn
						(format t "~%WOULD YOU LIKE TO USE THE SAME BOARD (Y-N)? ")
						(when (eq 'n (read))
							(setf player nil)
							(setf hazards nil)
						)
						(game-loop)
					)
				)
			))
			(game-loop)
		)
	)
)

;; The main loop for the entire game
(defun main-game-loop ()
	(loop while (and (player-alive) (wumpus-alive))
		do
			(print-location-info)
			(fresh-line)
			(handle-player-input)))

#|
			(princ "SHOOT OR MOVE? ")
			(case (read)
				(s (player-shoot))
				(m (player-move)))))
|#

;; List of cons pairs containing commands and the functions that
;; those commands execute??? Sounds kinda cool to me.
(defun handle-player-input ()
	(loop do
		(princ "SHOOT OR MOVE? ")
		(case (read)
			(s (player-shoot) (return))
			(m (player-move) (return)))))

;; Helper function to return a random node in the cave 
(defun random-node (cave)
	(1+ (random (length cave)))
)

;; Returns a list of n random unique integers in the range [0, max)
;; Note that n <= max for this to work
(defun nRandUnique (n max) 
	(loop 
		with lst = (loop for i from 0 below max collect i)
		with lowerBound = (- max n)
		for i from (1- max) downto lowerBound
		do
			(rotatef (nth i lst) (nth (random (1+ i)) lst))
		collect
			(nth i lst)))

;; Something I made for fun.
;; When you call it with a number it will create an array of
;; values [0, m) and return and random number in that range. 
;; Then when you call the function without any
;; parameters it will return random unique numbers in the range
;; [0, m) of course repeating when you call it more than m times.
(let ((max 0) (arr nil))
	(defun randUnique (&optional m)
		(when (equal max -1) (setf max (1- (array-total-size arr))))
		(when m
			(setf arr (make-array m :initial-contents 
				(loop for i from 0 below m collect i)))
			(setf max (1- m)))
		(rotatef (aref arr max) (aref arr (random (1+ max))))
		(aref arr (1+ (decf max)))))

;; Playing around with lisp's OO programming. 
;; In particular all the hazards in the game (wumpus, pit, bats)
;; are derived classes of the base class "hazard". 
(defstruct hazard
	location
)

(defstruct (wumpus (:include hazard)) living)

(defstruct (bats (:include hazard)))

(defstruct (pit (:include hazard)))

;; How the wumpus reacts to getting shot
(defmethod hazard-shootReaction ((h wumpus) shoot-loc)
	(cond
		((equal shoot-loc (hazard-location h))
			(princ "YOU KILLED THE WUMPUS!")
			(setf (wumpus-living (car *hazards*)) nil))
		(T (setf (hazard-location h) 
			(random-list-elem (assoc (hazard-location h) *caves*))))))

;; How the bats react to getting shot
(defmethod hazard-shootReaction ((h bats) shoot-loc)
	(when (equal (hazard-location h) shoot-loc)
		(princ "YOU HEAR BATS SCREECHING ANGRILY")))

;; How the bottomless pit reacts to getting shot
(defmethod hazard-shootReaction ((h pit) shoot-loc)
	(when (equal (hazard-location h) shoot-loc)
		(princ "YOU HEAR YOUR ARROW GO WHISTLING DOWN A HOLE")))

;; The action the wumpus takes when you shoot an arrow or bump into him.
(defmethod hazard-moveReaction ((h wumpus) cave)
	(let ((wumpus-loc (hazard-location h)))
		(when (equal wumpus-loc (player-location *player*))
			(princ "OOPS... YOU BUMPED A WUMPUS")
			(setf (hazard-location h) (random-list-elem (assoc wumpus-loc cave)))
			(when (equal (hazard-location h) (player-location *player*))
				(fresh-line)
				(princ "TSK TSK TSK - WUMPUS GOT YOU!")
				(setf (player-living *player*) nil)))))

;; The action a bat takes after you move
(defmethod hazard-moveReaction ((h bats) cave)
	(when (equal (hazard-location h) (player-location *player*))
		(princ "SUPER BATS CARRY YOU! ELSEWHERESVILLE FOR YOU!!!") 
		(setf (player-location *player*) (random-node cave))
		(handle-new-location)))

;; The action the pit takes after you move
(defmethod hazard-moveReaction ((h pit) cave)
	(when (equal (hazard-location h) (player-location *player*))
		(princ "YYYIIIIIEEEEE... YOU FELL INTO A PIT AND DIED")
		(setf (player-living *player*) nil)))

;; Creates the hazards and their locations
(defun init-hazards (hazards)
	(if hazards
		(setf *hazards* hazards)
		(let ((taken-spots (list (player-location *player*))))
			(labels ((find-empty-loc ()
					(let ((loc (random-node *caves*)))
						(if (member loc taken-spots)
							(find-empty-loc) 
							(progn
								(push loc taken-spots)
								loc
							)
						)
					)
				))
				(setf *hazards* 
					(list 
						(make-wumpus :location (find-empty-loc) :living T)
						(make-bats :location (find-empty-loc))
						(make-bats :location (find-empty-loc))
						(make-pit :location (find-empty-loc))
						(make-pit :location (find-empty-loc))
					)
				)
			)
		)
	)
	(mapcar #'copy-structure *hazards*)
)

;; Returns true/false if the wumpus is/is not alive.
(defun wumpus-alive ()
	(wumpus-living (car *hazards*))
)

;;The player is defined by location, number of arrows, and 
;;if he/she is living.
(defstruct player
	location
	arrows
	living
)

;; Utility function that returns the list of tunnels adjacent
;; to the player
(defun adjacent-tunnels (location cave)
	(cdr (assoc location  cave)))

;; Utility function that returns a random element in a list
(defun random-list-elem (lst)
	(nth (random (length lst)) lst))

;; Returns true/false if the player is/is not alive.
(defun player-alive ()
	(and 
		(player-living *player*) 
		(> (player-arrows *player*) 0)
	)
)

;; Initialzes the player
(defun init-player (player)
	(if player
		(setf *player* player)
		(setf *player* (make-player :location (random-node *caves*) :arrows 3 :living T))
	)
	(copy-structure *player*)
)

;; Handles player shooting
(defun player-shoot ()
	(format t "~&SHOOT WHERE? ")
	(let ((shoot-loc (read)) (adj-tunnels (adjacent-tunnels (player-location *player*) *caves*))) 
		(if 
			(and 
				(integerp shoot-loc) 
				(member shoot-loc adj-tunnels)
			)
			(let ((haz (find shoot-loc *hazards* :key (lambda (h) (hazard-location h)))))
				(decf (player-arrows *player*))
				(if haz
					(hazard-shootReaction haz shoot-loc)
					(princ "MISSED"))
			)
			(player-shoot)
		)
	)
)

;; Handles the player moving around
(defun player-move ()
	(fresh-line)
	(princ "WHERE TO? ")
	(let ((new-loc (read)))
		(if 
			(and 
				(integerp new-loc) 
				(member new-loc (adjacent-tunnels (player-location *player*) *caves*)) 
			)	
			(progn
				(setf (player-location *player*) new-loc)
				(handle-new-location)
			)
			(player-move)
		)
	)
)

;; Get's called when a player moves to a new spot.
(defun handle-new-location ()
	(let ((player-loc (player-location *player*)))
		(mapc
			(lambda (h)
				(when (player-alive)
					(fresh-line)
					(hazard-moveReaction h *caves*)
				)
			)
			*hazards*
		)
	)
)

;; Prints information about your current status.
(defun print-location-info () 
	(format t "~&~%YOU ARE IN ROOM: ~a" (player-location *player*))
	(let ((adj-tunnels (adjacent-tunnels (player-location *player*) *caves*)))
		(mapc
			(lambda (h)
				(fresh-line)
				(when (member (hazard-location h) adj-tunnels)
					(case (type-of h)
						(wumpus (princ "YOU SMELL A WUMPUS"))
						(bats (princ "YOU HEAR BATS"))
						(pit (princ "YOU FEEL A BREEZE"))
					)
				)
			)
			*hazards*
		)
		(format t "~&TUNNELS LEAD TO: ~a" adj-tunnels)
		(format t "~&YOU HAVE ~a ARROWS REMAINING" (player-arrows *player*))
	)
)
