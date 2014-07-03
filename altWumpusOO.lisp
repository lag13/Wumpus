;;The (almost) classic game of Hunt the Wumpus.

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
	(fresh-line)
	(when (and (player-alive) (wumpus-alive))
		(print-location-info)
		(fresh-line)
		(princ "SHOOT OR MOVE? ")
		(case (read)
			(s (player-shoot))
			(m (player-move))
		)
		(main-game-loop)
	)
)

;; Helper function to return a random node in the cave 
(defun random-node (cave)
	(1+ (random (length cave)))
)

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
(defmethod hazard-reaction ((h wumpus))
	(princ "YOU KILLED THE WUMPUS!")
	(setf (wumpus-living (car *hazards*)) nil)
)

;; How the bats react to getting shot
(defmethod hazard-reaction ((h bats))
	(princ "YOU HEAR BATS SCREECHING ANGRILY")
)

;; How the bottomless pit reacts to getting shot
(defmethod hazard-reaction ((h pit))
	(princ "YOU HEAR YOUR ARROW GO WHISTLING DOWN A HOLE")
)

;; The action the wumpus takes when you shoot an arrow or bump into him.
(defmethod hazard-moveReaction ((h wumpus) cave)
	(let ((wumpus-loc (hazard-location (car *hazards*))))
		(setf wumpus-loc (random-list-elem (assoc wumpus-loc cave))) 

		(when (equal wumpus-loc (player-location *player*))
			(fresh-line)
			(princ "TSK TSK TSK - WUMPUS GOT YOU!")
			(setf (player-living *player*) nil)
		)
		(setf (hazard-location (car *hazards*)) wumpus-loc)
	)
)

;; The action a bat takes after you move
(defmethod hazard-moveReaction ((h bats) cave)
	(setf (player-location *player*) (random-node cave))
	(handle-new-location)
) 

;; The action the pit takes after you move
(defmethod hazard-moveReaction ((h pit) cave)
	(setf (player-living *player*) nil)
)

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
			(let ((miss-msg t) (haz (find shoot-loc *hazards* :key (lambda (h) (hazard-location h)))))
				(when haz
					(setf miss-msg nil)
					(hazard-reaction haz)
				)
				(when (wumpus-alive) 
					(hazard-moveReaction (car *hazards*) *caves*) 
					(decf (player-arrows *player*))
				)
				(when miss-msg (princ "MISSED"))
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
				(when (and (player-alive) (equal player-loc (hazard-location h)))
					(fresh-line)
					(case (type-of h)
						(wumpus (princ "OOPS... YOU BUMPED A WUMPUS"))
						(bats (princ "SUPER BATS CARRY YOU! ELSEWHERESVILLE FOR YOU!!!")) 
						(pit (princ "YYYIIIIIEEEEE... YOU FELL INTO A PIT AND DIED"))
					)
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
