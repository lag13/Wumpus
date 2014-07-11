;; I also want the arrow to stop when it hits something...
;; Or maybe I don't stop the arrow when it hits something...
;; http://letoverlambda.com/textmode.cl/guest/chap2.html

;; List of possible commands
(defparameter *command-list* 
  '((m . (player-move . 0))
    (s . (player-shoot . 0))))
		  
;; Global variables representing the player and hazards
(defparameter *player* nil)
(defparameter *hazards* nil)

;;The original layout of the cave is a dodecahedron
(defparameter *cave* '((0 1 4 7)	
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

;;The player is defined by location, number of arrows, and 
;;if he/she is living.
(defstruct player location arrows health)

;; All hazards will be derived from the hazard class
;; All wumpus's will be derived from the wumpus class
(defstruct hazard location)
(defstruct (wumpus (:include hazard)) health)

;; The function you call to play my version of hunt the wumpus
(defun hunt-the-wumpus ()
  (format t "WELCOME TO HUNT THE WUMPUS~%")
  (let ((continue-gamep T) (player+hazards-repeat nil))
    (loop while continue-gamep
	  do
	  (setf player+hazards-repeat (init-game player+hazards-repeat))
	  (setf *player* (copy-structure (car player+hazards-repeat)))
	  (setf *hazards* (mapcar #'copy-structure (cdr player+hazards-repeat)))
	  (main-game-loop) 
	  (if (won-gamep)
	      (format t "~%HE HE HE THE WUMPUS'LL GET YOU NEXT TIME")
	    (format t "~%HA HA HA YOU LOSE"))
	  (format t "~%WOULD YOU LIKE TO PLAY AGAIN (Y-N)? ")
	  (if (eq 'n (read))
	      (setf continue-gamep nil)
	    (progn
	      (format t "~%WOULD YOU LIKE TO USE THE SAME BOARD (Y-N)? ")
	      (when (eq 'n (read))
		(setf player+hazards-repeat nil)))))))

;; The main loop for the entire game
(defun main-game-loop ()
  (loop with command with args 
	while (and (not (won-gamep)) (not (lost-gamep)))
	do
	(print-location-info)
	(setf command nil)
	(loop until command
	      do
	      (princ "SHOOT OR MOVE? ")
	      (setf command (find (read) *command-list* :key #'car)))
	(setf args (loop repeat (cddr command) collect (read)))
	(when (listen) (clear-input)) ; Clears any remaining input
	(apply (cadr command) args)))

(defun lost-gamep ()
  (or 
   (<= (player-health *player*) 0) 
   (<= (player-arrows *player*) 0)))

(defun won-gamep ()
  (loop for h in *hazards* 
	always (if (eq 'wumpus (type-of h)) (<= (wumpus-health h) 0) T)))

;; Prints information about your current status.
(defun print-location-info () 
  (format t "~&~%YOU ARE IN ROOM: ~a" (player-location *player*))
  (let ((adj-tunnels (adjacent-tunnels (player-location *player*))))
    (loop for h in *hazards* do
	  (when (member (hazard-location h) adj-tunnels)
	    (fresh-line)
	    (case (type-of h)
		  (wumpus (princ "YOU SMELL A WUMPUS"))
		  (bats (princ "YOU HEAR BATS"))
		  (pit (princ "YOU FEEL A BREEZE")))))
    (format t "~&TUNNELS LEAD TO: ~a" adj-tunnels)
    (format t "~&YOU HAVE ~a ARROWS REMAINING" (player-arrows *player*))))

;; Returns a list of n random unique integers in the range [0, max)
;; Note that n <= max for this to work
(defun n-rand-unique (n max) 
  (loop with arr = (make-array max 
			       :initial-contents (loop for i from 0 below max collect i))
	with lowerBound = (- max n)
	for i from (1- max) downto lowerBound
	do (rotatef (aref arr i) (aref arr (random (1+ i))))
	collect (aref arr i)))

;; Loops over the hazards and calls their "reaction" methods.
(defun hazards-react (hazard-react-method &rest args)
  (loop with step = #'cdr with lst = *hazards* with h with temp
	while (and lst (not (lost-gamep)))
	do
	(setf h (car lst))
	(setf lst (funcall step lst))
	(setf temp (apply hazard-react-method h args))
	(when (and (consp temp) (eq (car temp) 'change-step))
	  (setf step (cdr temp)))))

;; Utility function that returns the list of tunnels adjacent
;; to the player
(defun adjacent-tunnels (location) 
  (cdr (assoc location *cave*)))

;; Utility function that returns a random element in a list
(defun random-list-elem (lst)
  (nth (random (length lst)) lst))

(defun init-game (player+hazards)
  (if player+hazards
      player+hazards
    (progn
      (let ((positions (n-rand-unique 6 (length *cave*))))
	(list (make-player :location (nth 0 positions) :arrows 3 :health 1)
	      (make-wumpus :location (nth 1 positions) :health 1)
	      (make-bats :location (nth 2 positions))
	      (make-bats :location (nth 3 positions))
	      (make-pit :location (nth 4 positions))
	      (make-pit :location (nth 5 positions)))))))

;; The Hazards in the cave.
(defstruct (bats (:include hazard)))
(defstruct (pit (:include hazard)))

;; Handles player shooting
(defun player-shoot ()
  (let ((shoot-loc nil)
	(adj-tunnels (adjacent-tunnels (player-location *player*))))
    (loop until (and (integerp shoot-loc) (member shoot-loc adj-tunnels))
	  do
	  (format t "~&SHOOT WHERE? ")
	  (setf shoot-loc (read)))
    (hazards-react #'hazard-shoot-reaction shoot-loc)
    (unless (find shoot-loc *hazards* :key (lambda (h) (hazard-location h)))
      (princ "MISSED")))
  (decf (player-arrows *player*)))

;; The action the wumpus takes when you shoot
(defmethod hazard-shoot-reaction ((h wumpus) shoot-loc)
  (if (equal shoot-loc (hazard-location h))
      (progn 
	(princ "YOU KILLED THE WUMPUS!")
	(setf (wumpus-health h) 0))
    (setf (hazard-location h) 
	    (random-list-elem (assoc (hazard-location h) *cave*)))))

;; The action the bats takes when you shoot
(defmethod hazard-shoot-reaction ((h bats) shoot-loc)
  (when (equal (hazard-location h) shoot-loc)
    (princ "YOU HEAR BATS SCREECHING ANGRILY")))

;; The action the pit takes when you shoot
(defmethod hazard-shoot-reaction ((h pit) shoot-loc)
  (when (equal (hazard-location h) shoot-loc)
    (princ "YOU HEAR YOUR ARROW GO WHISTLING DOWN A HOLE")))

;; Handles the player moving around
(defun player-move ()
  (let ((move-loc nil)
	(adj-tunnels (adjacent-tunnels (player-location *player*))))
    (loop until (and (integerp move-loc) (member move-loc adj-tunnels))
	  do
	  (format t "~&WHERE TO? ")
	  (setf move-loc (read)))
    (setf (player-location *player*) move-loc)
    (hazards-react #'hazard-move-reaction)))

;; The action the wumpus takes when you move
(defmethod hazard-move-reaction ((h wumpus))
  (let ((wumpus-loc (hazard-location h)))
    (when (equal wumpus-loc (player-location *player*))
      (princ "OOPS... YOU BUMPED A WUMPUS")
      (setf (hazard-location h) (random-list-elem (assoc wumpus-loc *cave*)))
      (when (equal (hazard-location h) (player-location *player*))
	(fresh-line)
	(princ "TSK TSK TSK - WUMPUS GOT YOU!")
	(setf (player-health *player*) 0)))))

;; The action a bat takes when you move
(defmethod hazard-move-reaction ((h bats))
  (when (equal (hazard-location h) (player-location *player*))
    (princ "SUPER BATS CARRY YOU! ELSEWHERESVILLE FOR YOU!!!")
    (setf (player-location *player*) (random (length *cave*)))
    (hazards-react #'hazard-move-reaction)
    (cons 'change-step (lambda (lst) nil))))

;; The action the pit takes when you move
(defmethod hazard-move-reaction ((h pit))
  (when (equal (hazard-location h) (player-location *player*))
    (princ "YYYIIIIIEEEEE... YOU FELL INTO A PIT AND DIED")
    (setf (player-health *player*) 0)))
