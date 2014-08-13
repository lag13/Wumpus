;;;; My slightly altered version of hunt the wumpus.

;; Loads the wumpus game engine (which also loads the text game engine).
(load "wumpusGameEngine.lisp")

;; The classic two hazards in hunt the wumpus.
(defstruct (bats (:include hazard)))
(defstruct (pit (:include hazard)))

;; The function you call to play my slightly altered version of hunt
;; the wumpus.
(defun hunt-the-wumpus ()
  (format t "WELCOME TO HUNT THE WUMPUS~%")
  (init-game)
  (let ((continue-gamep T))
    (loop while continue-gamep
       do
	 (let ((*player* (copy-structure *player*))
	       (*hazards* (mapcar #'copy-structure *hazards*))
	       (*cave* (mapcar #'copy-list *cave*)))
	   (main-game-loop :continue-game-p (and (not (won-gamep)) (not (lost-gamep)))
			   :pre-comp ((print-location-info))
			   :handle-input ((handle-input 
					   (lambda () (format t "SHOOT OR MOVE (S-M)? ")))))
	   (if (won-gamep)
	       (format t "~&HE HE HE THE WUMPUS'LL GET YOU NEXT TIME")
	       (format t "~&HA HA HA YOU LOSE")))
	 (format t "~%~%")
	 (if (y-or-n-p "WOULD YOU LIKE TO PLAY AGAIN?")
	     (unless (y-or-n-p "WOULD YOU LIKE TO USE THE SAME BOARD?")
	       (init-game))
	     (setf continue-gamep nil)))))

(defun print-location-info () 
  "Prints information about your current status in the game."
  (format t "~&~%YOU ARE IN ROOM: ~a~%" (player-location *player*))
  (let ((adj-tunnels (adjacent-tunnels (player-location *player*))))
    (loop for h in *hazards* do
	 (when (member (hazard-location h) adj-tunnels)
	   (case (type-of h)
	     (wumpus (format t "YOU SMELL A WUMPUS~%"))
	     (bats (format t "YOU HEAR BATS~%"))
	     (pit (format t "YOU FEEL A BREEZE~%")))))
    (format t "~&TUNNELS LEAD TO: ~a" adj-tunnels)
    (format t "~&YOU HAVE ~a ARROWS REMAINING~%" (player-arrows *player*))))

(defun init-game ()
  "Initializes the game."
  (setf *cave* (dodec-cave))
  (let ((positions (n-rand-unique 6 (length *cave*))))
    (setf *player* (make-player :location (nth 0 positions) :arrows 3 :health 1))
    (setf *hazards* (list (make-wumpus :location (nth 1 positions) :health 1)
			  (make-bats :location (nth 2 positions))
			  (make-bats :location (nth 3 positions))
			  (make-pit :location (nth 4 positions))
			  (make-pit :location (nth 5 positions))))))

;; Adds a command that explains the other commands on the list.
(defcommand ? "Explains the possible commands a player can execute and what they do."
  command-help ()
  (format t "~%POSSIBLE COMMANDS:~%")
  (loop for c in *command-list* do 
       (format t "~a:~5t~a~%" (command-sym c) (command-description c))))

;; Adds a shooting command
(defcommand s "Shoots an arrow."
  player-shoot ()
  (let ((shoot-loc nil)
	(adj-tunnels (adjacent-tunnels (player-location *player*))))
    (loop until (and (integerp shoot-loc) (member shoot-loc adj-tunnels))
       do
	 (format t "~&SHOOT WHERE? ")
	 (setf shoot-loc (read)))
    (unless (find shoot-loc *hazards* :key (lambda (h) (hazard-location h)))
      (princ "MISSED"))
    (hazards-react #'hazard-shoot-reaction shoot-loc))
  (decf (player-arrows *player*)))

;; How the hazards react to a player shooting an arrow.
(def-hazard-reactions hazard-shoot-reaction (h shoot-loc)
  (wumpus (if (equal shoot-loc (hazard-location h))
	      (progn 
		(format t "YOU KILLED THE WUMPUS!~%")
		(setf (wumpus-health h) 0)
		(cons 'change-step (lambda (lst) nil)))
	      (setf (hazard-location h)
		    (random-list-elem (assoc (hazard-location h) *cave*)))))
  (bats (when (equal (hazard-location h) shoot-loc)
	  (format t "YOU HEAR BATS SCREECHING ANGRILY~%")))
  (pit (when (equal (hazard-location h) shoot-loc)
	 (format t "YOU HEAR YOUR ARROW GO WHISTLING DOWN A HOLE~%"))))

;; Adds a moving command.
(defcommand m "Handles player movement."
  player-move ()
  (let ((move-loc nil)
	(adj-tunnels (adjacent-tunnels (player-location *player*))))
    (loop until (and (integerp move-loc) (member move-loc adj-tunnels))
       do
	 (format t "~&WHERE TO? ")
	 (setf move-loc (read)))
    (setf (player-location *player*) move-loc)
    (hazards-react #'hazard-move-reaction)))

;; How the hazards react to a player moving.
(def-hazard-reactions hazard-move-reaction (h)
  (bats (when (equal (hazard-location h) (player-location *player*))
	  (princ "SUPER BATS CARRY YOU! ELSEWHERESVILLE FOR YOU!!!")
	  (setf (player-location *player*) (random (length *cave*)))
	  (hazards-react #'hazard-move-reaction)
	  (cons 'change-step (lambda (lst) nil))))
  (pit (when (equal (hazard-location h) (player-location *player*))
	 (princ "YYYIIIIIEEEEE... YOU FELL INTO A PIT AND DIED")
	 (setf (player-health *player*) 0)
	 (cons 'change-step (lambda (lst) nil))))
  (wumpus (let ((wumpus-loc (hazard-location h)))
	    (when (equal wumpus-loc (player-location *player*))
	      (princ "OOPS... YOU BUMPED A WUMPUS")
	      (setf (hazard-location h) (random-list-elem (assoc wumpus-loc *cave*)))
	      (when (equal (hazard-location h) (player-location *player*))
		(fresh-line)
		(princ "TSK TSK TSK - WUMPUS GOT YOU!")
		(setf (player-health *player*) 0)
		(cons 'change-step (lambda (lst) nil)))))))

