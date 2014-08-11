;;;; A hunt the wumpus type game inspired by the movie "Edge of
;;;; Tomorrow"

;; Loads the wumpus game engine (which also loads the text game engine).
(load "wumpusGameEngine.lisp")

(defparameter *quit-gamep* nil
  "Helps allow the player to quit the game.")
(defparameter *time-travelp* nil
  "True or false if the player can time travel")
(defvar *alien-time-travelingp* nil) 
(defparameter *charge-frequency* 5
  "A player can charge once every 5 turns.")
(defparameter *low-blood-limit* 5
  "If a player spends 5 turns with 1 health then they lose the ability to time travel")
(defvar *omega-loc* nil
  "It's just convenient to have the aliens know the location of the omega.")

;; The character in this game.
(defstruct tomc health location low-blood-count charge-limit ammo bombs)

;; The three types of aliens in the movie
(defstruct (mimic (:include wumpus)))
(defstruct (alpha (:include wumpus)))
(defstruct (omega (:include wumpus)))

;; The function you call to play this game.
(defun edge-of-wumpus ()
  (format t "WELCOME TO EDGE OF WUMPUS~%")
  (when (y-or-n-p "WOULD YOU LIKE TO LEARN ABOUT THE GAME?")
    (format t "THIS GAME IS INSPIRED BY THE MOVIE \"EDGE OF TOMORROW\". FOR THOSE WHO HAVEN'T SEEN IT, THE MOVIE IS ABOUT AN ALIEN INVASION. THE ALIENS ARE CALLED \"MIMICS\" AND THERE ARE THREE DIFFERENT TYPES OF THEM. ONE TYPE IS YOUR AVERAGE JOE SORT OF MIMIC THAT MAKES UP THE MAJORITY OF THE MIMIC POPULATION. THE OTHER TWO TYPES, NICKNAMED THE \"ALPHA\" AND THE \"OMEGA\", ARE MUCH MORE IMPORTANT. THE ALPHA IS A BEEFED UP VERSION OF A REGULAR MIMIC AND IS EXTREMELY RARE. THE OMEGA IS SORT OF THE QUEEN BEE OF THE MIMICS. THE ALPHA AND THE OMEGA HAVE AN IMPORTANT RELATIONSHIP. WHEN THE ALPHA DIES, THE OMEGA IS ABLE TO RESET TIME THUS GIVING THE MIMICS AN ADVANTAGE IN BATTLE. THAT TIME TRAVELING POWER COMES FROM THE ALPHA'S BLOOD. IF YOU KILL THE OMEGA, ALL THE OTHER MIMICS DIE. IN THE MOVIE, TOM CRUISE KILLS THE ALPHA WITH A BOMB AND KILLS HIMSELF IN THE PROCESS. HOWEVER, BEFORE HE COMPLETELY DIES HE ABSORBS SOME OF THE ALPHA'S BLOOD AND THUS WHENEVER HE DIES TIME WILL BE RESET.~%~%")
    (format t "GAMEPLAY:~%IN THIS GAME YOUR CHARACTER MOVES AROUND A 6 BY 6 GRID FILLED WITH 7 MIMICS, 1 ALPHA, and 1 OMEGA. YOU HAVE VARIOUS WEAPONS AT YOUR DISPOSAL AND THE ULTIMATE GOAL IS TO KILL THE OMEGA.~%~%")
    (format t "ALIENS:~%MIMIC - A BASIC ALIEN WHICH CAN HIT YOU FOR 1 DAMAGE AND HAS 1 HEALTH. YOU WILL SEE \"YOU HEAR A NEARBY MIMIC\" WHEN THERE IS ONE ADJACENT TO YOU.~%ALPHA - HAS 3 HEALTH AND HITS YOU FOR 2 DAMAGE. IF YOU KILL THE ALPHA, THEN THE GAME WILL START OVER WITH A DIFFEREN'T CONFIGURATION. YOU WILL SEE \"YOU SENSE A POWERFUL PREDATOR\" WHEN THERE IS ONE ADJACENT TO YOU.~%OMEGA - DOESN'T ATTACK OR MOVE AND HAS 5 HEALTH. IF YOU KILL IT, YOU WIN THE GAME. YOU WILL SEE \"YOU SEE A FAINT GLOW\" WHEN THERE IS ONE ADJACENT TO YOU.~%~%")
    (format t "YOUR CHARACTER:~%YOUR CHARACTER STARTS OFF THE GAME WITH 4 HEALTH, 4 SHOTS, AND 1 BOMB. THERE ARE A COUPLE DIFFERENT METHODS OF ATTACK WHICH ALL DO 1 DAMAGE (EXCEPT FOR THE BOMB WHICH DOES 3). MOST COMMANDS WILL TAKE A NUMBER REPRESENTING WHERE YOU WANT THAT COMMAND TO TAKE PLACE. FOR EXAMPLE IF YOU WANTED TO MOVE TO LOCATION 2, YOU WOULD TYPE: M 2. OR IF YOU WANTED TO THROW YOUR BOMB IN LOCATION 6 YOU'D TYPE: B 6 (NOTE THAT IT DOESN'T MATTER IF THE LETTERS ARE CAPITAL OR NOT). TO SEE A LIST OF ALL THE COMMANDS AND A BRIEF DESCRIPTION OF WHAT THEY DO JUST TYPE: ?.~%~%")
    (format t "TIME TRAVEL:~%THE WAY I DID TIME TRAVEL IS THE FOLLOWING: YOU ARE CAPABLE OF THROWING A BOMB IN THE LOCATION YOU ARE STANDING IN. IF YOU DO THIS (AND AN ALPHA IS ALSO IN YOUR LOCATION) THEN YOU WILL DIE AND GAIN THE ABILITY TO TIME TRAVEL. SO FROM NOW ON, IF YOU DIE, YOU WILL START THE SAME GAME OVER. AND WHEN I SAY \"THE SAME GAME\" I REALLY MEAN IT, THE ALIENS WILL MOVE IN EXACTLY THE SAME WAY. THIS IS HOW YOU CAN BEAT THEM, YOU LEARN HOW THEY MOVE AND TAKE ADVANTAGE OF IT. IF YOU KILL AN ALPHA THEN THE OMEGA WILL RESET TIME. THE ONLY DIFFERENCE BETWEEN THE OMEGA RESETTING TIME AND TIME BEING RESET WHEN YOU DIE IS THAT WHEN THE OMEGA RESETS TIME THE STARTING CONFIGURATION OF THE GAME WILL BE DIFFERENT. THERE IS ONE MORE STIPULATION TO YOU BEING ABLE TO TIME TRAVEL. IN THE MOVIE, IF YOU LOST TOO MUCH BLOOD THEN YOU LOST THE TIME TRAVELING ABILITY. I'VE IMPLEMENTED A SIMILAR IDEA. IF YOU ARE AT 1 HEALTH FOR 5 CONSECUTIVE TURNS, THEN YOU CAN NO LONGER TIME TRAVEL. ONE WAY TO GET AROUND THIS IS TO KILL YOURSELF WITH A BOMB BEFORE THAT HAPPENS.~%~%"))
  (init-game)
  (loop do (main-game)
     while (when (y-or-n-p "~2%WOULD YOU LIKE TO PLAY AGAIN?")
	     (setf *time-travelp* nil)
	     (if (y-or-n-p "WOULD YOU LIKE TO USE THE SAME BOARD?")
		 (init-game T)
		 (init-game))
	     T)))

(defun main-game ()
  (setf *alien-time-travelingp* nil)
  (main-game-loop (lambda () (and (not *quit-gamep*) (not *alien-time-travelingp*) 
				  (not (winp)) (not (losep))))
		  (lambda () (format t "ENTER A COMMAND: "))
		  #'print-location-info
		  (lambda ()
		    (when (plusp (tomc-charge-limit *player*))
		      (decf (tomc-charge-limit *player*)))
		    (when (and *time-travelp* 
			       (= (tomc-health *player*) 1)
			       (= (incf (tomc-low-blood-count *player*))
				  *low-blood-limit*))
		      (format t "YOU HAVE LOST TOO MUCH BLOOD AND LOST THE ABILITY TO TIME TRAVEL...~%")
		      (setf *time-travelp* nil))))
  (cond 
    ((winp) (format t "~&CONGRATULATIONS. YOU'VE HELD OFF THE INVASION, FOR NOW..."))
    ((and (<= (tomc-health *player*) 0) *time-travelp*)
     (format t "YOU HAVE THE BLOOD OF THE ALPHA AND YOUR DEATH TRIGGERED TIME TRAVEL~%")
     (init-game T T) (format t "~&GREAT SCOTT!~%")
     (main-game))
    (*alien-time-travelingp* (format t "THE OMEGA TURNS BACK THE CLOCKS...~%")
			     (init-game) (format t "~&GREAT SCOTT!~%")
			     (main-game))
    ((losep) (format t "~&HA HA HA YOU DIED AND THE WUMPUS INVASION WAS A SUCCESS"))))
  
(defun winp ()
  "You win the game if you kill the omega."
  (notany (lambda (h) (eq 'omega (type-of h))) *hazards*))

(defun losep ()
  "You lose the game if you lose all of your health."
  (<= (tomc-health *player*) 0))

(defun vowel-p (char)
  "Returns the passed in character if it is a vowel and nil otherwise."
  (find char "aeiou" :test #'char-equal))

(defun starts-with-vowel (s)
  "Returns true if the passed in string or symbol begins with a vowel"
  (vowel-p (char (string s) 0)))

(defun print-location-info () 
  "Prints information about your current status in the game."
  (format t "~&~%YOU ARE IN SECTOR: ~a~%" (tomc-location *player*))
  (loop for h in *hazards* for type = (type-of h) do
       (when (= (hazard-location h) (tomc-location *player*))
	 (format t "THERE IS A~:[~;N~] ~a IN YOUR SECTOR!~%" (starts-with-vowel type) type)))
  (let ((adj-tunnels (adjacent-tunnels (tomc-location *player*))))
    (format t "NEIGHBORING SECTORS: ~a~%" adj-tunnels)
    (loop for h in *hazards* do
	 (when (member (hazard-location h) adj-tunnels)
	   (case (type-of h)
	     (mimic (format t "YOU HEAR A NEARBY MIMIC~%"))
	     (alpha (format t "YOU SENSE A POWERFUL PREDATOR~%"))
	     (omega (format t "YOU SEE A FAINT GLOW~%"))))))
  (format t "~&YOU HAVE ~a HEALTH REMAINING~%" (tomc-health *player*))
  (format t "~&YOU HAVE ~a SHOTS REMAINING~%" (tomc-ammo *player*))
  (format t "~&YOU HAVE ~a TURNS BEFORE YOU CAN CHARGE~%" (tomc-charge-limit *player*))
  (format t "~&YOU HAVE ~a BOMBS REMAINING~%" (tomc-bombs *player*))
  (when (and *time-travelp* (= (tomc-health *player*) 1))
    (format t "YOU HAVE LOW BLOOD... YOU HAVE ~a TURNS BEFORE YOU CAN'T TIME TRAVEL~%" (- *low-blood-limit* (tomc-low-blood-count *player*)))))

(let (old-player old-hazards old-cave old-random-state)
  (defun init-game (&optional replay-gamep same-random-state-p)
    "Initializes the game."
    (setf *quit-gamep* nil)
    (if replay-gamep
	(progn
	  (setf *player* (copy-structure old-player)
		*hazards* (mapcar #'copy-structure old-hazards)
		*cave* (mapcar #'copy-list old-cave))
	  (when same-random-state-p
	    (setf *random-state* (make-random-state old-random-state))))
	(let ((positions (n-rand-unique 10 (length (setf *cave* (regular-cave 6 6))))))
	  (setf *player* (make-tomc :location (nth 0 positions) :ammo 4 :low-blood-count 0
				    :bombs 1 :health 4 :charge-limit 0)
		*hazards* (append (loop for i from 1 to 7 collect
				       (make-mimic :location (nth i positions) :health 1))
				  (list (make-alpha :location (nth 8 positions) :health 3))
				  (list (make-omega :location (setf *omega-loc* (nth 9 positions)) :health 5)))
		old-player (copy-structure *player*)
		old-hazards (mapcar #'copy-structure *hazards*)
		old-cave (mapcar #'copy-list *cave*)
		old-random-state (make-random-state))))))

(defun remove-dead-hazards ()
  "Removes any dead hazards from the list."
  (setf *hazards* (remove-if (lambda (h) (<= (wumpus-health h) 0)) *hazards*)))

(defcommand q "Quits the game."
  quit-game () (setf *quit-gamep* T))

;; Adds a command that explains the other commands on the list.
(defcommand ? "Explains the available commands and what they do."
  command-help ()
  (format t "~%POSSIBLE COMMANDS:~%")
  (loop for c in *command-list* do 
       (format t "~a:~5t~a~%" (command-sym c) (command-description c)))
  (setf *skip-bookkeepingp* T))

;; Adds a moving command.
(defcommand m "Moves you to an adjacent spot. Use ex: m 12"
  player-move (move-loc)
  (if (and (integerp move-loc) 
	   (member move-loc (adjacent-tunnels (tomc-location *player*))))
      (progn 
	(setf (tomc-location *player*) move-loc)
	(terpri)
	(mapc (lambda (h) (when (= (hazard-location h) move-loc)
			    (princ "YOU BUMPED INTO ")
			    (case (type-of h)
			      ((mimic) (format t "A MIMIC...~%"))
			      ((alpha) (format t "THE ALPHA...~%"))
			      ((omega) (format t "THE OMEGA...~%")))))
	      *hazards*)
	(hazards-react #'hazard-ai))
      (progn 
	(format t "~%INVALID LOCATION: ~a" move-loc)
	(setf *skip-bookkeepingp* T))))

;; Three of the commands that handle different kinds of player attacks
;; are VERY similar. So I decided to make this macro to abstract away
;; that pattern. I'm not sure if having this many parameters is a good
;; thing... but it gets the job done.
(defmacro player-attack (attack-loc attack-condition valid-locs miss-str update-state hazard-fn error-str)
  (let ((a-loc (gensym)) (a-cond (gensym)))
    `(let ((,a-loc ,attack-loc) (,a-cond ,attack-condition))
       (if (and (integerp ,a-loc)
		,a-cond
		(member ,a-loc ,valid-locs))
	   (progn
	     (terpri)
	     (unless (find ,a-loc *hazards* :key #'hazard-location)
	       (format t "~a~%" ,miss-str))
	     ,@update-state
	     (hazards-react #',hazard-fn ,a-loc)
	     (remove-dead-hazards)
	     (hazards-react #'hazard-ai))
	   (progn
	     (if ,a-cond
		 (format t "~%INVALID LOCATION: ~a" ,a-loc)
		 (format t "~%~a" ,error-str))
	     (setf *skip-bookkeepingp* T))))))

;; Adds a shooting command
(defcommand s "Shoots an arrow into an adjacent spot. Use ex: s 10"
  player-shoot (shoot-loc)
  (player-attack shoot-loc 
		 (plusp (tomc-ammo *player*)) 
		 (adjacent-tunnels (tomc-location *player*))
		 "MISSED"
		 ((decf (tomc-ammo *player*)))
		 hazard-shoot-reaction
		 "NO MORE AMMO!"))

;; Adds a command that lets the user move AND do dammage
(defcommand c "You charge to an adjacent spot. Use ex: c 11"
  player-charge (charge-loc)
  (player-attack charge-loc
		 (zerop (tomc-charge-limit *player*))
		 (adjacent-tunnels (tomc-location *player*))
		 "YOU DIDN'T RUN INTO ANYTHING"
		 ((setf (tomc-charge-limit *player*) (1+ *charge-frequency*))
		  (setf (tomc-location *player*) charge-loc))
		 hazard-charge-reaction
		 "CAN'T CHARGE YET"))

;; Throws a bomb
(defcommand b "Throws a bomb into an adjacent spot OR your current location. Use ex: s 9"
  player-bomb (bomb-loc)
  (let ((p-loc (tomc-location *player*)))
    (player-attack bomb-loc
		   (plusp (tomc-bombs *player*))
		   (cons p-loc (adjacent-tunnels p-loc))
		   "YOU DIDN'T HIT ANY ALIENS..."
		   ((decf (tomc-bombs *player*))
		    (when (and (= p-loc
				  (hazard-location (find 'alpha *hazards* :key #'type-of)))
			       (= p-loc bomb-loc))
		      (setf *time-travelp* T))
		    (when (= p-loc bomb-loc)
		      (setf (tomc-health *player*) 0)
		      (format t "YOU BLEW YOURSELF UP AND DIED...~%")))
		   hazard-bomb-reaction
		   "DON'T HAVE ANY BOMBS")))

(defcommand k "Stabs an alien in your current location with a knife. Use ex: k" 
  player-stab ()
  (if (find (tomc-location *player*) *hazards* :key (lambda (h) (hazard-location h)))
      (progn
	(terpri)
	(hazards-react #'hazard-stab-reaction)
	(remove-dead-hazards)
	(hazards-react #'hazard-ai))
      (progn
	(format t "~%NOTHING TO STAB")
	(setf *skip-bookkeepingp* T))))

(defmacro setf1-non-nil (form value)
  "Sets one form to a value if the value is true."
  (let ((old-val (gensym)) (new-val (gensym)))
    `(let ((,old-val ,form) (,new-val (setf ,form ,value)))
       (if (null ,new-val)
	   (setf ,form ,old-val)
	   ,new-val))))

(defun ai-movement (h dmg article h-str)
  "How the Alpha and Mimic move around the board."
  (let ((h-loc (hazard-location h)) (p-loc (tomc-location *player*)))
    (cond 
      ((= h-loc p-loc))
      ((= p-loc *omega-loc*) (setf1-non-nil (hazard-location h) 
					    (second (shortest-path h-loc *omega-loc*))))
      ((within-one h-loc p-loc) (when (zerop (random 3))
				  (setf (hazard-location h) p-loc)))
      ((within-two *omega-loc* p-loc) (setf1-non-nil (hazard-location h) 
						     (second (shortest-path h-loc *omega-loc*))))
      ((= h-loc *omega-loc*) 
       (setf (hazard-location h) (random-list-elem (adjacent-tunnels h-loc))))
      ((within-two p-loc h-loc) 
       (when (zerop (random 2))
	 (setf (hazard-location h)
	       (random-list-elem (intersection (adjacent-tunnels h-loc)
					       (adjacent-tunnels p-loc))))))
      ((within-one *omega-loc* h-loc)
       (setf (hazard-location h) 
	     (random-list-elem (set-difference (adjacent-tunnels h-loc) (list *omega-loc*)))))
      (T (when (> (random 4) 0)
	   (setf (hazard-location h) (random-list-elem (adjacent-tunnels h-loc))))))
    (when (= p-loc (hazard-location h))
      (format t "~a ~a ATTACKED YOU!" article h-str)
      (decf (tomc-health *player*) dmg))))

(def-hazard-reactions hazard-ai (h)
  (mimic (ai-movement h 1 "A" "MIMIC"))
  (alpha (ai-movement h 2 "THE" "ALPHA"))
  (omega (when (not (find 'alpha *hazards* :key (lambda (haz) (type-of haz))))
	   (setf *alien-time-travelingp* T))))

(defun hazard-damage-reaction (h action-loc dmg skip-p action-str article h-str)
  (when (= action-loc (hazard-location h))
    (format t "YOU ~a ~a ~a... " action-str article h-str)
    (if (<= (decf (wumpus-health h) dmg) 0)
	(format t "YOU KILLED THE ~a" h-str)
	(format t "THE ~a HAS ~a HEALTH REMAINING" h-str (wumpus-health h)))
    (when skip-p
      (cons 'change-step (lambda (lst) nil)))))

;; How the hazards react to being shot
(def-hazard-reactions hazard-shoot-reaction (h shoot-loc)
  (mimic (hazard-damage-reaction h shoot-loc 1 T "SHOT" "A" "MIMIC"))
  (alpha (hazard-damage-reaction h shoot-loc 1 T "SHOT" "THE" "ALPHA"))
  (omega (hazard-damage-reaction h shoot-loc 1 T "SHOT" "THE" "OMEGA")))

;; How the hazards react to being charged into
(def-hazard-reactions hazard-charge-reaction (h charge-loc)
  (mimic (hazard-damage-reaction h charge-loc 1 T "CHARGED INTO" "A" "MIMIC"))
  (alpha (hazard-damage-reaction h charge-loc 1 T "CHARGED INTO" "THE" "ALPHA"))
  (omega (hazard-damage-reaction h charge-loc 1 T "CHARGED INTO" "THE" "OMEGA")))

;; How the hazards react to getting stabbed
(def-hazard-reactions hazard-stab-reaction (h)
  (mimic (hazard-damage-reaction h (tomc-location *player*) 1 T "STABBED" "A" "MIMIC"))
  (alpha (hazard-damage-reaction h (tomc-location *player*) 1 T "STABBED" "THE" "ALPHA"))
  (omega (hazard-damage-reaction h (tomc-location *player*) 1 T "STABBED" "THE" "OMEGA")))

;; How the hazards react to getting hit with a bomb
(def-hazard-reactions hazard-bomb-reaction (h bomb-loc)
  (mimic (hazard-damage-reaction h bomb-loc 3 NIL "BOMBED" "A" "MIMIC"))
  (alpha (hazard-damage-reaction h bomb-loc 3 NIL "BOMBED" "THE" "ALPHA"))
  (omega (hazard-damage-reaction h bomb-loc 3 NIL "BOMBED" "THE" "OMEGA")))

