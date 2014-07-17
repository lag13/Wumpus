(load "textGameEngine.lisp")

(defvar *player* nil
  "The player character.")

(defvar *hazards* nil
  "A list of hazards.")

(defvar *cave* nil
  "The cave of tunnels.")

;; The player is defined by location, number of arrows, and 
;; amount of health.
(defstruct player location arrows health)

;; All hazards will be derived from the hazard class
;; All wumpus's will be derived from the wumpus class
(defstruct hazard location)
(defstruct (wumpus (:include hazard)) health)

;; In my setup, caves are a list of lists. The car of each
;; inner list is the node and the cdr is the nodes adjacent
;; to that node.
(defun adjacent-tunnels (location) 
  "Returns the list of tunnels adjacent to location."
  (cdr (assoc location *cave*)))

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

;; If the user wants different winning or losing conditions 
;; they could make such functions themselves. However, if they
;; want to make a new losing condition it MUST have the same name
;; as this one. The only reason is that the "hazards-react" function
;; below calls this function and I don't feel like passing it in
;; as a parameter.
;; I don't know if that's the best way to go about doing things
;; (probably not) but I think it works.
(defun lost-gamep ()
  "Default way to lose a game."
  (or 
   (<= (player-health *player*) 0) 
   (<= (player-arrows *player*) 0)))

(defun won-gamep ()
  "Default way to win a game is to kill all wumpus's."
  (loop for h in *hazards* 
	always (if (eq 'wumpus (type-of h)) (<= (wumpus-health h) 0) T)))

;; This game is turn based, you take an action and then the hazards react.
;; So my idea was that the hazards will all have "reaction" methods to your 
;; actions. Then, after completing your action, you could just loop over 
;; the hazards and call their reaction methods. This function does this 
;; looping. One initial challenge with this approach was dealing with
;; the super bats which carry you to a new location. Since you are no longer
;; in the location which prompted the looping in the first place then we
;; shouldn't keep looping over the hazards. To get around this I let those 
;; reaction methods change the looping behavior of this function. So in the 
;; case of the bats, they could essentially tell the loop to stop. To change 
;; how the loop steps the reaction methods can return a cons pair containing 
;; the symbol CHANGE-STEP and a function representing the new step.
(defun hazards-react (hazard-react-method &rest args)
  "Loops over the list of hazards and calls their reaction methods."
  (loop with step = #'cdr with lst = *hazards* with h with return-val
	while (and (setf h (car lst))
		   (not (lost-gamep)))
	do
	(fresh-line)
	(setf return-val (apply hazard-react-method h args))
	(when (and (consp return-val) 
		   (eq (car return-val) 'change-step))
	  (setf step (cdr return-val)))
	(setf lst (funcall step lst))))
