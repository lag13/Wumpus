;;;; A text game engine

;; Originally when I wrote this text game engine there were two main
;; things that it defined. One was an environment where the "core"
;; code of a game could be run. What this environment mainly did was
;; repeatedly read user input, match it up with a defined command, and
;; call the function associated with that command (while also reading
;; any arguments necessary for the funciton). The other was a couple
;; of macros that allowed programmer's to create the commands that the
;; "main game loop" would be matching up with user input. I often
;; change my mind about how to program somthing and this was no
;; exception.

;; In particular, I thought my implementation of a text game engine
;; was too specific. If this code could be used for many different
;; types of games then I shouldn't specify how user input is
;; handled; I should let the programmer's of specific games deal with
;; that! So, with that minimalist philosophy in mind, I modified this
;; file.

;; The main-game-loop is now a macro which essentially pastes passed
;; in code into a little template. My idea when creating it was that
;; most games will pretty much follow this pattern: Some computation
;; could be done before user input, input is handled and action is
;; taken based on it, some computation could be done after handling
;; input, and this cycle will repeat based on the state of the
;; game. In some specific situations you might want to avoid the "pre"
;; and "post" input computations so, to that end, this macro defines a
;; couple of functions that could be used to skip these parts. Because
;; I thought it was simpler to accomplish this "skipping" behavior
;; using jumps, rather than looping and conditionals, the looping
;; behavior of the macro is accomplished using the "tagbody" special
;; operator. Another possible reason for doing this is because, ever
;; since I did SPIM assembly programming in school, I've always liked
;; the ability to jump around code. I know it can lead to "spaghetti
;; code" but the amount of control jumping gives you is just too cool.

;; Note that defining these "skipping" functions within the macro
;; works for two reasons: One. As far as I can tell by reading the
;; documentation, wherever defun gets used it defines a global
;; function. Two. When the functions are defined within the tagbody
;; like this they capture the value of the tags that they reference
;; (thats a closure) so even when the functions are called in
;; different contexts, they'll still reference those specific tags.

;; One reason why I like how this code is written is because to
;; create an infinite REPL loop I can now just write this:

;;           (main-game-loop :handle-input ((print (eval (read))))) 

;; On some level, I think being able to do that is aesthetically
;; pleasing but this ability to redifine how input is handled could
;; make debugging and testing easier. For instance, when I want to
;; test out my code I could change the :handle-input keyword parameter
;; to be: ((eval (read))). Then I could directly call functions
;; associated with user input to simulate normal game behavior, but I
;; could also manipulate the game state to test out specific
;; situations.

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

