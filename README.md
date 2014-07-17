Wumpus
======
Variations on the classic game "Hunt the Wumpus"

About
-----
Hunt the Wumpus is an old computer game created by Gregory Yob in 1972. The basic idea is that you are in a network of caves which contain hazards like bottomless pits, super bats, and of course the deadly wumpus. Your goal is to move throughout the caves, find the wumpus, and kill it before it kills you. See [Wikipedia](http://en.wikipedia.org/wiki/Hunt_the_Wumpus) for more information about this game.

Originally I started out with a single common lisp file which implemented a slightly altered version of hunt the wumpus. In my version arrows were not "crooked" i.e. they could only be shot into an adjacent room. Another alteration was that if you shoot the other hazards (bats or pits) with an arrow they "react" by printing out a message. I did that because I liked the idea of a player being able to "feel" their way through the cave rather than just move and hope for the best.

Over time I abstracted away functions from the original lisp implementation into 2 separate files. One of these files is (I think) a rudimentary text game engine and the other is a game engine for hunt the wumpus type games. My hope is that now, if I wanted to, I could create text based games more easily and, when creating these games, be able focus more on the game logic rather than other bookeeping.

myWumpus.lisp
----------------
My alteration of the classic game written in Lisp which uses those two game engines I described. To play it, fire up your favorite lisp interpreter (like [clisp](http://www.clisp.org/)), load the file, and call the function: (hunt-the-wumpus).
```
(load "myWumpus.lisp")
(hunt-the-wumpus)
```
In the game you can only do two things: shoot or move. To shoot just hit 's' and it will prompt you to enter the cave you want to shoot at. Similarly, type 'm' to move and it will prompt you to enter the cave you wish to move to. Happy playing!
