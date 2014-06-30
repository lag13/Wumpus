Wumpus
======
Variations on the classic game "Hunt the Wumpus"

About
-----
Hunt the Wumpus is an old computer game created by Gregory Yob in 1972. The basic idea is that you are in a network of caves which contain hazards like bottomless pits, super bats, and of course the deadly wumpus. Your goal is to move throughout the caves, find the wumpus, and kill it before it kills you. See [Wikipedia](http://en.wikipedia.org/wiki/Hunt_the_Wumpus) for more information about this game.

My Alteration
-------------
To kill the wumpus you shoot an arrow at him. From what I remember reading about the original game, you get 1 "crooked" arrow. This arrow can travel through up to 5 different rooms without reversals (hence crooked). So you could shoot your arrow through rooms 1, 2, and 3 if you wanted. When I first read this rule I wasn't a huge fan of it. I thought it might be too easy to hit the wumpus if you were able to attack 5 places simultaneously. I also didn't like the idea of dying in bottomless pits purely by chance.

So my alteration is to give the player 3 arrows which can be shot into only one room. In addition to killing the wumpus, these arrows will alert you if you've hit hazards like bats or bottomless pits. This gives you a way to kind of "feel out" the cave without directly endangering your life.

altWumpusOO.lisp
----------------
My alteration of the classic game written in Lisp. To play it, fire up your favorite lisp interpreter (like [clisp](http://www.clisp.org/)) and call the function:
```
(hunt-the-wumpus)
```
In the game you can only do two things: shoot or move. To shoot just hit 's' and it will prompt you to enter the cave you want to shoot at. Similarly, type 'm' to move and it will prompt you to enter the cave you wish to move to. Happy playing!
