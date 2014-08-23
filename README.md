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

edgeOfWumpus.lisp
-----------------
A hunt the wumpus type game inspired by the movie "Edge of Tomorrow". You are placed on a 6x6 grid along with mimics, one alpha, and one omega. The goal is to kill the omega. You have many different types of movement/attack which you can see by entering '?' while playing the game. The way I implemented time travelling was this: If you throw a bomb in your location, and the alpha is also in your location, then you will die and have the ability to time travel. So from now on, whenever you die you will start the game over.

The general gist of the alien AI is this: Only the mimics and the alpha move (and they move in the same way). If you are within two moves of the omega then they will travel towards it. If they are within two spots of you there is a chance they will try to follow you. If they are within one move of you there is a chance they will attack you. Other than that, they move randomly. The general strategy (in my mind) is to kill as many mimics as you can before making a move on the omega. To play, load the file and call the function: (edge-of-wumpus).
```
(load "edgeOfWumpus.lisp")
(edge-of-wumpus)
```
When I thought of making an "edge of tomorrow" based game I got excited because I enjoyed the movie and thought a game based on it would be fun. So I was a little disappointed when I finished this game and did not have as much fun as I thought I would. The reason for this is because the key to winning is getting that ability to time travel. Once you have that ability, the game just becomes you memorizing how the aliens move and responding appropriately. If you make a mistake (which will happen when you try new things) you'll have to start over and enter the same sequence of commands which brought you to that spot in the first place. So the whole process of winning becomes very tedious and there's no real challenge to it. Looking back on it, this fact seems obvious. Why would someone want to play a text based game where they keep redoing the same exact stuff just to get a little farther in the game? I will say that it made me realize how boring it must have been for Tom Cruise's character to have to repeat his actions from the previous day just so he could get a little farther.

This all sounds pretty harsh but in reality I enjoy playing the game and in creating it there were a lot of challenges that were fun to work through. So overall I'm fairly happy with it. A couple things I wanted to do was implement "recording" commands which let you record a sequence of moves so you don't have to keep doing the same things over and over. Also I should really seed that random number generator.
