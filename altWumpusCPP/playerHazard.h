#ifndef _PLAYER_HAZARDS_H_
#define _PLAYER_HAZARDS_H_
/*
Defines a classes for the player and the hazards.
*/
class Player
{
	public:
		int location;
		int numArrows;
		bool living;
	public:
		Player(int nloc, int nArrows);
		void setLocation(int loc)
		{
			location = loc;
		}
		void setNumArrows(int na)
		{
			numArrows = na;
		}
};

class Hazard
{
	protected:
		int location;

	public:
		Hazard(int loc);
		virtual void moveReaction(Player *p) = 0;
		virtual void shootReaction(int shotLoc, Player *p) = 0;
};

class Wumpus : public Hazard
{
	private:
		bool living;

	public:
		Wumpus(int loc);
		virtual void moveReaction(Player *p);
		virtual void shootReaction(int shotLoc, Player *p);
};

class SuperBat : public Hazard
{
		SuperBat(int loc);
		virtual void moveReaction(Player *p);
		virtual void shootReaction(int shotLoc, Player *p);
};

class Pit : public Hazard
{
		Pit(int loc);
		virtual void moveReaction(Player *p);
		virtual void shootReaction(int shotLoc, Player *p);
};

#endif
