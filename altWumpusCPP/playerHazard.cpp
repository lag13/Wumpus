#include <cstdlib>
#include "playerHazard.h"

Player::Player(int nloc, int nArrows)
	:	location(nloc),
		numArrows(nArrows)
{}

Hazard::Hazard(int loc)
	:	location(loc)
{}

Wumpus::Wumpus(int loc)
	:	Hazard(loc),
		living(true)
{}

/*
When the player moves 
*/
void
Wumpus::moveReaction(Player *p)
{
	if(p->location == this->location)
	{
		// Move to random adjacent tunnel or stay in the same spot
		// If spot is same then player is dead
	}
}

void
Wumpus::shootReaction(int shotLoc, Player *p)
{
	if(p->location == this->location)
	{
		this->living = false;
	}
	else
	{
		// Move randomly
	}
}

SuperBat::SuperBat(int loc)
	:	Hazard(loc)
{}

void
SuperBat::moveReaction(Player *p)
{
	if(p->location == this->location)
	{
		p->location = rand() % 20;
	}
}

void
SuperBat::shootReaction(int shotLoc, Player *p)
{
	if(p->location == this->location)
	{
		// print message
	}
}

Pit::Pit(int loc)
	:	Hazard(loc)
{}

void
Pit::moveReaction(Player *p)
{
	if(p->location == this->location)
	{
		p->living = false;
	}
}

void
Pit::shootReaction(int shotLoc, Player *p)
{
	if(p->location == this->location)
	{
		// print message
	}
}
