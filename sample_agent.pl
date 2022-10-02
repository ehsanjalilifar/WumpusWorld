%my_agent.pl

%   this procedure requires the external definition of two procedures:
%
%     init_agent: called after new world is initialized.  should perform
%                 any needed agent initialization.
%
%     run_agent(percept,action): given the current percept, this procedure
%                 should return an appropriate action, which is then
%                 executed.
%
% This is what should be fleshed out

:- use_module(library(clpfd)).

%% Variables
% Agent variables
:- dynamic agent_loc/2, agent_orient/1.
:- dynamic rich/1, arrow/1.
:- dynamic past/1.
:- dynamic killer/1.
:- dynamic shot_origin/2, shot_angle/1.
% Board variables
:- dynamic safe/2.
:- dynamic wall/2.
:- dynamic has_stench/2, has_breeze/2.
:- dynamic no_stench/2, no_breeze/2.
:- dynamic has_wumpus/2, has_pit/2.
:- dynamic maybe_wumpus/2, maybe_pit/2.
:- dynamic no_wumpus/2, no_pit/2.
:- dynamic has_glitter/2, no_glitter/2.
:- dynamic seen/2.
:- dynamic path/1.

%% Agent Variable Descriptions
% -- agent_loc: (X, Y), where X is row on board and Y
% is column on board. (1,1) is the entrance.
% -- agent_orient: A, where A is the direction the agent is 
% facing on the board. 0 = North, 1 = East, 2 = South, 3 = West.
% -- rich: "yes" if agent has gold, otherwise "no".
% -- arrow: "yes" if agent has arrow, otherwise "no".
% -- past: Past action taken by agent.
% -- killer: "yes" if we've killed the wumpus, otherwise "no".
% -- shot_origin: (X, Y) of arrow shot location
% -- shot_angle: A of arrow shot angle

%% Board Variable Descriptions
% -- safe: (X, Y) where we KNOW it's safe to step on.
% -- wall: (X, Y) where we KNOW there is a wall (out of bounds).
% -- has_stench: (X, Y) where we noticed a stench
% -- has_breeze: (X, Y) where we noticed a breeze
% -- no_stench: (X, Y) where we KNOW there's no stench.
% -- no_breeze: (X, Y) where we KNOW there's no breeze.
% -- has_wumpus: (X, Y) where we KNOW the wumpus is.
% -- has_pit: (X, Y) where we KNOW a pit is.
% -- has_glitter: (X, Y) where we KNOW glitter is
% -- no_glitter: (X, Y) where we KNOw there's no glitter
% -- no_wumpus: (X, Y) where we KNOW there's no wumpus
% -- no_pit: (X, Y) where we KNOW there's no pit
% -- maybe_wumpus: (X, Y) where a wumpus COULD be (based on stench)
% -- maybe_pit: (X, Y) where a pit COULD be (based on breeze)
% -- seen: (X, Y) where we've been to a space before (prioritize going to unseen spaces)
% -- path: Path of actions that bring us back to the entrance (in reverse order)

%% Tracking Percepts
% These functions update our knowledge base based on our past
% action and what we just observed
% Percept options are:
% [Stench, Breeze, Glitter, Bump, Scream]
% Actions are:
% goforward, turnleft, turnright, grab, shoot, climb
track_percepts(PastAction, [Stench, Breeze, Glitter, Bump, Scream]):-
  track_movement(PastAction, Bump), % Important that this happens first, updates our position
  track_rotate(PastAction),
  track_stench(PastAction, Stench),
  track_breeze(PastAction, Breeze),
  track_glitter(PastAction, Glitter),
  track_scream(PastAction, Scream).


%track_stench(Past, Stench).
% Stench
track_stench(_, yes):-
  agent_loc(X, Y),
  assert(has_stench(X, Y)).
% No stench
track_stench(_, no):-
  agent_loc(X, Y),
  assert(no_stench(X, Y)).


% track_breeze(Past, Breeze).
% Breeze
%%%%%%%%%% Your Code Here %%%%%%%%%%


% track_glitter(Past, Glitter).
% Mark glitter spot
track_glitter(_, yes):-
  agent_loc(X, Y),
  assert(has_glitter(X, Y)).
% We took the glitter, we're rich!
track_glitter(grab, no):-
  agent_loc(X, Y),
  retract(has_glitter(X, Y)),
  assert(no_glitter(X, Y)),
  assert(rich(yes)).
% Do nothing
track_glitter(_, no):-
  agent_loc(X, Y),
  assert(no_glitter(X, Y)).


% track_bump(Action, Bump).
% Don't move, mark wall
track_movement(goforward, yes):-
  agent_loc(X, Y),
  agent_orient(A),
  step_forward(X, Y, A, X1, Y1),
  assert(seen(X1, Y1)), % Technically mark wall as seen
  assert(wall(X1, Y1)). % Mark wall
% Move forward
track_movement(goforward, no):-
  agent_loc(X, Y),
  agent_orient(A),
  assert(seen(X, Y)), % Mark previous location as seen
  step_forward(X, Y, A, X1, Y1),
  retract(agent_loc(X, Y)),
  assert(agent_loc(X1, Y1)),
  path(P), % Track path return
  retract(path(P)),
  append([turnleft, turnleft, goforward], P, NewP),
  assert(path(NewP)).
% Do nothing (we didn't move forward)
track_movement(_, _).


% track_rotate(Past).
% No associated percept with this
%%%%%%%%%% Your Code Here %%%%%%%%%%


% track_scream(Past, Scream).
%%%%%%%%%% Your Code Here %%%%%%%%%%


%% Our logical rules based on our Percepts
% A cell is safe if there's no wumpus or pit there
safe(X, Y):-
  no_wumpus(X, Y),
  no_pit(X, Y).


% A cell has no wumpus if there's no stench in at least
% one of its adjacent squares
% Note that we need #= instead of is because of 
% https://stackoverflow.com/questions/23815952/prolog-arguments-are-not-sufficiently-instantiated
no_wumpus(X, Y):-
  XUp #= X+1,
  XDown #= X-1,
  YRight #= Y+1,
  YLeft #= Y-1,
  (
    no_stench(XUp, Y);
    no_stench(XDown, Y);
    no_stench(X, YRight);
    no_stench(X, YLeft)
  ).
% Wumpus can't be alive if we've killed it
no_wumpus(_, _):-
  killer(yes).


% A cell might have a wumpus if we don't know if there's a wumpus
% there and one of the adjacent squares has a stench
%%%%%%%%%% Your Code Here %%%%%%%%%%


% A wumpus is surrounded by at least 3 stenches or against a wall/pit
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If X, Y has two adjacent stenchs, but its diagonal doesn't have a wumpus,
% then X, Y must have the wumpus
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If there is a stench on either side of X, Y, then the wumpus must be there
%%%%%%%%%% Your Code Here %%%%%%%%%%


% A cell has no pit if there's no breeze in at least
% one of its adjacent squares
no_pit(X, Y):-
  XUp #= X+1,
  XDown #= X-1,
  YRight #= Y+1,
  YLeft #= Y-1,
  (
    no_breeze(XUp, Y);
    no_breeze(XDown, Y);
    no_breeze(X, YRight);
    no_breeze(X, YLeft)
  ).


% A cell might have a pit if we don't know if there's a pit
% there and one of the adjacent squares has a breeze
%%%%%%%%%% Your Code Here %%%%%%%%%%


% A pit is surrounded by breezes or against a wall
%%%%%%%%%% Your Code Here %%%%%%%%%%
  

% Check if the arrow hit a point
% We can assume the arrow also hits the point
% it was shot from
%%%%%%%%%% Your Code Here %%%%%%%%%%
% Angle 0 (North): Y must match and X >= XOrigin
%%%%%%%%%% Your Code Here %%%%%%%%%%
% Angle 1 (East): X must match and Y >= YOrigin
%%%%%%%%%% Your Code Here %%%%%%%%%%
% Angle 2 (South): Y must match and X =< XOrigin
%%%%%%%%%% Your Code Here %%%%%%%%%%
% Angle 3 (West): X must match and Y =< YOrigin
%%%%%%%%%% Your Code Here %%%%%%%%%%


% Calculate rotations
%%%%%%%%%% Your Code Here %%%%%%%%%%


% North
%%%%%%%%%% Your Code Here %%%%%%%%%%


% East
%%%%%%%%%% Your Code Here %%%%%%%%%%


% South
%%%%%%%%%% Your Code Here %%%%%%%%%%


% West
%%%%%%%%%% Your Code Here %%%%%%%%%%


% Look at what our step would be
%%%%%%%%%% Your Code Here %%%%%%%%%%


% We can't move anywhere :(
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If number of safe and visited cells are equal
%%%%%%%%%% Your Code Here %%%%%%%%%%


%% Action deciders
% Note that we only need Action since
% we've already updated our knowledge base
% Actions are:
% goforward, turnleft, turnright, grab, shoot, climb


% If we're on the gold, grab it!
get_action(Action):-
  agent_loc(X, Y),
  has_glitter(X, Y),
  Action=grab.


% If we have at least one gold and we're at the
% entrance, climb!
get_action(Action):-
  rich(yes),
  agent_loc(X,Y),
  X =:= 1,
  Y =:= 1,
  Action=climb.


% If nowhere left to go, climb
get_action(Action):-
  all_explored,
  agent_loc(X, Y),
  X =:= 1,
  Y =:= 1,
  Action=climb.


% If there's nowhere left to explore, go home
%%%%%%%%%% Your Code Here %%%%%%%%%%


% Move forward to an unexplored space
%%%%%%%%%% Your Code Here %%%%%%%%%%


% Turn towards unexplored space
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If we're facing the wumpus, fire!
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If we're next to the wumpus and we have the arrow, face it!
%%%%%%%%%% Your Code Here %%%%%%%%%%


% No new spot to explore and no wumpus to kill, Move forward if we can do so safely
% and without bumping a wall
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If we're stuck and facing where a wumpus might be, fire!
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If we're stuck and out of ammo, we might've missed the wumpus, just leave :(
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If we're stuck and out of ammo, we might be surrounded by pits, just leave :(
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If forward isn't safe or there's a wall,
% look to the left. If its safe and there's no wall,
% rotate left
%%%%%%%%%% Your Code Here %%%%%%%%%%


% If there's literally nothing else to do, rotate right.
%%%%%%%%%% Your Code Here %%%%%%%%%%

% Reset some variables
reset:-
  retractall(agent_loc(_,_)),
  retractall(agent_orient(_)),
  retractall(rich(_)),
  retractall(arrow(_)),
  retractall(past(_)),
  retractall(killer(_)),
  retractall(shot_origin(_,_)),
  retractall(shot_angle(_)),
  retractall(wall(_,_)),
  retractall(has_stench(_,_)),
  retractall(has_breeze(_,_)),
  retractall(no_stench(_,_)),
  retractall(no_breeze(_,_)),
  retractall(seen(_,_)),
  retractall(path(_)).

%% Interface
% evaluate_agent(1, Score, Time).
init_agent:-
  format('===== Starting game ====='),
  display_world,
  reset,
  assert(agent_loc(1,1)),
  assert(agent_orient(1)),
  assert(rich(no)),
  assert(arrow(yes)),
  assert(past(spawn)),
  assert(killer(no)),
  assert(no_wumpus(1,1)),
  assert(no_pit(1,1)),
  assert(safe(1,1)).

run_agent(Percepts, Action):-
  display_world,
  %sleep(1),
  past(P),
  track_percepts(P, Percepts),
  get_action(H),
  retract(past(P)),
  assert(past(H)),
  Action=H.
