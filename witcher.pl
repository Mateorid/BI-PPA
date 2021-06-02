/* BI-PPA Prolog witcher adventure game */


/*Free memory*/
:- retractall(currently_in(_)),retractall(is_in(_,_)), retractall(description(_,_)), retractall(door(_,_,_,_,_)).
:- ansi_format([bold,fg(green)],"Enter 'start.' to start the game.", []).


/*Shortcuts*/
n :- move(n).
s :- move(s).
e :- move(e).
w :- move(w).
i :- inventory().
l :- look().
r :- consult('witcher.pl').
h :- help().


/*Start implementation*/
start :-	h(), assert(currently_in(entrance)), entered(entrance).


/* Paths --> path(from,direction,to)*/
path(entrance, n, main_hall).
path(main_hall, s, entrance).
path(main_hall, n, long_hall).
path(main_hall, w, bone_hall).
path(main_hall, e, chest_hall).
path(bone_hall,w,hall_of_howls).
path(bone_hall,e,main_hall).
path(hall_of_howls,w,iron_room).
path(hall_of_howls,n,werewolf_room).
path(hall_of_howls,e,bone_hall).
path(iron_room,e,hall_of_howls).
path(werewolf_room,s,hall_of_howls).
path(werewolf_room,n,fountain_room).
path(fountain_room,s,werewolf_room).
path(blue_room,e,fountain_room).
path(chest_hall,w,main_hall).
path(chest_hall,e,kitchen).
path(kitchen,w,chest_hall).
path(kitchen,n,loud_hall).
path(loud_hall,s,kitchen).
path(loud_hall,e,bandits_lounge).
path(bandits_lounge,w,loud_hall).
path(bandits_lounge,e,corridor).
path(corridor,n,silver_room).
path(corridor,s,red_room).
path(corridor,w,bandits_lounge).
path(silver_room,s,corridor).
path(red_room,n,corridor).
path(long_hall,s,main_hall).
path(long_hall,n,statue_hall).
path(statue_hall,e,storage_room).
path(statue_hall,s,long_hall).
path(statue_hall,w,hall_of_paintings).
path(storage_room,w,statue_hall).
path(hall_of_paintings,e,statue_hall).
%path(treasure_room,s,hall_of_paintings).		You win in here, no point of keeping it.
%path(hall_of_paintings,n,treasure_room).		BLUE DOOR
%path(fountain_room,w,blue_room).     			RED DOOR


/*Spawns all the entitites and necessary stuff, is_in(WHAT,WHERE)*/
:-
assert(is_in(player,alive)),
assert(is_in(player,playing)),
assert(is_in(silver_sword,silver_room)),
assert(is_in(werewolf,werewolf_room)),
assert(is_in(bandit,bandits_lounge)),
assert(is_in(iron_sword,iron_room)),
assert(is_in(health_potion,storage_room)),
assert(is_in(teleport,hall_of_howls)),
assert(is_in(blue_key,blue_room)),
assert(is_in(red_key,red_room)),
assert(door(hall_of_paintings,n,treasure_room,locked, blue)),		%Blue door
assert(door(fountain_room, w, blue_room,locked, red)). 				%Red door


/*Triggers sequence of tasks when entereing a given room*/
entered(R) :-	describe(R), in_room(R), monsters(R), status(), doors(R).


/*Prints description of given room*/
describe(R) :-	description(R,X), !, write(X), nl.


/*Prints all possible directions*/
doors(R) :-		path(R,n,_), write("You can go north."), nl, fail ;
				path(R,s,_), write("You can go south."), nl, fail ;
				path(R,e,_), write("You can go east." ), nl, fail ;
				path(R,w,_), write("You can go west." ), nl,	! ;
				door(R,_,blue_room,locked,red),					!,
					ansi_format([fg(red)], 'There''s a locked red door to the west', []);
				door(R,_,blue_room,unlocked,red),				!,
					ansi_format([fg(red)], 'There''s an unlocked red door to the west', []);
				door(R,_,_,locked,blue),						!,
					ansi_format([fg(blue)], 'There''s a locked blue door to the south', []);
				door(R,_,_,unlocked,blue),						!,
					ansi_format([fg(blue)], 'There''s an unlocked blue door to the south', []);
				true.


/*Prints your inventory*/
inventory() :-	(is_in(X,inventory), 
				ansi_format([underline,fg(magenta)], 'You have ~w in your inventory.', [X]),
				nl, fail); true.


/*Handles movement input*/
move(Dir) :-	is_in(player,alive), is_in(player,playing), currently_in(Cur), path(Cur,Dir,Dest), !,
				retract(currently_in(_)), assert(currently_in(Dest)), entered(Dest).
/*Locked red door with key*/
move(Dir) :-	is_in(player,alive), is_in(player,playing), currently_in(Cur), door(Cur,Dir,Dest,locked,red), is_in(red_key,inventory), !, 		
				retract(door(_,_,_,locked,red)), retract(is_in(red_key,inventory)), assert(door(Cur,Dir,Dest,unlocked,red)),
				retract(currently_in(_)), assert(currently_in(Dest)), ansi_format([fg(yellow)],"You unlocked the door.~n", []),
				entered(Dest).
/*Locked blue door with key*/
move(Dir) :-	is_in(player,alive), is_in(player,playing), currently_in(Cur), door(Cur,Dir,Dest,locked,blue), is_in(blue_key,inventory), !, 	
				retract(door(_,_,_,locked,blue)), retract(is_in(blue_key,inventory)), assert(door(Cur,Dir,Dest,unlocked,blue)),
				retract(currently_in(_)), assert(currently_in(Dest)), ansi_format([fg(yellow)],"You unlocked the door.~n", []),
				entered(Dest).
/*Unlocked door movement*/
move(Dir) :-	is_in(player,alive), is_in(player,playing), currently_in(Cur), door(Cur,Dir,Dest,unlocked,_), !,									
				retract(currently_in(Cur)), assert(currently_in(Dest)), entered(Dest).
/*No such path*/
move(_) :-		is_in(player,alive), is_in(player,playing), !, ansi_format([fg(red)],"There's no path there or the path is locked!",[]).
/*Game over movement*/
move(_) :- 		ansi_format([fg(yellow)],"Game over~nEnter 'r.' to restart the game or 'halt.' to exit.",[]).


/*Prints what is in the room*/
in_room(R) :-	(is_in(X,R), ansi_format([fg(yellow)],
					 'There''s a ~w in this room!', [X]),
					 nl, fail
					); true.


/*Prints info about the current room*/
look() :-		currently_in(R), describe(R), in_room(R), doors(R).


/*Handles the combat*/
monsters(R) :-	is_in(bandit,R), (is_in(iron_sword,inventory), ! *->
					retract(is_in(bandit,_)), assert(is_in(dead_bandit,R)),
					ansi_format([fg(yellow)], 
					'Luckily you have the iron sword that you found earlier ~nand you slash the bandits head of with a swift spin.~n',[])
					;
				assert(is_in(damage,body)),
				ansi_format([fg(white),bg(red)], 'You don''t have the iron sword necessary to kill the bandit, you recieved heavy damage from him!', []), nl), fail.
monsters(R) :-	is_in(werewolf,R), (is_in(silver_sword,inventory), ! *->
					retract(is_in(werewolf,_)), assert(is_in(dead_werewolf,R)),
					ansi_format([fg(yellow)], 
					'You use your silver sword to roll behind the werewold and behead him!~n',[])
					;
				assert(is_in(damage,body)),
				ansi_format([fg(white),bg(red)], 'You don''t have the necessary silver sword to kill the werewolf, you recieved heavy damage from him!', []), nl), fail.
monsters(_) :-	true.


/*Checks for death/win situations*/
status() :-		currently_in(treasure_room), ansi_format([bold,fg(white),bg(yellow)], 'VICTORY you found the treasure!', []), nl,
				ansi_format([bold,fg(yellow)],"Enter 'r.' to restart the game or 'halt.' to exit.", []), !, retract(is_in(player,playing)), fail.
status() :-		damage(X),X > 1, ansi_format([bold,fg(white),bg(red)], 'DEFEAT, you took too much damage.', []),
				retract(is_in(player,alive)), !, nl,
				ansi_format([bold,fg(red)],"Enter 'r.' to restart the game or 'halt.' to exit.", []),fail.
status() :- 	true.


/*damage(-D) :- returns how much damage has player recieved*/
damage(D) :-	findall(Y, is_in(Y,body), X), length(X,D).


/*Move item from current room to inventory*/
take(destination) :-	ansi_format([bold,fg(red)],"You can't do that. It's a magical entity that can't be stored in an inventory!~nNice try tho :^)", []).
take(teleport)	:-		currently_in(X) ,is_in(teleport,X), !, retractall(is_in(destination,_)), retract(is_in(teleport,X)), assert(is_in(teleport,inventory)),
						ansi_format([fg(green)],"You pickuped a teleport! Drop it in your desired destination room to set teleport destination.", []).
take(X) :-				currently_in(Y), is_in(X,Y), !, retract(is_in(X,_)), assert(is_in(X,inventory)), ansi_format([fg(green)],"You pickuped ~w!", [X]).
take(_) :- 				ansi_format([fg(red)],"I don't see that here!",[]).


/*Moves item from inventory to current room*/
drop(teleport) :-		is_in(teleport,inventory), !, currently_in(Y), retract(is_in(teleport,inventory)), assert(is_in(teleport,Y)),
						assert(is_in(destination,Y)), ansi_format([fg(blue)],"You set the teleport destination to ~w.", [Y]).
drop(X) :-				is_in(X,inventory), !, currently_in(Y), retract(is_in(X,inventory)), assert(is_in(X,Y)), ansi_format([fg(blue)],"You dropped ~w!", [X]).
drop(_) :-				ansi_format([fg(red)],"I don't have that!",[]).


/*use(item)*/
use(health_potion) :- 	is_in(health_potion,inventory), !, retract(is_in(health_potion,inventory)), retractall(is_in(damage,_)), ansi_format([fg(green)],"You patched up all your wounds.", []).
use(teleport) :- 		is_in(teleport,inventory), !, ansi_format([fg(red)],"You did not set a destination. You can set teleports destination by dropping it in the desired destination room!",[]).
use(teleport) :-		is_in(destination,X), !, retract(currently_in(_)), assert(currently_in(X)), retractall(is_in(teleport,_)), retractall(is_in(destination,_)),
						ansi_format([bold,fg(blue)],"*magical teleportation noises*~n", []), entered(X).
use(X) :-				is_in(X,inventory), !, ansi_format([fg(blue)],"Can't really use ~w here...",[X]).
use(X) :-				ansi_format([fg(red)],"You don't have a ~w.",[X]).


/*Help*/
help() :-																		nl,
			ansi_format([fg(yellow)],"Witcher adventure game controlls", []),   nl,
			write("start.         ...     Starts the game"), 					nl,
			write("n./s./e./w.    ...     Move in that direction"),				nl,
			write("i.             ...     Prints your inventory"), 				nl,
			write("l.             ...     Prints info about current room"), 	nl,
			write("take(object).  ...     Stores object in your inventory"), 	nl,
			write("drop(object).  ...     Drops object from your inventory"), 	nl,
			write("h./help.       ...     Shows this help text"), 				nl,
			write("r.             ...     Restarts the game"), 					nl,
			nl, nl.


/*Room descriptions*/
:-
assert(description(entrance,"You're standing in front of an abandoned palace, you see some light on the east side of the palace.")),
assert(description(main_hall,"You're standing in the main hall, it's really quiet...")),
assert(description(long_hall,"You entered a very long quiet hall. You can hear the winds whistling through the broken windows...")),
assert(description(bone_hall,"You're in a hall filled with old human & animal bones. Scary...")),
assert(description(chest_hall,"You're standing in something that looks like an old storage room. There's bunch of chest by the walls.")),
assert(description(hall_of_howls,"Just an ordinary room, but you hear an scary sound coming from behind the north door...")),
assert(description(werewolf_room,"You enetered the werewolfs nest!!!")),
assert(description(fountain_room,"A quiet pretty room with an old non-functioning fountain in the middle.")),
assert(description(kitchen,"You entered what used to be a kitchen. There's bunch of old kitchen equipment.")),
assert(description(corridor,"An ordinary corridor with doors on the sides.")),
assert(description(silver_room,"You entered an old armory. It's been monstly looted tho...")),
assert(description(red_room,"You've come into what seems like an red-velvet lounge.")),
assert(description(statue_hall,"You're standing in a hall filled with old statues.")),
assert(description(storage_room,"Completely empty room with just bunch of cobwebs on the walls...")),
assert(description(hall_of_paintings,"A hall filled with old dusty paintings of some old men and women...")),
assert(description(treasure_room,"You entered the treasure room!!!")),
assert(description(loud_hall,"You entered a well lit room, you can hear some muffled human noise from behind the doors on the east.")),
assert(description(bandits_lounge,"You enetered the bandits room!!!")),
assert(description(iron_room,"An empty room with an iron sword stuck in a rock... hmm... this sounds familiar.")),
assert(description(blue_room,"You've come into what seems like an blue-velvet lounge.")).