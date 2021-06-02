balls(S) :- balls(S,[S]).
balls([1,0,0],States) :- debug(States).
balls([0,1,0],States) :- debug(States).
balls([0,0,1],States) :- debug(States).
balls(State,States) :- genmove(State,NState),
    				   notIn(NState,States),
    				   balls(NState,[NState|States]).
genmove([R,G,B],[Rn,Gn,Bn]):- R>0,G>0, Rn is R-1, Gn is G-1, Bn is B+1. %RG
genmove([R,G,B],[Rn,Gn,Bn]):- R>0,B>0, Rn is R-1, Gn is G+1, Bn is B-1. %RB
genmove([R,G,B],[Rn,Gn,Bn]):- B>0,G>0, Rn is R+1, Gn is G-1, Bn is B-1. %GB

notIn(_E,[]).
notIn(E,[H|T]) :- E\=H, notIn(E,T).

debug([]).
debug([H|T]):-debug(T),writeln(H).