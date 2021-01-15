:- use_module(library(aproblog)).

:- use_semiring(
    sr_plus,   % addition (arity 3)
    sr_times,  % multiplication (arity 3)
    sr_zero,   % neutral element of addition
    sr_one,    % neutral element of multiplication
    sr_neg,    % negation of fact label
    false,     % requires solving disjoint sum problem?
    false).    % requires solving neutral sum problem?

sr_zero((0.0, 0.0)).
sr_one((1.0, 1.0)).
sr_times((Ta, Ca), (Tb, Cb), (Tc, Cc)) :- Ta < 0, Tb < 0, Tc is 0.0, Cc is Ca*Cb.
sr_times((Ta, Ca), (Tb, Cb), (Tc, Cc)) :- Ta > 0, Tc is Ta*Tb, Cc is Ca*Cb.
sr_times((Ta, Ca), (Tb, Cb), (Tc, Cc)) :- Tb > 0, Tc is Ta*Tb, Cc is Ca*Cb.
sr_times((Ta, Ca), (Tb, Cb), (Tc, Cc)) :- (Ta == 0.0; Tb == 0.0), Tc is 0.0, Cc is Ca*Cb.


sr_plus((Ta, Ca), (Tb, Cb), (Ta, Ca)) :- Ca > Cb.
sr_plus((Ta, Ca), (Tb, Cb), (Tb, Cb)) :- Cb > Ca.
sr_plus((Ta, C), (Tb, C), (Tc, C)) :- (Ta + Tb) \== 0.0, Tc is (sign(Ta+Tb) * max(Ta, Tb)).
sr_plus((Ta, C), (Tb, C), (Tc, C)) :- (Ta + Tb) == 0.0, Tc is max(Ta, Tb).


sr_neg((Ta, Ca), (Tb, Ca)) :- Ta > 0, Tb is 1.0-Ta.
sr_neg((Ta, Ca), (Tb, Ca)) :- Ta < 0, Tb is Ta-1.0. 
sr_neg((0.0, Ca), (0.0, Ca)). 

%%%% ProbLog does not support dif/2 %%%%
dif(A,B):- \+(A==B).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query(placeChain(chainAR,P,C)).

trusts(X,X).

trusts2(A,B,D) :-
    D > 0,
    trusts(A,B).
trusts2(A,B,D) :-
    D > 0,
    trusts(A,C),
    NewD is D - 1,
    trusts2(C,B,NewD).

%%% trustable relations declared by appOp
(0.99,0.99)::trusts(appOp, appOp).
(0.1,0.9)::trusts(appOp, privateCitzen).
(0.7,0.8)::trusts(appOp, unipi).
(0.9,0.8)::trusts(appOp, telco).
(0.9,0.9)::trusts(appOp, bigG).
(0.2,0.9)::trusts(appOp, openStreets).

%%% trustable relations declared by unipi
(-0.2,0.5)::trusts(unipi, privateCitzen).
(0.8,0.8)::trusts(unipi, telco).
(0.8,0.9)::trusts(unipi, bigG).
%%% trustable relations declared by telco
(-0.5,0.9)::trusts(telco, privateCitzen).
(0.7,0.9)::trusts(telco, unipi).
(0.8,0.9)::trusts(telco, bigG).