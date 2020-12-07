:- use_module(library(lists)).
:- consult('infrastructure').
:- consult('application').

%query(secFaaS(RESULT)).
%RESULT= placement for event at every request
%[(event1, [(chainID1, PLACEMENT)], [(chainID2, Placement)...]..., [ChainIDK, Placement]...),... (eventn), [(chainIDm, Placement)]...]
% an autonomic placer/orchestrator handles the placement of chain instances 
% defined by the app operator, e.g.:
secFaaS(AllPlacedChains) :- 
	retractall(event(_,_,_,_)),
	consult('newtriggers'),
	findall((Id,SourceId, EventId, Params), eventInstance(Id,SourceId, EventId, Params), Events), % SF: changed 07/12, now retrieves all info
	placeAll(Events, [], AllPlacedChains). %[] is initial Allocated Hardware

%Given a list of EventInstance, place all the chains triggered
placeAll([],_,[]).
placeAll([EventInstance|ListOfEvents], AllocHW, [PlacedByEvent|AllPlacedChains]):-
	placeTriggered(EventInstance, AllocHW, NewAllocHW, PlacedByEvent),
	placeAll(ListOfEvents, NewAllocHW, AllPlacedChains).

%Given an EventInstance, place all the chains it triggers
placeTriggered((EventInstanceId,SourceId, EventId, Params), AllocHW, NewAllocHW, (EventInstanceId,PlacedByEvent)):-
	% SF: changed 07/12, does not retrieve eventInstance again
	eventSource(SourceId, SourceType, _),
	findall(ChainId, functionChainTrigger(ChainId, SourceType, EventId), TriggeredChains),
	placeChains(TriggeredChains, Params, AllocHW, NewAllocHW, PlacedByEvent).

%Given a list of Chains, place every single of them
placeChains([], _,AllocHW,AllocHW,[]):-!.
placeChains([ChainId|ListOfChains], Params, OldAllocHW, NewAllocHW, [PlacedChain|PlacedChains]):-
	placeChain(ChainId, Params, OldAllocHW, AllocHW,PlacedChain),
	placeChains(ListOfChains, Params, AllocHW, NewAllocHW, PlacedChains),!.

%Given a chain, input paramaters types and an allocation, return the new allocation and a placement of the chain
placeChain(ChainId, Params, AllocHW, NewAllocHW, (ChainId,PlacedChain,Cost)):-
    functionTypes(Params, ChainId, TypedFunctions),
    mapping(TypedFunctions, AllocHW, NewAllocHW,PlacedChain,Cost).

functionTypes(Params, ChainId, TypedFunctions) :-
	functionChain(ChainId, ListOfFunctions),
	typePropagation(Params, ListOfFunctions, TypedFunctions).

typePropagation(_, [], []). 
typePropagation(InTypes, [F|Fs], [(F,Type,Interactions)|FTs]) :-
	functionBehaviour(F,InTypes, Interactions, OutTypes),
	typesOfInteractions(Interactions, InteractionsTypes),
	append(InTypes, InteractionsTypes, FirstAppend),
	append(FirstAppend, OutTypes, AllTypes),
	sort(AllTypes,AllTypesSorted),
	highestType(AllTypesSorted,Type),
	typePropagation(OutTypes, Fs, FTs).

%create a list of types from readList or writeList
typesOfInteractions([],[]).
typesOfInteractions([(_,_,Types)|Ints], ListOfTypes):-
	typesOfInteractions(Ints, RecursiveList),
	append(Types, RecursiveList, ListOfTypes).

%find highest type in a list
highestType([T], T).
highestType([T1,T2|Ts], MaxT) :-
	highestType([T2|Ts], MaxTofRest),
	maxType(T1, MaxTofRest, MaxT).

%find_Max(label1,label2,maximumLabel)
maxType(X, X, X):-!. 										    %equal lable
maxType(X, Y, X) :- dif(X,Y), lattice_higherThan(X,Y),!.	        %labels reachable with path from X to Y
maxType(X, Y, Y) :- dif(X,Y), lattice_higherThan(Y,X),!.        	%labels reachable with path from Y to X 
maxType(X, Y, Top) :-										%labels not reachable with path (on different branch) 
	dif(X,Y), \+ lattice_higherThan(X,Y), \+ lattice_higherThan(Y,X),
	lattice_higherThan(Top, X), lattice_higherThan(Top, Y),
	\+ (lattice_higherThan(Top, LowerTop), lattice_higherThan(LowerTop, X), lattice_higherThan(LowerTop, Y)).
lattice_higherThan(X, Y) :- g_lattice_higherThan(X,Y).
lattice_higherThan(X, Y) :- g_lattice_higherThan(X,W), lattice_higherThan(W,Y).


mapping([], AllocHW,AllocHW,[],0).
%(Function, FunctionType, Interactions)
mapping([(F,FT,I)|Fs], OldAllocHW, NewAllocHW, [on(F,N,C)|P],Cost):-
    function(F, _,SWReqs, HWReqs, TimeUnits), % SF: added cost 07/12
	node(N, _, _, SWCaps, HWCaps, UnitCost), % SF: added cost 07/12
	subset(SWReqs, SWCaps), nodeLabelOK(FT, N, NodeLabel),
	servicesOK(I,N,NodeLabel),
	nodeResourceOK(N, OldAllocHW, HWCaps, HWReqs, AllocHW),
	mapping(Fs, AllocHW, NewAllocHW, P, OldCost),
	C is TimeUnits * UnitCost, Cost is OldCost + C. % SF: added cost 07/12

nodeLabelOK(FT, Node, FT):- 
	assignNodeLabel(Node, FT).
nodeLabelOK(FT, Node, NodeLabel):- 
	assignNodeLabel(Node, NodeLabel), dif(FT,NodeLabel), lattice_higherThan(NodeLabel, FT).

servicesOK([],_,_).
servicesOK([(_,ServiceType,InteractionParamTypes)|Interactions], Node, NodeLabel):-
	service(ServiceId, _,ServiceType, Node),
	assignServiceLabel(ServiceId, ServiceType, ServiceLabel),
	(NodeLabel==ServiceLabel;lattice_higherThan(NodeLabel, ServiceLabel)), %check service label is ok with node label
	highestType(InteractionParamTypes, MaxType),
	(MaxType==ServiceLabel;lattice_higherThan(ServiceLabel, MaxType)), %check service label is ok with interaction labels
	(NodeLabel==MaxType ;lattice_higherThan(NodeLabel, MaxType)), %check interaction labels is ok with node label
	servicesOK(Interactions, Node, NodeLabel).

%nodeResourceOK(NodeId, AllocHW, HWCaps, HWReqs,NewAllocHW),
nodeResourceOK(_,AllocHW,_,[],AllocHW).
nodeResourceOK(Node, OldAllocHW, HWCaps,[(Resource, Needed)|HWReqs], NewAllocHW):-
	member((Resource,TotalResources), HWCaps),
	Needed =< TotalResources,
	checkAndAllocate(Node, OldAllocHW, Resource, TotalResources, Needed, AllocHW),
	nodeResourceOK(Node, AllocHW,HWCaps, HWReqs, NewAllocHW).

%search Node and look in its resources list
checkAndAllocate(Node, [], Resource, _, Needed, [(Node,[(Resource, Needed)])]).
checkAndAllocate(Node, [(N,ListOfRes)|OldAllocHW], Resource, TotalResources, Needed,[(N,ListOfRes)|NewAllocHW]):-
	Node\==N, checkAndAllocate(Node, OldAllocHW, Resource, TotalResources, Needed,NewAllocHW).
checkAndAllocate(Node, [(N,ListOfRes)|OldAllocHW], Resource, TotalResources, Needed,[(N,NewListOfR)|OldAllocHW]):-
	Node==N, checkListOfRes(ListOfRes, Resource, TotalResources,Needed, NewListOfR).

%search for a resource in list, check if resources are available and update resources list
checkListOfRes([],Resource, _,Needed, [(Resource, Needed)]).
checkListOfRes([(Res, _)|OldList], Resource, TotalResources,Needed, NewList):-
	Resource\==Res, checkListOfRes(OldList, Resource, TotalResources, Needed, NewList).
checkListOfRes([(Res, Used)|OldList], Resource, TotalResources, Needed, [(Res, NewUsed)|OldList]):-
	Resource==Res, NewUsed is Used + Needed, NewUsed =< TotalResources.
