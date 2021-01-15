:- use_module(library(lists)).
%:- consult('infrastructure').
%:- consult('application').
%:- consult('trust').
:- consult('./ARapp/infrastructureAR').
:- consult('./ARapp/applicationAR').
:- consult('./ARapp/trust').


placeChain(ChainId, PlacedChain, Costs):-
	functionChain(ChainId, AppOp,(_,_,Params), ListOfFunctions,LatencyList),
	typePropagation(Params, ListOfFunctions, TypedFunctions),
	mapping(AppOp,TypedFunctions, [], NewAllocHW,PlacedChain),
	checkLatency(PlacedChain, LatencyList), % TODO: to be computed on the fly!!!
	determineCosts(PlacedChain, Costs).

determineCosts([],[]).
determineCosts([on(_,N,_)|Plc], [(N,C)|Costs]):-
	determineCosts(Plc,Costs),
	\+(member((N,_,_),Costs)),
	node(N,_,_,_,_,C).

typePropagation(_,[], []). 
typePropagation(InTypes, [(F,Services)|Fs], [(F,Services,Type)|FLabels]) :-
	functionBehaviour(F,InTypes, InteractionsTypes, OutTypes),
	append(InTypes, InteractionsTypes, FirstAppend),
	append(FirstAppend, OutTypes, AllTypes),
	sort(AllTypes,AllTypesSorted),
	highestType(AllTypesSorted,Type),
	typePropagation(OutTypes, Fs, FLabels).

%find highest type in a list
highestType([T], T).
highestType([T1,T2|Ts], MaxT) :-
	highestType([T2|Ts], MaxTofRest),
	maxType(T1, MaxTofRest, MaxT).

%find_Max(label1,label2,maximumLabel)
maxType(X, X, X). %:- !.								    %equal lable
maxType(X, Y, X) :- dif(X,Y), lattice_higherThan(X,Y). %,!.	      %labels reachable with path from X to Y
maxType(X, Y, Y) :- dif(X,Y), lattice_higherThan(Y,X).%,!.        	%labels reachable with path from Y to X 
maxType(X, Y, Top) :-										%labels not reachable with path (on different branch) 
	dif(X,Y), \+ lattice_higherThan(X,Y), \+ lattice_higherThan(Y,X),
	lattice_higherThan(Top, X), lattice_higherThan(Top, Y),
	\+ (lattice_higherThan(Top, LowerTop), lattice_higherThan(LowerTop, X), lattice_higherThan(LowerTop, Y)).
lattice_higherThan(X, Y) :- g_lattice_higherThan(X,Y).
lattice_higherThan(X, Y) :- g_lattice_higherThan(X,W), lattice_higherThan(W,Y).

%mapping(TypedFunction(Function, Services, Label), functionChainId, OldAllocation, NewAllocation, Placement)
mapping(AppOp,[], AllocHW,AllocHW,[]).
mapping(AppOp,[(F,PartialBinding,FLabel)|Fs], OldAllocHW, NewAllocHW, [on(F,N,Binding)|P]):-
    function(F,SWReqs, HWReqs, _, ServiceReqs),
	node(N, Nop, _, SWCaps, HWCaps,_),
	subset(SWReqs, SWCaps),
	hwReqsOK(HWReqs, HWCaps, N, OldAllocHW,AllocHW),
	assignNodeLabel(N,NodeLabel), labelOK(FLabel,NodeLabel),
	bindServices(AppOp,N,FLabel,PartialBinding,ServiceReqs,Binding),
	trusts2(AppOp,Nop,3),
	mapping(AppOp,Fs, AllocHW, NewAllocHW, P).

bindServices(_,_,_,[],[],[]).
bindServices(AppOp,Node,FLabel,[SId|SerList], [(ServiceType,ReqLatency)|ReqList],[(ServiceType,SId,ServiceNode)|Binding]):-
	service(SId, SProv, ServiceType, ServiceNode),
	assignServiceLabel(SId,ServiceType, ServiceLabel), labelOK(FLabel,ServiceLabel),
	assignNodeLabel(ServiceNode, SerNodeLabel), labelOK(ServiceLabel, SerNodeLabel),
	link(Node, ServiceNode, Latency), Latency =< ReqLatency,
	trusts2(AppOp,SProv,3),
	bindServices(AppOp,Node,FLabel,SerList,ReqList, Binding).

labelOK(SameLabel, SameLabel).
labelOK(FunctionType, Label):- 
	dif(FunctionType, Label), lattice_higherThan(Label, FunctionType).

hwReqsOK((RAMReq,VCPUsReq,CPUReq), (RAMCap,VCPUsCap,CPUCap), N, [], [(N,(RAMReq,VCPUsReq,CPUReq))]) :-
	RAMCap >= RAMReq, CPUCap >= CPUReq, VCPUsCap >= VCPUsReq.
hwReqsOK((RAMReq,VCPUsReq,CPUReq), (RAMCap,VCPUsCap,CPUCap), N, [(N,(AllocRAM,AllocVCPUs,AllocCPU))|L], [(N,(NewAllocRAM,NewAllocVCPUs,NewAllocCPU))|L]) :-
	NewAllocRAM is AllocRAM + RAMReq, RAMCap >= NewAllocRAM,
	NewAllocVCPUs is AllocVCPUs + VCPUsReq, VCPUsCap >= NewAllocVCPUs,
	NewAllocCPU is AllocCPU + CPUReq, CPUCap >= NewAllocCPU.
hwReqsOK(HWReqs, HWCaps, N, [(N1,AllocHW)|L], [(N1,AllocHW)|NewL]) :-
	N \== N1, hwReqsOK(HWReqs, HWCaps, N, L, NewL).

checkLatency([on(_,_,_)],[]).
checkLatency([on(_,N1,_), on(_,N2,_)|PlaceList],[ReqLat|LatList]):-
	link(N1,N2,Lat),
	Lat=<ReqLat,
	checkLatency([on(_,N2,_)|PlaceList],LatList).