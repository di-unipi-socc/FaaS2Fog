:- use_module(library(lists)).
%:- consult('infrastructure').
%:- consult('application').
%:- consult('trust').
:- consult('./ARapp/infrastructureAR').
:- consult('./ARapp/applicationAR').
:- consult('./ARapp/trust').

placeChain(ChainId, Placement, Billing):-
    functionChain(ChainId, AppOp, (GeneratorId,EventTrigger,TriggerTypes), FunctionList, LatencyList),
	eventGenerator(GeneratorId,EventTrigger,SourceDestNodes),
    typePropagation(TriggerTypes, FunctionList, TypedFunctionList),
    mapping(AppOp, TypedFunctionList, LatencyList,SourceDestNodes, Placement),
    determineCosts(Placement, Billing).

determineCosts([],[]).
determineCosts([on(_,N,_)|Plc], [(N,C)|Costs]):-
	determineCosts(Plc,Costs),
	\+(member((N,_,_),Costs)),
	node(N,_,_,_,_,C).

typePropagation(_,[], []). 
typePropagation(InTypes, [(F,FServices)|FunctionList], [(F,FServices,FType)|TypedFunctionList]) :-
    functionBehaviour(F, InTypes, InteractionsTypes, OutTypes),
    append(InTypes, InteractionsTypes, TempTypes), append(TempTypes, OutTypes, AllTypes),
    sort(AllTypes, AllTypesSorted),
    highestType(AllTypesSorted,FType),
    typePropagation(OutTypes, FunctionList, TypedFunctionList).


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


mapping(AppOp, TypedFunctionList, LatencyList, (SourceNode,DestNode), Placement) :-
	mapping(AppOp, TypedFunctionList, LatencyList, SourceNode,DestNode,[], _, Placement).

mapping(AppOp, [(F,FServices,FType)], [Lat1,Lat2|[]],PreviousNode,LastNode,OldAllocHW, AllocHW,[on(F,N,FServicesBinding)]):-
	node(N, Nop, _, SWCaps, HWCaps, _),
	checkLatPreviousNode(N,PreviousNode,Lat1),
	functionReqs(F,SWReqs, HWReqs, FServicesReqs),
    swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldAllocHW, AllocHW),
    compatibleType(FType,N),
    bindServices(AppOp, N, FServices, FType, FServicesReqs, FServicesBinding),
	trustRadius(D),trusts(AppOp,Nop,D),
	checkLatPreviousNode(LastNode,N,Lat2).

mapping(AppOp, [(F,FServices,FType)|FunctionList], [Latency|LatencyList], PreviousNode,LastNode,OldAllocHW, NewAllocHW, [on(F,N,FServicesBinding)|P]):-
    node(N, Nop, _, SWCaps, HWCaps, _),
	checkLatPreviousNode(N,PreviousNode,Latency),
	functionReqs(F,SWReqs, HWReqs, FServicesReqs),
    swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldAllocHW, AllocHW),
    compatibleType(FType,N),
    bindServices(AppOp, N, FServices, FType, FServicesReqs, FServicesBinding),
	trustRadius(D),trusts(AppOp,Nop,D),
    mapping(AppOp, FunctionList, LatencyList, N, LastNode, AllocHW, NewAllocHW, P).

checkLatPreviousNode(N1,N2, ReqLatency):-
	link(N1, N2, Latency), Latency =< ReqLatency.

swReqsOK(SWReqs, SWCaps):- subset(SWReqs, SWCaps).

hwReqsOK((RAMReq,VCPUsReq,CPUReq), (RAMCap,VCPUsCap,CPUCap), N, [], [(N,(RAMReq,VCPUsReq,CPUReq))]) :-
	RAMCap >= RAMReq, CPUCap >= CPUReq, VCPUsCap >= VCPUsReq.
hwReqsOK((RAMReq,VCPUsReq,CPUReq), (RAMCap,VCPUsCap,CPUCap), N, [(N,(AllocRAM,AllocVCPUs,AllocCPU))|L], [(N,(NewAllocRAM,NewAllocVCPUs,NewAllocCPU))|L]) :-
	NewAllocRAM is AllocRAM + RAMReq, RAMCap >= NewAllocRAM,
	NewAllocVCPUs is AllocVCPUs + VCPUsReq, VCPUsCap >= NewAllocVCPUs,
	NewAllocCPU is AllocCPU + CPUReq, CPUCap >= NewAllocCPU.
hwReqsOK(HWReqs, HWCaps, N, [(N1,AllocHW)|L], [(N1,AllocHW)|NewL]) :-
	N \== N1, hwReqsOK(HWReqs, HWCaps, N, L, NewL).

compatibleType(Ftype,N) :- assignNodeLabel(N, Ntype), compatible(Ftype, Ntype).
compatible(T,T).
compatible(T1,T2) :- dif(T1,T2), lattice_higherThan(T2, T1).

bindServices(_,_,[],_,[],[]).
bindServices(AppOp,Node,[SId|SerList], FLabel, [(ServiceType,ReqLatency)|ReqList],[(ServiceType,SId,ServiceNode)|Binding]):-
	service(SId, ServiceProvider, ServiceType, ServiceNode),
	assignServiceLabel(SId,ServiceType, ServiceLabel), compatible(FLabel,ServiceLabel),
	assignNodeLabel(ServiceNode, SerNodeLabel), compatible(ServiceLabel, SerNodeLabel),
	link(Node, ServiceNode, Latency), Latency =< ReqLatency,
	node(ServiceNode, ServiceNodeProvider, _,_,_,_),
	trustRadius(D), trusts(AppOp,ServiceProvider,D), trusts(AppOp,ServiceNodeProvider,D),
	bindServices(AppOp,Node,SerList,FLabel,ReqList, Binding).