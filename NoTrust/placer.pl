:- use_module(library(lists)).
:- consult('infrastructure').
:- consult('application').
:- dynamic eventInstance/4.
:- dynamic avgExecTime/3.

%query(secFaaS(RESULT)).
%RESULT= placement for event at every request
%[(event1, [(chainID1, PLACEMENT)], [(chainID2, Placement)...]..., [ChainIDK, Placement]...),... (eventn), [(chainIDm, Placement)]...]
% an autonomic placer/orchestrator handles the placement of chain instances 
% defined by the app operator, e.g.:
/*secFaaS(AllPlacedChains) :- 
	retractall(event(_,_,_,_)),
	consult('monitoring'),
	findall((Id,SourceId, EventId, Params), eventInstance(Id,SourceId, EventId, Params), Events), % SF: changed 07/12, now retrieves all info
	placeAll(Events, [], AllPlacedChains). %[] is initial Allocated Hardware

%Given a list of EventInstance, place all the chains triggered
placeAll([],_,[]).
placeAll([EventInstance|ListOfEvents], AllocHW, [PlacedByEvent|AllPlacedChains]):-
	placeTriggered(EventInstance, AllocHW, NewAllocHW, PlacedByEvent),
	placeAll(ListOfEvents, NewAllocHW, AllPlacedChains).

%Given an EventInstance, place all the chains it triggers
placeTriggered((EventInstanceId,SourceId, EventId, Params), AllocHW, NewAllocHW, (EventInstanceId,PlacedByEvent)):-
	eventSource(SourceId, SourceType, _),
	findall((ChainId,Binding), functionChainTrigger(ChainId, SourceType, EventId,Binding), TriggeredChains),
	placeChains(TriggeredChains, Params, AllocHW, NewAllocHW, PlacedByEvent).

%Given a list of Chains, place every single of them
%%%%%%%%%%%%%%%%%
% BEWARE: CUTS! %
%%%%%%%%%%%%%%%%%
placeChains([], _,AllocHW,AllocHW,[]):-!.
placeChains([(ChainId,Binding)|ListOfChains], Params, OldAllocHW, NewAllocHW, [PlacedChain|PlacedChains]):-
	placeChain(ChainId, Binding, Params, OldAllocHW, AllocHW,PlacedChain),
	placeChains(ListOfChains, Params, AllocHW, NewAllocHW, PlacedChains),!.

%Given a chain, input paramaters types and an allocation, return the new allocation and a placement of the chain
placeChain(ChainId, Binding, Params, AllocHW, NewAllocHW, (ChainId,PlacedChain,Cost)):-
    functionTypes(Params, ChainId, TypedFunctions),
    mapping(TypedFunctions, Binding,AllocHW, NewAllocHW,PlacedChain,Cost).
*/
placeChain(ChainId, PlacedChain, Costs):-
	functionChain(ChainId, (_,_,Params), ListOfFunctions,LatencyList),
	typePropagation(Params, ListOfFunctions, TypedFunctions),
	mapping(TypedFunctions, [], NewAllocHW,PlacedChain),
	checkLatency(PlacedChain, LatencyList),
	printCosts(PlacedChain, Costs).

checkLatency([on(_,_,_)],[]).
checkLatency([on(_,N1,_), on(_,N2,_)|PlaceList],[ReqLat|LatList]):-
	link(N1,N2,Lat),
	Lat=<ReqLat,
	checkLatency([on(_,N2,_)|PlaceList],LatList).

printCosts([],[]).
printCosts([on(_,N,_)|Plc], [(N,C)|Costs]):-
	printCosts(Plc,Costs),
	\+(member((N,_,_),Costs)),
	node(N,_,_,_,_,C).
%functionTypes(ChainId, TypedFunctions) :-
%	functionChain(ChainId, (_,_,Params), ListOfFunctions,_),
%	typePropagation(Params, ListOfFunctions, TypedFunctions).

typePropagation(_,[], []). 
typePropagation(InTypes, [(F,Services)|Fs], [(F,Services,Type)|FTs]) :-
	functionBehaviour(F,InTypes, InteractionsTypes, OutTypes),
	append(InTypes, InteractionsTypes, FirstAppend),
	append(FirstAppend, OutTypes, AllTypes),
	sort(AllTypes,AllTypesSorted),
	highestType(AllTypesSorted,Type),
	typePropagation(OutTypes, Fs, FTs).

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

%mapping(TypedFunction(Function, Services, Label), functionChainId, OldAllocation, NewAllocation, Placement)
mapping([], AllocHW,AllocHW,[]).
mapping([(F,Services,FT)|Fs], OldAllocHW, NewAllocHW, [on(F,N,Bindings)|P]):-
    function(F, _,SWReqs, HWReqs, _, ServiceReqs),
	node(N, _, _, SWCaps, HWCaps,_),
	subset(SWReqs, SWCaps),
	hwReqsOK(HWReqs, HWCaps, N, OldAllocHW,AllocHW),
	assignNodeLabel(N,NodeLabel),
	labelOK(FT,NodeLabel),
	checkServices(N, FT,Services,ServiceReqs,Bindings),
	%link(N, NWinner, LatWinner),
	%service(Sid, _, ServiceType, NWinner),
	%servicesOK(I,N,NodeLabel),
	mapping(Fs, AllocHW, NewAllocHW, P).

% hai appena piazzato F su NFaaS e devi scegliere con che servizio bindare
/*link(NFaaS, NWinner, LWinner),
serviceInstance(SWinner, NWinner),
ReqLat =< LWinner,
\+ (serviceInstance(SId2,N2,...), SId2 \== SWinner, N2 \== NWinner, link(NFaaS, N2, L2), L2 < LWinner).
*/
%first label can be on second label (ex: function on node)
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

%checkServices(Node, listOfServices, ServiceReqs,Bindings)
checkServices(_,_,[],[],[]).
checkServices(Node,FT,[ServiceId|SerList], [(ServiceType,ReqLatency)|ReqList],Bindings):-
	\+(var(ServiceId)),
	service(ServiceId, _, ServiceType, ServiceNode),
	link(Node, ServiceNode, Latency),
	Latency=<ReqLatency,
	checkServices(Node, FT,SerList,ReqList,Bindings).
checkServices(Node,FT,[ServiceId|SerList], [(ServiceType,ReqLatency)|ReqList],[(ServiceType,Sid,ServiceNode)|Bindings]):-
	var(ServiceId),
	service(Sid, _, ServiceType, ServiceNode),
	assignNodeLabel(ServiceNode, SerNodeLabel),
	labelOK(ServiceLabel, SerNodeLabel),
	assignServiceLabel(Sid,ServiceType, ServiceLabel),
	labelOK(FT,ServiceLabel),
	link(Node, ServiceNode, Latency),
	Latency=<ReqLatency,
	checkServices(Node,FT,SerList,ReqList, Bindings).

/*
servicesOK([],_,_).
servicesOK([(_,ServiceType,InteractionParamTypes)|Interactions], Node, NodeLabel):-
	service(ServiceId, _,ServiceType, Node),
	assignServiceLabel(ServiceId, ServiceType, ServiceLabel),
	(NodeLabel==ServiceLabel;lattice_higherThan(NodeLabel, ServiceLabel)), %check service label is ok with node label
	highestType(InteractionParamTypes, MaxType),
	(MaxType==ServiceLabel;lattice_higherThan(ServiceLabel, MaxType)), %check service label is ok with interaction labels
	(NodeLabel==MaxType ;lattice_higherThan(NodeLabel, MaxType)), %check interaction labels is ok with node label
	servicesOK(Interactions, Node, NodeLabel).



%findBindings(ListOfInteractions, ListOfBingings)
findBindings([], []).
findBindings([(ServiceId, ServiceType, SecType)|Interactions],[(ServiceType,Services)|BindingList]):-
	var(ServiceId),
	findall(SId, (service(SId,_,ServiceType, Node),servicesOK2(SId, ServiceType, SecType, Node)), Services),
	findBindings(Interactions, BindingList).
findBindings([(ServiceId, ServiceType, SecType)|Interactions],[(ServiceType,[ServiceId])|BindingList]):-
	nonvar(ServiceId),
	service(SId,_,ServiceType, Node),
	servicesOK2(SId, ServiceType, SecType, Node),
	findBindings(Interactions, BindingList).


servicesOK2(ServiceId, ServiceType,HighestType, Node):-
	assignServiceLabel(ServiceId, ServiceType, ServiceLabel),
	assignNodeLabel(Node, NodeLabel),
	(NodeLabel==ServiceLabel;lattice_higherThan(NodeLabel, ServiceLabel)), %check service label is ok with node label
	(HighestType==ServiceLabel;lattice_higherThan(ServiceLabel, HighestType)), %check service label is ok with interaction labels
	(NodeLabel==HighestType ;lattice_higherThan(NodeLabel, HighestType)). %check interaction labels is ok with node label
*/