%%AR GATHERING APPLICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% function(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
function(fLogin, [python], (1024, 2, 500), 1, [(database, 130)]).
function(fBuildings, [cpp, openCV], (2048, 4, 1500), 1, []).
function(fLocalise, [java], (1024, 2, 400), 1,[(mapService, 100)]).
function(fGather, [python], (1024, 2, 500), 1, [(gatherService, 100)]).
function(fAR, [cpp, openCV], (2048, 4, 1100), 1, []).
function(fMap, [python], (1024, 2, 500), 1, [(mapService, 2000)]).

%functionBehaviour(functionId, listOfInputs, listOf(serviceReq, TypeParam), listOfOutputs)
functionBehaviour(fLogin, [U, Sc, G, Se],[U], [Sc, G, Se]).
functionBehaviour(fBuildings,[Sc, G, Se],[], [Sc, low, G, Se]).
functionBehaviour(fLocalise, [Sc, Sb, G, Se], [Sb, G, Se, low], [Sc, Sb, low]).
functionBehaviour(fGather, [Sc, Sb, Sh], [Sh, medium], [Sc, Sb, Sh, medium]).%:- G is medium.
functionBehaviour(fAR, [Sc, _, _, _], [], [Sc]). %output depends from Sc
functionBehaviour(fMap, [_, _, Sh], [Sh], [low]).

%functionChain(functionChainId, operatorId, triggeringEvent(eventSource, eventType, inputParameters,
%               listOfFunctions(functionId(listOfServiceInstances)),
%               listOfIntraFunctionLatencies).
functionChain(
  chainAR, appOp,(userDevice, newScreen, [top,low,low, low]), %[userInfo, screen, geo, sensors]
  [(fLogin,[myUserDB]),(fBuildings,[]),(fLocalise,[_]),(fGather, [myGatherService]), (fAR, [])],
  [130,130,130,130]
).

functionChain(
  chainMap, appOp,(userDevice, newMapReq, [top,low,low, low]), %[userInfo, screen, geo, sensors]
  [(fLogin,[myUserDB]),(fBuildings,[]),(fLocalise,[_]),(fMap, [_])],
  [500,500,500]
).

% lattice of security types
g_lattice_higherThan(top, medium).
g_lattice_higherThan(medium, low).


% node labeling
assignNodeLabel(NodeId, top)    :- node(NodeId,_,SecCaps,_,_,_), member(antitampering, SecCaps), member(data_encryption, SecCaps).
assignNodeLabel(NodeId, medium) :- node(NodeId,_,SecCaps,_,_,_), \+(member(antitampering, SecCaps)), member(data_encryption, SecCaps).
assignNodeLabel(NodeId, low)    :- node(NodeId,_,SecCaps,_,_,_), \+(member(data_encryption, SecCaps)).

%service labeling
assignServiceLabel(SId, database, top) :- service(SId, appOp, database, _).
assignServiceLabel(SId, T, medium) :- service(SId, appOp, T, _), \+(T == database).
assignServiceLabel(SId, mapService, medium) :- service(SId, bigG, mapService, _).
assignServiceLabel(SId, Type, low) :- 
    service(SId, Provider, Type, _),
    \+(Provider == appOp),
    \+((Provider == bigG, Type == mapService)).