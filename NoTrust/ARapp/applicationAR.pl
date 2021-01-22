%%AR GATHERING APPLICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% functionReqs(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
functionReqs(fLogin, [python], (1024, 2, 500), [(database, 130)]).
functionReqs(fBuildings, [cpp, openCV], (2048, 4, 1500), []).
functionReqs(fLocalise, [java], (1024, 2, 400), [(mapService, 100)]).
functionReqs(fGather, [python], (1024, 2, 500), [(gatherService, 100)]).
functionReqs(fAR, [cpp, openCV], (2048, 4, 1100), []).
functionReqs(fNav, [python], (1024, 2, 500), [(mapService, 100)]).

%functionBehaviour(functionId, listOfInputs, listOf(serviceReq, TypeParam), listOfOutputs)
functionBehaviour(fLogin, [U, Sc, G, Se],[U], [Sc, G, Se]).
functionBehaviour(fBuildings,[Sc, G, Se],[], [Sc, low, G, Se]).
functionBehaviour(fLocalise, [Sc, Sb, G, Se], [Sb, G, Se, low], [Sc, Sb, low]).
functionBehaviour(fGather, [Sc, _, Sh], [Sh, medium], [Sc, medium]).
functionBehaviour(fAR, [Sc,Infos], [], [ScAr]):- maxType(Sc, Infos, ScAr).
functionBehaviour(fNav, [Sc, G, Se], [G, low], [Sc,Draw]):- maxType(G,Se,Draw).

%functionChain(functionChainId, operatorId, triggeringEvent(eventSource, eventType, inputParameters), (latency from source, dest)
%               listOfFunctions(functionId(listOfServiceInstances)),
%               listOfIntraFunctionLatencies).
functionChain(
  chainGath, appOp,(userDevice, newGathReq, [top,low,low, low]), %[userInfo, screen, geo, sensors], latency
  [(fLogin,[myUserDB]),(fBuildings,[]),(fLocalise,[_]),(fGather, [myGatherService]), (fAR, [])],
  [inf,130,130,130,130,inf]
).

functionChain(
  chainMap, appOp,(userDevice, newMapReq, [top,low,low, low]), %[userInfo, screen, geo, sensors]
  [(fLogin,[myUserDB]),(fNav,[_]),(fAR, [])],
  [inf,100,100,inf]
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