%%AR GATHERING APPLICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% functionReqs(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
functionReqs(fLogin, [py3], (1024, 2, 500), [(userDB, 13)]).
functionReqs(fShop, [py3, numPy], (2048, 4, 1500), []).
functionReqs(fGeo, [js], (1024, 2, 400), [(maps, 30)]).
functionReqs(fGather, [js], (1024, 2, 500), [(shops, 12)]).
functionReqs(fAR, [py3, numPy], (2048, 4, 1200), []).

%functionBehaviour(functionId, listOfInputs, listOf(serviceReq, TypeParam), listOfOutputs)
functionBehaviour(fLogin, [U, Sc, G, Se],[U], [Sc, G, Se]).
functionBehaviour(fShop,[Sc, G, Se],[], [Sc, low, G, Se]).
functionBehaviour(fGeo, [Sc, Sb, G, Se], [Sb, G, Se, low], [Sc, Sb, low]).
functionBehaviour(fGather, [Sc, Sb, Sh], [Sh, medium], [Sc, Draw]):- maxType(Sb, Sh,Draw).
functionBehaviour(fAR, [Sc,Draw], [], [ScAr]):- maxType(Sc, Draw, ScAr).

%functionChain(functionChainId, operatorId, triggeringEvent(eventSource, eventType, inputParameters), (latency from source, dest)
%               listOfFunctions(functionId(listOfServiceInstances)),
%               listOfIntraFunctionLatencies).
functionChain(
  chainGath, appOp,(userDevice, [top,low,low, low]), %[userInfo, screen, geo, sensors], latency
  [(fLogin,[myUsers]),(fShop,[]),(fGeo,[_]),(fGather, [myShop]), (fAR, [])],
  [12,10,13,10,15]
).

% lattice of security types
g_lattice_higherThan(top, medium).
g_lattice_higherThan(medium, low).


% node labeling
assignNodeLabel(NodeId, top)    :- node(NodeId,_,SecCaps,_,_), member(antiTamp, SecCaps), member(pubKeyE, SecCaps).
assignNodeLabel(NodeId, medium) :- node(NodeId,_,SecCaps,_,_), \+(member(antiTamp, SecCaps)), member(pubKeyE, SecCaps).
assignNodeLabel(NodeId, low)    :- node(NodeId,_,SecCaps,_,_), \+(member(pubKeyE, SecCaps)).

%service labeling
assignServiceLabel(SId, _, top) :- service(SId, appOp, _, _).
%assignServiceLabel(SId, T, medium) :- service(SId, appOp, T, _), \+(T == userDB).
assignServiceLabel(SId, maps, medium) :- service(SId, cloudProvider, maps, _).
assignServiceLabel(SId, Type, low) :- 
    service(SId, Provider, Type, _),
    \+(Provider == appOp),
    \+((Provider == cloudProvider, Type == maps)).