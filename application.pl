%% APPLICATION (defined by operator) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function(functionId, operatorId, listOfSoftwareRequirements)
function(f1, appOp, [java],[(ram, 512)]).
function(f2, appOp, [python], [(ram, 256)]).

%functionBehaviour(functionId, listOfInputs, listOfInteractions, listOfOutputs)
%interaction (interactionType, serviceType, ListofParamTypes)
%interactionType: read ; write
%inputs are mutual exclusive
%f1
functionBehaviour(f1, [top,top],[(read,database,[top])], [top]).
functionBehaviour(f1, [top,low],[(read,database,[top])], [top]).
functionBehaviour(f1, [low,_],[], [medium]).
%f2
functionBehaviour(f2, [X],[],[X]).

%functionChain(functionChainId, listOfFunctions).
functionChain(c1,[f1,f2]).
functionChain(c2, [f1]).
functionChain(c3, [f2]).
%functionChainTrigger(functionChainId, eventGeneratorType, eventName
functionChainTrigger(c1, thermometer, tempTooHigh).
functionChainTrigger(c2, thermometer, tempTooHigh).
functionChainTrigger(c3, thermometer, alarm).

% lattice of security types
g_lattice_higherThan(top, high).
g_lattice_higherThan(high, medium).
g_lattice_higherThan(medium, low).


% node labeling
assignNodeLabel(NodeId, top)    :- node(NodeId,_,SecCaps,_,_), member(antitampering, SecCaps), member(data_encryption, SecCaps).
assignNodeLabel(NodeId, medium) :- node(NodeId,_,SecCaps,_,_), \+(member(antitampering, SecCaps)), member(data_encryption, SecCaps).
assignNodeLabel(NodeId, low)    :- node(NodeId,_,SecCaps,_,_), \+(member(data_encryption, SecCaps)).

%service labeling
assignServiceLabel(SId, database, top) :- service(SId, aws_EU, database, _).
assignServiceLabel(SId, database, medium) :- service(SId, aws_US, database, _).
assignServiceLable(SId, database, low) :- service(SId, ServiceProvider, database, _), \+ (ServiceProvider = aws_EU, ServiceProvider = aws_US).

