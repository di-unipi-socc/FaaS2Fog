%% INFRASTRUCTURE (info provided by node provider(s) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% node(nodeId, providerId, listOfSupportedSecurityProperties,listOFSoftwareCapabilities, listOFHWcapabilities, UnitCost)
%provider1
node(edge1, provider1, [data_encryption], [python,javascript,java], [(ram, 2048)], 1).
node(edge2, provider1, [data_encryption, antitampering],    [python, java], [(ram, 800)], 2).
%provider2
%node(edge3, provider2, [antitampering],                     [javascript, java]).
%node(edge4, provider2, [data_encryption,antitampering],     [python,java]).
%provider3
node(edge5, provider3, [data_encryption,antitampering],     [java], [(ram, 4096)], 1).
node(edge6, provider3, [wireless_security,antitampering],   [java],[(ram, 8192)], 2).


%eventSource(sourceId, sourceType, listOfEvents)
eventSource(thermometer42, thermometer, [(tempTooHigh,2), (tempToolow,2), (alarm,1)]).


%service(serviceId, serviceProvider, serviceType, deployedNode)
service(db1, aws_EU, database, edge2).
service(db2, aws_EU, database, edge5).