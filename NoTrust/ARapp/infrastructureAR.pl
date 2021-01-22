%% AR GATHERING INFRASTRUCTURE (info provided by node provider(s) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% node(nodeId, providerId, listOfSupportedSecurityProperties,listOFSoftwareCapabilities, listOFHWcapabilities (memory, cpu, mhz), Price)
%privateCitzen
node(private1, privateCitizen1, [], [java], (1024,4,2500), [privatePrices1]).
node(private2, privateCitizen2, [data_encryption], [python], (512,2,1500), [privatePrices2]).
%telco
node(centralRouter, telco, [data_encryption, antitampering], [python,cpp,openCV],(3071, 4, 2000), [telcoPrices]).
node(northAntenna, telco, [data_encryption, antitampering], [python, cpp,openCV],(2048, 4, 1500), [telcoPrices]).
node(eastAntenna, telco, [data_encryption], [python], (2048, 4, 1500), [telcoPrices]).
node(westAntenna, telco, [data_encryption], [python,openCV], (2048, 4, 1500),[telcoPrices]).
node(southAntenna, telco, [data_encryption, antitampering], [cpp],(2048, 4, 1500), [telcoPrices]).
%university
node(labServer, unipi, [data_encryption, antitampering], [python,java, cpp,openCV],(4096, 4, 2000), [uniPrices]).
node(officeServer, unipi, [], [python],(1024, 2, 1000), [uniPrices]).
node(switch, unipi, [data_encryption], [python,java],(2048, 4, 1500), [uniPrices]).


%eventGenerator(generatorId, eventType, SourceAndDestNodes)
eventGenerator(userDevice, newGathReq, (centralRouter,centralRouter)).
eventGenerator(userDevice, newMapReq, (centralRouter,centralRouter)).

%service(serviceId, serviceProvider, serviceType, deployedNode)
service(myUserDB, appOp, database, centralRouter).
service(gmap1, bigG, mapService, westAntenna).
%service(gmap2, bigG, mapService, northAntenna).
service(openMaps, openStreets, mapService, officeServer).
service(myGatherService, appOp, gatherService,westAntenna).
%service(publicGatherService, pub_amm, gatherService, southAntenna).

%link(node1, node2, latencyInMs)
link(X,X,0).
link(X,Y,L) :- dif(X,Y), (g_link(X,Y,L);g_link(Y,X,L)).
%centralRouter
g_link(centralRouter, northAntenna, 150).
g_link(centralRouter, eastAntenna, 150).
g_link(centralRouter, westAntenna, 150).
g_link(centralRouter, southAntenna, 150).
g_link(centralRouter, private1, 80).
g_link(centralRouter, private2, 120).
g_link(centralRouter, switch, 100).
g_link(centralRouter, officeServer, 200).
g_link(centralRouter, labServer, 130).
%northAntenna
g_link(northAntenna, eastAntenna, 200).
g_link(northAntenna, westAntenna, 200).
g_link(northAntenna, southAntenna, 300).
g_link(northAntenna, private1, 100).
g_link(northAntenna, private2, 110).
g_link(northAntenna, switch, 90).
g_link(northAntenna, officeServer, 300).
g_link(northAntenna, labServer, 200).
%eastAntenna
g_link(eastAntenna, westAntenna, 300).
g_link(eastAntenna, southAntenna, 200).
g_link(eastAntenna, private1, 280).
g_link(eastAntenna, private2, 40).
g_link(eastAntenna, switch, 170).
g_link(eastAntenna, officeServer, 320).
g_link(eastAntenna, labServer, 250).
%westAntenna
g_link(westAntenna, southAntenna, 200).
g_link(westAntenna, private1, 50).
g_link(westAntenna, private2, 200).
g_link(westAntenna, switch, 50).
g_link(westAntenna, officeServer, 50).
g_link(westAntenna, labServer, 150).
%southAntenna
g_link(southAntenna, private1, 100).
g_link(southAntenna, private2, 160).
g_link(southAntenna, switch, 125).
g_link(southAntenna, officeServer, 50).
g_link(southAntenna, labServer, 100).
%private1
g_link(private1, private2, 120).
g_link(private1, switch, 50).
g_link(private1, officeServer, 120).
g_link(private1, labServer, 80).
%private2
g_link(private2, switch, 130).
g_link(private2, officeServer, 200).
g_link(private2, labServer, 180).
%switch
g_link(switch, officeServer, 80).
g_link(switch, labServer, 60).
%officeServer
g_link(officeServer, labServer, 90).