:- dynamic eventInstance/4.

% eventInstance(eventInstanceID, eventSourceId, eventName, parameters)
eventInstance(e1,thermometer42, tempTooHigh, [top, low]).
eventInstance(e2,thermometer42, alarm, [low]).
