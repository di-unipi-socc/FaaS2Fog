% eventInstance(eventInstanceID, eventSourceId, eventName, parameters)
eventInstance(e1,thermometer42, tempTooHigh, [top, low]).
eventInstance(e2,thermometer42, alarm, [low]).
%(avgExecTime(FId,OpId, milliseconds), 
avgExecTime(f1,appOp,500)