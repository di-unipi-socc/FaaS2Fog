%%% trustable relations declared by appOp
trusts(appOp, provider1).
trusts(appOp, provider3).
trusts(appOp, ispOp).

%%% trust relations declared by provider1
%.9::trusts(provider1, provider2).
trusts(provider1, provider3).

%%% trust relation declared by provider2
%.8::trusts(provider2, provider1).

%%% trust relation declared by provider3
trusts(provider3, provider1).

%%% trust relations declared by ispOp  
trusts(ispOp, provider1).
%.8::trusts(ispOp, provider2).
