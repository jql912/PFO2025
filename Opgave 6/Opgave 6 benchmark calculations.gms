Sets
    AssetName           "Name of the asset"
    Date                "Observation dates"
    Asset               "ISIN codes" / DK0060259786 /
;

ALIAS(AssetName, i, j);
ALIAS(Date, t);

PARAMETER
    AssetReturn(t, Asset, i)
    Return(t, i)
;

$GDXIN weekly_returns_2013_2025
$LOAD Date AssetName AssetReturn
$GDXIN

SET
    First7YDates(t)
;

First7YDates(t) = yes$(ord(t)<=364)

PARAMETER
    First7YRet(t,Asset,i)
;

First7YRet(t,Asset,i) = AssetReturn(t,Asset,i)$(First7YDates(t));


Return(t,i) = SUM(Asset, First7YRet(t,Asset,i));


display return


************ BOOTSTRAP ************
Set
    s   /s1*s4/
    q   /q1*q1000/
;

Parameter
    randIndex(s,q)       
    Return_bs(i,s,q)
    Return_bs_3(i,q) 'Three month return';
;

* Generate 250 4 week scenarios
loop(q,
    loop(s,
*       Find random index
        randIndex(s,q) = uniformint(1,364);
    );

    loop(s,
*       Find the corresponding return
        loop(t$(ord(t) = round(randIndex(s,q))),
            Return_bs(i,s,q) = Return(t,i);
        );
    );

*   Multiply the 3 bootstrap values to get the 4 week return
    loop(i,
        Return_bs_3(i,q) = prod(s, (1+Return_bs(i,s,q)))-1;
    );
);

display Return_bs_3


*********** DEFINE WHAT IS NEEDED FOR CVaR MODEL *********

SCALARS
    alpha      
;
alpha = 0.95;


Parameters
    avg_ret(i) "Average return across scenarios"
;

avg_ret(i) = 1/card(q) * sum(q, Return_bs_3(i,q));

display avg_ret;


******* RUNNING CVaR ALG. *********


positive variables
    x(i)
    VaRDev(q)
;
*only invest in benchmark
x.fx("Jyske Portefølje Balanceret Akk KL") = 1;

variables
    Loss(q)
    VaR
    CVar
;
equation
    BudgetDef
    Surplus
    LossDef(q)
    VarDevDef(q)
    CVarDef
;
scalar
    pr
;
pr = 1/1000;

BudgetDef .. sum(i, x(i)) =E= 1;
*Surplus .. sum(i, EP(i)*x(i)) =g= MU_TARGET*Budget;
LossDef(q) .. Loss(q) =E= 1 - sum(i, Return_bs_3(i,q)*x(i));
VarDevDef(q) .. VarDev(q) =g= Loss(q) - VaR;
CVarDef .. CVar =E= VaR + (sum(q, pr*VarDev(q)))/(1-alpha);

MODEL MinCVaR /BudgetDef, LossDef, VarDevDef, CVarDef/;

option decimals=8;
SOLVE MinCVaR MINIMIZING CVaR USING LP;
display
    Var.l,
    CVaR.l,
    x.l,
    avg_ret
;