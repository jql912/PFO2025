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



*********** DEFINE WHAT IS NEEDED FOR DOWNSIDE REGRET MODEL *********

SCALARS
    DR_target       "Downside regret target"
;
DR_target = (1+0.02)**(1/13)-1;


Parameters
    avg_ret(i) "Average return across scenarios"
;

avg_ret(i) = 1/card(q) * sum(q, Return_bs_3(i,q));

display avg_ret;


******* RUNNING DOWNSIDE REGRET ALG. *********


positive variables
    x(i)
    Downside_regret(q)
;
*only invest in benchmark
x.fx("Jyske Portefølje Balanceret Akk KL") = 1;


variables
    Exp_Return 'Expected portfolio return'
    z_min_DR
;

equations
    BudgetCond          'We invest exactly 100%'
    ExpRetDef           'calculate expected return for portfolio'
    Downside_def(q)     'define downside regrets'
    ObjMinDR             'the object functuion minimizing downside regret'
;

BudgetCond ..           SUM(i, x(i)) =E= 1;

ExpRetDef ..            Exp_Return =E= sum(i, avg_ret(i)*x(i));

Downside_def(q) ..      Downside_regret(q) =g= (1+DR_target) - sum(i, (1+Return_bs_3(i,q))*x(i));

ObjMinDR ..             z_min_DR =e= sum(q, 1/1000*Downside_regret(q));

MODEL MinDR /BudgetCond, ExpRetDef, Downside_def, ObjMinDR/;

Option decimals=6;

SOLVE MinDR minimizing z_min_DR USING LP;

display
    x.l,
    Exp_Return.l,
    Downside_regret.l,
    z_min_DR.l
;