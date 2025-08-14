SET
    i
    q
;

PARAMETERS
    Return(i,q)
;

$GDXIN bootstrap_data_O6
$LOAD i, q, Return = Return_bs_3
$GDXIN


*********** DEFINE WHAT IS NEEDED FOR DOWNSIDE REGRET MODEL *********

SCALARS
    alpha      
    Bench_ret_4w    "Average benchmark return over 4-week horizon (found in Opgave 6 benchmark calculations.gms)"
    Bench_CVaR_4w   "Conditional value at risk for 4 week returns for benchmark returns  (found in Opgave 6 benchmark calculations.gms)"
;
alpha = 0.95;
Bench_ret_4w = (1+0.0591)**(1/13)-1;
Bench_CVaR_4w = 1.04324435;

Parameters
    avg_ret(i) "Average return across scenarios"
;

avg_ret(i) = 1/card(q) * sum(q, Return(i,q));



******* RUNNING CVaR ALG. *********


positive variables
    x(i)
    VaRDev(q)
;

variables
    Loss(q)
    VaR
    CVar
    Exp_Return
;
equation
    BudgetDef
    ReturnDef
    ReturnCond
    CVaRCond
    LossDef(q)
    VarDevDef(q)
    CVarDef
;
scalar
    pr
;
pr = 1/1000;

BudgetDef ..    sum(i, x(i)) =E= 1;
ReturnDef ..    Exp_Return =e= sum(i, avg_ret(i)*x(i));
ReturnCond ..   Exp_Return =g= Bench_ret_4w*1;
LossDef(q) ..   Loss(q) =E= 1 - sum(i, Return(i,q)*x(i));
VarDevDef(q) .. VarDev(q) =g= Loss(q) - VaR;
CVarDef ..      CVaR =E= VaR + (sum(q, pr*VarDev(q)))/(1-alpha);
CVarCond ..     CVaR =l= Bench_CVaR_4w;

MODEL MinCVaR_givenRet /BudgetDef, ReturnDef, ReturnCond, LossDef, VarDevDef, CVaRDef/;
MODEL MaxRet_givenCVaR /BudgetDef, ReturnDef, LossDef, VarDevDef, CVaRDef, CVaRCond/;

option decimals=8;



********** EXPORT PORTFOLIOS FOR PROBLEM 6.5 *********

PARAMETERS
    x_min_cvar(i)
    x_max_ret(i)
;

*Solve problem 6.3
SOLVE MinCVaR_givenRet MINIMIZING CVaR USING LP;
display
    Var.l,
    Bench_CVaR_4w,
    CVaR.l,
    Bench_ret_4w,
    Exp_Return.l
;
x_min_cvar(i) = x.l(i);


*Solve problem 6.4
SOLVE MaxRet_givenCVaR maximizing Exp_Return USING LP;

display
    Var.l,
    Bench_CVaR_4w,
    CVaR.l,
    Bench_ret_4w,
    Exp_Return.l
;

x_max_ret(i) = x.l(i);

execute_unload 'Data_6.5.gdx', i, x_min_cvar, x_max_ret;

$exit
