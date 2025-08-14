SET
    i
    q
;

PARAMETERS
    Return(i,q)
;

$GDXIN bootstrap_data_O5
$LOAD i, q, Return = Return_bs_3
$GDXIN


*********** DEFINE WHAT IS NEEDED FOR DOWNSIDE REGRET MODEL *********

SCALARS
    DR_target       "Downside regret target"
    Bench_ret_4w    "Average benchmark return over 4-week horizon (found in Opgave 5 benchmark calculations.gms)"
    Bench_DR_4w     "Expected downside regret for benchmark portfolio (found in Opgave 5 benchmark calculations.gms)"
;
DR_target = (1+0.02)**(1/13)-1;
Bench_ret_4w = (1+0.0591)**(1/13)-1;
Bench_DR_4w = 0.006451;

Parameters
    avg_ret(i) "Average return across scenarios"
;

avg_ret(i) = 1/card(q) * sum(q, Return(i,q));


******* RUNNING DOWNSIDE REGRET ALG. *********


positive variables
    x(i)
    Downside_regret(q)
;

variables
    Exp_Return 'Expected portfolio return'
    Downside_regret_port
;

equations
    BudgetCond          'We invest exactly 100%'
    Return_cond         'Expected return larger or equal to benchmark'
    ExpRetDef           'calculate expected return for portfolio'
    Downside_def(q)     'define downside regrets'
    Downside_cond(q)    'Downside regret smaller than benchmark downside regret'
    Downside_reg_port   'the downside regret for the entire portfolio'
;

BudgetCond ..           SUM(i, x(i)) =E= 1;

ExpRetDef ..            Exp_Return =E= sum(i, avg_ret(i)*x(i));

Return_cond ..          Exp_Return =g= Bench_ret_4w;

Downside_def(q) ..      Downside_regret(q) =g= (1+DR_target) - sum(i, (1+Return(i,q))*x(i));

Downside_cond(q) ..     Downside_regret_port =l= Bench_DR_4w;

Downside_reg_port ..    Downside_regret_port =e= sum(q, 1/1000*Downside_regret(q));

MODEL MinDR_given_return /BudgetCond, ExpRetDef, Return_cond, Downside_def, Downside_reg_port/;
MODEL MaxReturn_given_DR /BudgetCond, ExpRetDef, Downside_def, Downside_cond, Downside_reg_port/;

Option decimals=6;



********** EXPORT PORTFOLIOS FOR PROBLEM 5.5 *********

PARAMETERS
    x_min_dr(i)
    x_max_ret(i)
;

*Solve problem 5.3
SOLVE MinDR_given_return minimizing Downside_regret_port USING LP;

display
    x.l,
    Exp_Return.l,
    Bench_ret_4w,
    Downside_regret.l,
    Downside_regret_port.l
;

x_min_dr(i) = x.l(i);

*Solve problem 5.4
SOLVE MaxReturn_given_DR maximizing Exp_Return USING LP;

display
    x.l,
    Exp_Return.l,
    Bench_ret_4w,
    Downside_regret.l,
    Downside_regret_port.l
;

x_max_ret(i) = x.l(i);



execute_unload 'Data_5.5.gdx', i, x_min_dr, x_max_ret;

$exit
