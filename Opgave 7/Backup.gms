***** LOAD DATA *****

Sets
    AssetName           "Name of the asset"
    Date                "Observation dates"
    Asset               "ISIN codes" /
        DK0016109614,
        DK0060822468,
        LU0376446257,
        LU0376447149,
        LU0055631609,
        LU0788108826,
        LU0724618789,
        LU0252968424,
        LU0827889303,
        LU0827889139,
        LU0368252358,
        LU0320298689,
        LU1893597564,
        DK0060791564,
        DK0016303936,
        DK0060791218,
        DK0060244242,
        DK0060786564,
        LU0249702647,
        DK0060787109,
        DK0060789667,
        DK0016205685,
        DK0016023229,
        DK0060178275,
        DK0016205255,
        IE00B6R52036,
        DK0060185726,
        DK0060005098,
        DK0060005254,
        DK0060012466,
        DK0061542719,
        DK0016202740,
        DK0016306798,
        DK0061543600,
        DK0060014678,
        DK0061544921,
        DK0060158160,
        DK0060356392,
        DK0060761492,
        DK0061553245,
        DK0061150984
    /
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

Return(t,i) = SUM(Asset, AssetReturn(t,Asset,i));


*Load initial weights for CVaR-minimizing model
parameters
    x_init(i)
;

$GDXIN Initial_inv_strat_CVaR.gdx
$LOAD x_init = x_min_cvar
$GDXIN

display x_init



************************************************************************ ROLLING FORWARD ONE WEEK ************************************************************************

Set
*number of weeks to take per bootstrap
    s   /s1*s4/
*number of bootstraps
    q   /q1*q10/
*number of 4-week periods we roll forward on
    loop_no /loop1*loop73/
;

alias(loop_no, loop_next);


******* LOAD BENCHMARK RETURNS ********

*Rolling expected 4-week benchmark return (starting from 01-01-2020)
Parameter
    bench_ret_4w(loop_no)
;

$GDXIN Benchmark_Opg7.gdx
$LOAD Bench_ret_4w
$GDXIN

Parameters
    Dates(t)
    Dates_for_actual_return(t)
    randIndex(s,q)       
    Return_bs(i,s,q)
    Return_4(i,q) '4 week return'
    avg_ret(i) "Average 4 week return across new training period bootstrap scenarios"
    min_ret(i) "Worst 4 week return for each asset across training period bs"
    max_ret(i) "Best 4 week return for each asset across training period bs"
    x_start(i,loop_no)
    x_start_cur(i)
    x_across_loops(i,loop_no)
    Port_value(loop_no)
    Port_value_avg(loop_no)
    Port_value_min(loop_no)
    Port_value_max(loop_no)
    Port_value_after_cost(loop_no)
    Date_vec(loop_no)
;

    
scalars
    n_week          'Number of weeks in each roll (actually minimum, since time period becomes small when approaching 2025)'
    loop_step   'Ordinality of date for starting date in training data roll'
    cur_loop_no
    date_start
    date_end
    alpha
;

* stuff for the equation system

positive variables
    x(i)
    VaRDev(q)
;

variables
    Loss(q)
    VaR
    CVar
    Exp_Return
    Cost
    diff(i)         'Used for absolute value in cost function'
;
equation
    BudgetDef
    ReturnDef
    ReturnCond
    InitPortValueDef
    CostDef
    LossDef(q)
    VarDevDef(q)
    CVarDef
    diff_pos(i)
    diff_neg(i)
;
scalar
    pr
    InitPortValue
    Bench_ret
;

pr = 1/card(q);
n_week = 364;
alpha = 0.95;

*initialise the starting portfolios
x_start(i,"loop1") = x_init(i);
Port_value_avg("loop1") = 1;
Port_value_min("loop1") = 1;
Port_value_max("loop1") = 1;


* EQUATIONS FOR MODEL

diff_pos(i) ..  diff(i) =g= x(i) - x_start_cur(i);

diff_neg(i) ..  diff(i) =g= -x(i) + x_start_cur(i);

CostDef ..      cost =e= sum(i, diff(i)) * 0.001;

BudgetDef ..    sum(i, x(i)) =E= InitPortValue - cost;

ReturnDef ..    Exp_Return =e= sum(i, avg_ret(i)*x(i));

ReturnCond ..   Exp_Return =g= Bench_ret;

LossDef(q) ..   Loss(q) =E= InitPortValue - sum(i, Return_4(i,q)*x(i));

VarDevDef(q) .. VarDev(q) =g= Loss(q) - VaR;

CVarDef ..      CVaR =E= VaR + (sum(q, pr*VarDev(q)))/(1-alpha);


MODEL MinCVaR_Opg7 /diff_pos, diff_neg, CostDef, BudgetDef, ReturnDef, ReturnCond, LossDef, VarDevDef, CVaRDef/;


*************************** START LOOP ***********************************

loop(loop_no,

    cur_loop_no = ord(loop_no);
    
    date_start = cur_loop_no*4+1;
    date_end = n_week + cur_loop_no*4;
    

*   ********** DEFINE RELEVANT DATES *******

*   Find relevant dates
    Dates(t) = yes$(ord(t)<=date_end and ord(t)>=date_start);
    
*   Dates for the realised returns
    Dates_for_actual_return(t) = yes$(ord(t)<=date_start + 363 and ord(t)>=date_start + 360);
    display dates, Dates_for_actual_return;
    

    
*   ************ BOOTSTRAP 4 WEEK RETURNS OVER NEW TRAINING PERIOD ************

    
*   Generate 10 4 week scenarios
    loop(q,
        loop(s,
*           Find random index
            randIndex(s,q) = uniformint(date_start,date_end);
        );
    
        loop(s,
*           Find the corresponding return
            loop(t$(ord(t) = round(randIndex(s,q))),
                Return_bs(i,s,q) = Return(t,i);
            );
        );
    
*       Multiply the bootstrap values to get the 4 week return
        loop(i,
            Return_4(i,q) = prod(s, (1+Return_bs(i,s,q)))-1;
        );
    );
    
    
*   *********** DEFINE WHAT IS NEEDED FOR MODEL *********
    
    
    x_start_cur(i) = x_start(i,loop_no);
    
    Bench_ret = Bench_ret_4w(loop_no);
    
    avg_ret(i) = 1/card(q) * sum(q, Return_4(i,q));
    
    min_ret(i) = smin(q, Return_4(i,q));
    
    max_ret(i) = smax(q, Return_4(i,q));
    
    display date_end, Return_4, avg_ret, min_ret, max_ret;
    
*   Define Model
    
*   InitPortvalue is previous portfolio times actual return of the period
    InitPortValue = sum(i, x_start(i,loop_no)$(ord(loop_no) = cur_loop_no)*prod(t, (1+Return(t,i)$(Dates_for_actual_return(t)))));

    
    option decimals=8;
    
    solve MinCVaR_Opg7 minimizing CVaR using lp;

    
*   Save portfolio valuee (both current and previous after costs)

    Port_value_after_cost(loop_no) = InitPortValue - cost.l;
    
    Port_value(loop_no) = InitPortValue;
    
    Port_value_avg(loop_no)$(ord(loop_no) = cur_loop_no) = sum(i, x_start(i,loop_no)$(ord(loop_no) = cur_loop_no)*(1+avg_ret(i)));
    
    Port_value_min(loop_no)$(ord(loop_no) = cur_loop_no) = sum(i, x_start(i,loop_no)$(ord(loop_no) = cur_loop_no)*(1+min_ret(i)));
    
    Port_value_max(loop_no)$(ord(loop_no) = cur_loop_no) = sum(i, x_start(i,loop_no)$(ord(loop_no) = cur_loop_no)*(1+max_ret(i)));
    
*   Save optimal portfolio
    x_across_loops(i,loop_no)$(ord(loop_no) = cur_loop_no) = x.l(i);
    
*   New initial portfolio values for next period
    x_start(i,loop_next)$(ord(loop_next) = cur_loop_no + 1) = x.l(i);
    
    display Port_value, Port_value_avg, Port_value_min, Port_value_max, Cost.l;
)

