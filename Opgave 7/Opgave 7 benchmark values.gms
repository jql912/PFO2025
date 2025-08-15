option decimals = 8;

***** LAOD DATA ********

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

Return(t,i) = SUM(Asset, AssetReturn(t,Asset,i));


****** DEFINE STUFF FOR LOOP *********

Set
    s   /s1*s4/
    q   /q1*q1000/
    loop_no /loop1*loop73/
;

alias(loop_no, loop_next);

Parameters
    Dates(t)
    randIndex(s,q)       
    Return_bs(i,s,q)
    Return_4(i,q)           '4 week return'
    avg_ret(i)              "Average 4 week return across new training period bootstrap scenarios"
    Bench_ret_4w(loop_no)   "Average benchmark return over 4-week horizon (found in Opgave 6 benchmark calculations.gms)"
    sd_bench(loop_no) 
;

    
scalars
    n_week          'Number of weeks in each roll (actually minimum, since time period becomes small when approaching 2025)'
    loop_step       'Ordinality of date for starting date in training data roll'
    cur_loop_no
    date_start
    date_end
    pr
;

pr = 1/card(q);
n_week = 364;

scalar target_4w "Target return over 4-week period";
target_4w = 1.02**(1/13) - 1;


************** START LOOP *******************

loop(loop_no,

    cur_loop_no = ord(loop_no);
    
    date_start = cur_loop_no*4+1;
    date_end = n_week + cur_loop_no*4;
    

*   ********** DEFINE RELEVANT DATES *******

*   Find relevant dates
    Dates(t) = yes$(ord(t)<=date_end and ord(t)>=date_start);
    

    
*   ************ BOOTSTRAP 4 WEEK RETURNS OVER NEW TRAINING PERIOD ************

    
*   Generate 1000 4 week scenarios
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
    
*       Multiply the 4 single week returns bootstrap values to get the 4 week return
        loop(i,
            Return_4(i,q) = prod(s, (1+Return_bs(i,s,q)))-1;
        );
    );
    
    
*   *********** FIND AVERAGE RETURN *********
    
    avg_ret(i) = 1/card(q) * sum(q, Return_4(i,q));
    
    Bench_ret_4w(loop_no) = sum(i, avg_ret(i));
    
*   ********** FIND STANDARD DEVIATION ***********

    sd_bench(loop_no) = sum(i, sqrt(sum(q, (Return_4(i,q) - avg_ret(i)) * (Return_4(i,q) - avg_ret(i)))/(card(q)-1)));

)

execute_unload 'Benchmark_Opg7.gdx', loop_no, Bench_ret_4w, sd_bench;
