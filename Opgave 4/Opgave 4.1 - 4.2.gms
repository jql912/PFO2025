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

SET
    First7YDates(t)
;

First7YDates(t) = yes$(ord(t)<=364)

PARAMETER
    First7YRet(t,Asset,i)
;

First7YRet(t,Asset,i) = AssetReturn(t,Asset,i)$(First7YDates(t));


Return(t,i) = SUM(Asset, First7YRet(t,Asset,i));



*************** FIND AFKAST OG COVARIANS ***************

Parameters
    CovWeek(i,j)
    CovYear(i,j)
    MeanRetWeek(i)
    MeanRetYear(i)
;



MeanRetWeek(i) = sum(t, Return(t,i)) / 364;
MeanRetYear(i) = prod(t, (1+Return(t,i)))**(1/7) - 1;

CovWeek(i,j) = sum(t,
    (Return(t,i) - MeanRetWeek(i)) * (Return(t,j) - MeanRetWeek(j))
) / (364 - 1);

CovYear(i,j) = CovWeek(i,j)*52;


********* MARKOWITZ *********

POSITIVE VARIABLES
    X(i) 'Holdings of assets';

VARIABLES
    PortVariance 'Portfolio variance'
    PortSD       'Portfolio SD'
    PortReturn   'Portfolio return'
    z            'Objective function value';


EQUATIONS
    ReturnDef    'Equation defining the portfolio return'
    VarDef       'Equation defining the portfolio variance'
    SDDef        'Equation defining the portfolio SD'
    ReturnCond   'Equation that defines the return condition for the minimization over SD'
    VarCond      'Equation used in problem 4.2 to set variance in loop'
    SDCond       'Equation that defines the SD condition for maximization over expected return'
    NormalCon    'Equation defining the normalization contraint'
    ObjDef_Eff_front 'Objective function when including a lambda';
    

Scalars
    Return_bench    'Based on Balanceret'
    SD_bench        'Based on Balanceret'
    Var             'Variance used in loop (problem 4.2)'
    Lambda
;
Return_bench = 0.05914930;
SD_bench =  sqrt(0.00558054);

ReturnDef ..   PortReturn    =e= sum(i,MeanRetYear(i)*x(i));

ReturnCond ..  PortReturn    =g= Return_bench;

VarDef    ..   PortVariance  =e= sum((i,j), x(i)*CovYear(i,j)*x(j));

VarCond     ..  sum((i,j), x(i)*CovYear(i,j)*x(j)) =e= Var;

SDDef ..       PortSD =e= sqrt(PortVariance);

SDCond   ..    PortSD =l= SD_bench;

NormalCon ..   SUM(i, x(i))  =e= 1;

ObjDef_Eff_front ..   z       =e= (1-lambda) * PortReturn - lambda * PortVariance;


MODEL MeanVar_Return_bench 'PFO Model 3.2.3' /ReturnDef,VarDef,SDDef,NormalCon, ReturnCond/;
MODEL MeanVar_SD_bench 'PFO Model 3.2.3' /ReturnDef,VarDef,SDDef,NormalCon, SDCond/;
MODEL MeanVar_no_bench 'PFO Model 3.2.3' /ReturnDef,VarDef,NormalCon,ObjDef_Eff_front/;

OPTION decimals = 6 ;


* Solve first part of 4.1
SOLVE MeanVar_Return_bench MINIMIZING PortSD USING nlp;
display x.l, PortSD.l, PortReturn.l;

*save portfolios for part 4.3
PARAMETERS
    x_min_sd(i)
    x_max_ret(i)
;

x_min_sd(i) = x.l(i);

SOLVE MeanVar_SD_bench MAXIMIZING PortReturn USING nlp;
display x.l, PortSD.l, PortReturn.l;

x_max_ret(i) = x.l(i);



execute_unload 'Data_4.3.gdx', AssetName, x_min_sd, x_max_ret;

$exit

********* PROBLEM 4.2 *********


*Find smallest and largest variance without constraints
Parameter
    VarLambda0    Portfolio variance for lambda=0
    VarLambda1    Portfolio variance for lambda=1;
    
lambda = 0;
solve MeanVar_no_bench maximizing z using NLP;
VarLambda0 = PortVariance.l;

lambda = 1;
solve MeanVar_no_bench maximizing z using NLP;
VarLambda1 = PortVariance.l;


Scalar
    StepSize
    Var
    iterNum;

StepSize = (VarLambda0 - VarLambda1) / 10;


*Model that maximizes return given variance
MODEL MeanVarLoop /ReturnDef,VarCond,NormalCon/;

*Export the loop results
FILE FrontierHandle /"Problem 4.2 EffFront.csv"/;

FrontierHandle.pc = 5;
FrontierHandle.pw = 1048;

* Assign the output stream to the file handle "Frontier"

PUT FrontierHandle;

* Write the heading

PUT "Variance","ExpReturn";

LOOP (i, PUT i.tl);

PUT /;

SET steps /step1*step11/;

LOOP(steps,
    iterNum = ORD(steps);
    Var = VarLambda1 + StepSize*(iterNum - 1);

    SOLVE MeanVarLoop MAXIMIZING PortReturn USING NLP;

    PUT Var:6:5, PortReturn.l:6:5;

    LOOP(i,
        PUT x.l(i):6:5;
    );

    PUT /;
);

* Close file

PUTCLOSE;



******** Problem 4.3 **********

Parameters
    cum_return(i)
;

cum_return(i) = prod(t, (1+Return(t,i)) - 1)

