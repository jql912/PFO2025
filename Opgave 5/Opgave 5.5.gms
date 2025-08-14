********** Load return data for 2020-2025 *********

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
    Last6YDates(t)
;

Last6YDates(t) = yes$(ord(t)>364)

PARAMETER
    Last6YRet(t,Asset,i)
;

Last6YRet(t,Asset,i) = AssetReturn(t,Asset,i)$(Last6YDates(t));


Return(t,i) = SUM(Asset, Last6YRet(t,Asset,i));

display Return




******** LOAD PORTFOLIOS, TOTAL RETURN AND MULTIPLY BY WEIGHTS *********

*Load weights
parameters
    x_min_dr(AssetName)
    x_max_ret(AssetName)
;

$GDXIN Data_5.5.gdx
$LOAD x_min_dr x_max_ret
$GDXIN

display x_min_dr, x_max_ret


*total returns
PARAMETER TotalReturn(i)  "Total return over all t for asset i";

TotalReturn(i) = prod(t, 1 + Return(t,i)) - 1;


*cumulative returns
PARAMETERS
    CumReturn(t,i)
    CumReturn_min_dr(t)
    CumReturn_max_ret(t)
    TotalReturn_min_dr
    TotalReturn_max_ret
;

*for each asset
CumReturn(t,i) = 0;

scalar factor;
LOOP(i,
    factor = 1;
    LOOP(t$(Last6YDates(t)),
        factor = factor * (1 + Return(t,i));
        CumReturn(t,i) = factor;
    );
);


*for the two portfolios
CumReturn_min_dr(t) = sum(i, CumReturn(t,i) * x_min_dr(i));
CumReturn_max_ret(t) = sum(i, CumReturn(t,i) * x_max_ret(i));

display CumReturn_min_dr, CumReturn_max_ret;

TotalReturn_min_dr = sum(i,TotalReturn(i)*x_min_dr(i));
TotalReturn_max_ret = sum(i,TotalReturn(i)*x_max_ret(i));

display TotalReturn_min_dr,TotalReturn_max_ret



***** EXPORTER *******

*--- Export CumReturn to CSV ---
FILE CumReturnFile /"CumReturn_5.5.csv"/;

* Optional: set page control, width if needed
CumReturnFile.pc = 5;
CumReturnFile.pw = 1048;

PUT CumReturnFile;

* Write header
PUT "Date","CumReturn_min_dr","CumReturn_max_ret" /;

* Loop over dates
LOOP(t$(Last6YDates(t)),
    PUT t.tl:12:0, CumReturn_min_dr(t):12:6, CumReturn_max_ret(t):12:6/;
);

* Close file
PUTCLOSE;





******** WEEKLY RETURNS FOR PORTFOLIOS **********

Parameters
    Return_min_dr(t)
    Return_max_ret(t)
    test
;

Return_min_dr(t) = sum(i, (1+Return(t,i)$(Last6YDates(t))) * x_min_dr(i));
Return_max_ret(t) = sum(i, (1+Return(t,i)$(Last6YDates(t))) * x_max_ret(i));

display Return_min_dr, Return_max_ret;

***** EXPORTER *******

*--- Export CumReturn to CSV ---
FILE ReturnFile /"Return_5.5.csv"/;

* Optional: set page control, width if needed
ReturnFile.pc = 5;
ReturnFile.pw = 1048;

PUT ReturnFile;

* Write header
PUT "Date","Return_min_dr","Return_max_ret" /;

* Loop over dates
LOOP(t$(Last6YDates(t)),
    PUT t.tl:12:0, CumReturn_min_dr(t):12:6, CumReturn_max_ret(t):12:6/;
);

* Close file
PUTCLOSE;
