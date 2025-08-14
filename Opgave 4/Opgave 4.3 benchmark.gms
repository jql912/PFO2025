Sets
    AssetName           "Name of the asset"
    Date                "Observation dates"
    Asset               "ISIN codes" / DK0060259786 /
;

ALIAS(AssetName, i, j);
ALIAS(Date, t);

PARAMETER
    AssetReturn(t, Asset, i)
    Return(t)
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


Return(t) = Last6YRet(t,'DK0060259786','Jyske Portefølje Balanceret Akk KL');


display return




******** CUM RETURN ********

*total returns
PARAMETER TotalReturn  "Total return over all t for asset i";

TotalReturn = prod(t, 1 + Return(t)) - 1;


*cumulative returns
PARAMETERS
    CumReturn(t)
;

*for each asset
CumReturn(t) = 0;

scalar factor;
factor = 1;
LOOP(t$(Last6YDates(t)),
    factor = factor * (1 + Return(t));
    CumReturn(t) = factor;
);


display TotalReturn, CumReturn;



***** EXPORTER *******

*--- Export CumReturn to CSV ---
FILE CumReturnFile /"CumReturn_benchmark.csv"/;

* Optional: set page control, width if needed
CumReturnFile.pc = 5;
CumReturnFile.pw = 1048;

PUT CumReturnFile;

* Write header
PUT "Date","CumReturn_benchmark" /;

* Loop over dates
LOOP(t$(Last6YDates(t)),
    PUT t.tl:12:0, CumReturn(t):12:6 /;
);

* Close file
PUTCLOSE;