option decimals = 8;

***** LAOD DATA ********

Sets
    AssetName           "Name of the asset"
    Date                "Observation dates"
    Asset               "ISIN codes" / DK0060259786 /
;

ALIAS(AssetName, i, j);
ALIAS(Date, t, tt);

PARAMETER
    AssetReturn(t, Asset, i)
    Return(t)
    Dates_2020_2025(t)
    Parameter CumValue(t) "Cumulative portfolio value over Dates_2020_2025";
;

$GDXIN weekly_returns_2013_2025
$LOAD Date AssetName AssetReturn
$GDXIN

Return(t) = SUM((Asset,i), AssetReturn(t,Asset,i));

Dates_2020_2025(t) = Date(t)$(ord(t) >= 365);

display Dates_2020_2025;


* Initialize first date
CumValue(t) = 0;
CumValue("2020-01-01") = 1;

* Loop through your dates
Scalar prevValue;
prevValue = 1;

loop(t$(Dates_2020_2025(t)),
    if(ord(t) = 365,
        CumValue(t) = prevValue;
    else
        CumValue(t) = prevValue * (1 + Return(t));
        prevValue = CumValue(t);
    );
);

display Return, CumValue

* Parameter for cumulative value every 4 weeks
Parameter CumValue4w(t) "Cumulative portfolio value, compounded every 4 weeks";
Parameter Return4w(t) "Return over each 4-week period";

* Initialize first date
CumValue4w(t) = 0;
Scalar prevValue;
prevValue = 1;

* Loop over all weeks in Dates_2020_2025


Scalar ret4w;
loop(t$(Dates_2020_2025(t)),
    
    if(mod(ord(t)-365,4) = 0,   
        ret4w = 1;
        
        
        loop(tt$(ord(tt) >= ord(t) and ord(tt) < ord(t)+4 and Dates_2020_2025(tt)),
            ret4w = ret4w * (1 + Return(tt));
        );
        
        ret4w = ret4w - 1;          
        Return4w(t) = ret4w;       
        
        CumValue4w(t) = prevValue * (1 + ret4w);  
        prevValue = CumValue4w(t);
    );
);

execute_unload 'benchmark_port_value.gdx' CumValue4w;


$exit

*--- Export to CSV ---
FILE ReturnFile /"PortValues_Opg7_benchmark.csv"/;

* Optional: set page control, width if needed
ReturnFile.pc = 5;
ReturnFile.pw = 1048;

PUT ReturnFile;

* Write header
PUT "Loop number", "Benchmark Value"/;

* Loop over dates
LOOP(t$(Dates_2020_2025(t)),
    PUT t.tl:12:0, CumValue(t):12:6;
    PUT /;
);

* Close file
PUTCLOSE;
