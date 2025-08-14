Sets
    AssetName           "Name of the asset"
    Date                "Observation dates"
    Asset               "ISIN codes" / DK0060259356,
                    DK0060259430,
                    DK0060259513,
                    DK0060259786 /
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


*************** FIND AFKAST OG COVARIANS ***************

Parameters
    CovWeek(i,j)
    CovYear(i,j)
    MeanRetWeek(i)
    MeanRetYear(i)
;


*
MeanRetWeek(i) = sum(t, Return(t,i)) / 364;
MeanRetYear(i) = prod(t, (1+Return(t,i)))**(1/7) - 1;

*Find weekly covariance
CovWeek(i,j) = sum(t,
    (Return(t,i) - MeanRetWeek(i)) * (Return(t,j) - MeanRetWeek(j))
) / (364 - 1);

*Scale covariance to yearly values
CovYear(i,j) = CovWeek(i,j)*52;

option decimals=8;
display MeanRetYear, CovYear;
