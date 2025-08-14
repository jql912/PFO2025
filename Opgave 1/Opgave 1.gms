SET
    AssetName
    Date
    Asset /DK0016111511/
;

ALIAS(AssetName, i);
ALIAS(Date, t);

PARAMETER
    AssetReturn(t, Asset, i)
    Returns(t, i)
    DateIndex(t)
    AvgRet3(i)
    VarianceAnnualReturn3(i)
    StdDevAnnualReturn3(i)
    AvgRet5(i)
    VarianceAnnualReturn5(i)
    StdDevAnnualReturn5(i)
    AvgRet10(i)
    VarianceAnnualReturn10(i)
    StdDevAnnualReturn10(i);
;

$GDXIN weekly_returns_2013_2025
$LOAD Date AssetName AssetReturn
$GDXIN

SET
    First3YDates(t)
;

First3YDates(t) = yes$(ord(t)<156)

PARAMETER
    First3YRet(t,Asset,i)
;

First3YRet(t,Asset,i) = AssetReturn(t,Asset,i)$(First3YDates(t))

display First3YRet

$exit



Returns(t,i) = SUM(Asset, AssetReturn(t,Asset,i));

SET YearIndex10 /2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024/;
SET YearIndex5 /2020, 2021, 2022, 2023, 2024/;
SET YearIndex3 /2022, 2023, 2024/;


execute_unload 'returns.gdx', Returns;

execute 'gdxdump returns.gdx par=Returns output=returns.csv';


$exit


******* 3 års data *********

Scalar NDates;
NDates = card(Date);

loop(t,
   DateIndex(t) = ord(t);
);

Parameter WeekToYear(t);
loop(t,
   WeekToYear(t) = floor((DateIndex(t)-1) / 52) + 2013;
);

Parameter
    AnnualReturn3(YearIndex3, i)
    AvgAnnualReturn3(i)
    VarianceAnnualReturn3(i)
    StdDevAnnualReturn3(i)
    AvgStdDevAnnualReturn3(i)
;

loop((YearIndex3,i),
    AnnualReturn3(YearIndex3,i) = prod(t$(WeekToYear(t) = ord(YearIndex3)+2021), 1 + Returns(t,i)) - 1;
);


AvgAnnualReturn3(i) = sum(YearIndex3, AnnualReturn3(YearIndex3,i)) / 3;

loop(i,
    AvgAnnualReturn3(i) = sum(YearIndex3, AnnualReturn3(YearIndex3,i)) / card(YearIndex3);

    VarianceAnnualReturn3(i) = sum(YearIndex3, sqr(AnnualReturn3(YearIndex3,i) - AvgAnnualReturn3(i))) / (card(YearIndex3) - 1);

    StdDevAnnualReturn3(i) = sqrt(VarianceAnnualReturn3(i));
);

display YearIndex3, WeekToYear, AnnualReturn3, AvgAnnualReturn3, StdDevAnnualReturn3;

EXECUTE_UNLOAD 'AvgRet3.gdx' i, YearIndex3, AvgAnnualReturn3, AnnualReturn3, StdDevAnnualReturn3;


******* 5 års data *********



Parameter
    AnnualReturn5(YearIndex5, i)
    AvgAnnualReturn5(i)
;

loop((YearIndex5,i),
    AnnualReturn5(YearIndex5,i) = prod(t$(WeekToYear(t) = ord(YearIndex5)+2019), 1 + Returns(t,i)) - 1;
);



AvgAnnualReturn5(i) = sum(YearIndex5, AnnualReturn5(YearIndex5,i)) / 5;

display YearIndex5, WeekToYear, AnnualReturn5, AvgAnnualReturn5;

EXECUTE_UNLOAD 'AvgRet5.gdx' i, YearIndex5, AvgAnnualReturn5, AnnualReturn5;


******* 10 års data *********

display YearIndex10;

Parameter
    AnnualReturn10(YearIndex10, i)
    AvgAnnualReturn10(i)
;

loop((YearIndex10,i),
    AnnualReturn10(YearIndex10,i) = prod(t$(WeekToYear(t) = ord(YearIndex10)+2014), 1 + Returns(t,i)) - 1;
);



AvgAnnualReturn10(i) = sum(YearIndex10, AnnualReturn10(YearIndex10,i)) / 10;

display YearIndex10, WeekToYear, AnnualReturn10, AvgAnnualReturn10;

EXECUTE_UNLOAD 'AvgRet10.gdx' i, YearIndex10, AvgAnnualReturn10, AnnualReturn10;