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

SET
    First7YDates(t)
;

First7YDates(t) = yes$(ord(t)<=364)

PARAMETER
    First7YRet(t,Asset,i)
;

First7YRet(t,Asset,i) = AssetReturn(t,Asset,i)$(First7YDates(t));


Return(t,i) = SUM(Asset, First7YRet(t,Asset,i));


************ BOOTSTRAP ************
Set
    s   /s1*s4/
    q   /q1*q1000/
;

Parameter
    randIndex(s,q)       
    Return_bs(i,s,q)
    Return_bs_3(i,q) 'Three month return';
;

* Generate 250 4 week scenarios
loop(q,
    loop(s,
*       Find random index
        randIndex(s,q) = uniformint(1,364);
    );

    loop(s,
*       Find the corresponding return
        loop(t$(ord(t) = round(randIndex(s,q))),
            Return_bs(i,s,q) = Return(t,i);
        );
    );

*   Multiply the 3 bootstrap values to get the 4 week return
    loop(i,
        Return_bs_3(i,q) = prod(s, (1+Return_bs(i,s,q)))-1;
    );
);

display Return_bs_3

EXECUTE_UNLOAD 'bootstrap_data_O5.gdx' i, q, Return_bs_3;