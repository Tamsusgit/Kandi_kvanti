Stata-demo 11.3.2021, Teemu

* komennon ajo: maalaa ja ctrl + D


* Aineiston tuonti:
* File -> Import -> xsl*

*aineiston perustiedot, ok jos vähän muuttujia
codebook

YHDEN MUUTTUJAN KUVAILUA
*perustiedot muuttujittain (summary, su)
su SR_vp elevation
su SR_vp elevation, detail

*taulukointi tabulate, ta (jos arvoja rajallinen määrä)
ta SR_vp

*histogrammi
hist SR_vp, bin(2) frequency
hist SR_vp, bin(10) frequency 
*10 luokkaa 

help hist

KAHDEN MUUTTUJAN ANALYYSIA

*hajontakuva + regressiosovite
scatter SR_vp elevation || lfit SR_vp elevation

*Pearson
corr SR_vp elevation
pwcorr SR_vp elevation, sig 
* significanse

*Sparman, vain järjestysasteikolliset muuttujat
spearman SR_vp elevation

*OLS_regressio, bivariaattimalli (eka selitettävä muuttuja) (tämä siis se suora)
reg SR_vp elevation

OIKEET TEHTÄVÄT

* Selitettäväks SR_vp (lajirunsaus), selittäväks elevation, tavg_summer (T arvg) ja SWI (SAGA Wetness Index)

su tavg_summer SWI

median SR_vp

hist SWI, bin(5) frequency

scatter SR_vp SWI
scatter SR_vp tavg_summer

corr SR_vp tavg_summer
spearman SR_vp tavg_summer

reg SR_vp tavg_summer

scatter SR_vp tavg_summer || lfit SR_vp tavg_summer






