*** WDI data ***
cd "C:\Users\valor\OneDrive\Bureau\Master 2\Eco des donneés en panel"

* Sélection et traitement des variables "CO2" et "PIB"
clear
insheet using WDICSV.csv , delimiter(",") names 

* Filtrer les variables d'intérêt "CO2" et "PIB"
keep if inlist(indicatorcode, "NY.GDP.PCAP.PP.CD", "EN.ATM.CO2E.PC", "SP.URB.TOTL.IN.ZS", "NE.TRD.GNFS.ZS", "EG.EGY.PRIM.PP.KD")

* Liste des pays cibles (sans les pays avec données manquantes)
keep if inlist(countrycode, "ALB", "ARG", "ARM", "AUS", "AUT", "AZE", "BDI", "BEL") ///  
    | inlist(countrycode, "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", "BIH", "BLR") ///  
    | inlist(countrycode, "BLZ", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA", "CAF") ///  
    | inlist(countrycode, "CAN", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG") ///  
    | inlist(countrycode, "COL", "COM", "CPV", "CRI", "CYP", "CZE", "DEU", "DNK") ///  
    | inlist(countrycode, "DOM", "DZA", "ECU", "EGY", "ESP", "EST", "FIN", "FJI") ///  
    | inlist(countrycode, "FRA", "FSM", "GAB", "GBR", "GEO", "GHA", "GIN", "GMB") ///  
    | inlist(countrycode, "GNB", "GRC", "GTM", "HND", "HRV", "HTI", "HUN") ///  
    | inlist(countrycode, "IDN", "IND", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA") ///  
    | inlist(countrycode, "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR", "KOR") ///  
    | inlist(countrycode, "LBN", "LBY", "LTU", "LUX", "LVA", "MAR", "MDA", "MDG") ///  
    | inlist(countrycode, "MEX", "MHL", "MKD", "MLI", "MLT", "MNG", "MOZ", "MRT") ///  
    | inlist(countrycode, "MUS", "MYS", "NAM", "NER", "NIC", "NLD", "NOR", "NPL") ///  
    | inlist(countrycode, "NZL", "OMN", "PAK", "PAN", "PER", "PHL", "POL", "PRT") ///  
    | inlist(countrycode, "PRY", "QAT", "ROU", "RUS", "RWA", "SDN", "SEN", "SGP") ///  
    | inlist(countrycode, "SLB", "SLE", "SLV", "SRB", "SVK", "SVN", "SWE", "SWZ") ///  
    | inlist(countrycode, "SYC", "TCD", "TGO", "THA", "TJK", "TON", "TUN") ///  
    | inlist(countrycode, "TUR", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VNM") ///  
    | inlist(countrycode, "VUT", "ZAF", "ZMB", "ZWE")
*Séléction de 146 pays
	
* Suppression des points dans indicatorcode
replace indicatorcode = subinstr(indicatorcode, ".", "", .)

* Créer une variable 'zind' pour grouper les indicateurs
egen zind = group(indicatorcode)

* Vérifier la variable zind
sum zind
local maxzind = r(max)

save WDI, replace

* Boucle pour transposer chaque indicateur
forvalues it=1(1)`maxzind' {
	quietly {
	use WDI, clear
	keep if zind==`it'
	reshape long v, i(countrycode) j(year)
	replace year=year+1955
	local zz=indicatorcode
	rename v `zz'
	local ww=indicatorname
	label var `zz' " `ww' "
	drop indicatorcode indicatorname
	sort countrycode year
	save temp`it', replace
	}
	display "itération"`it'
}

	* 3.3 assemblage des fichiers
use temp1, clear
forvalues it=2(1)`maxzind' {
	quietly{
	merge countrycode year using temp`it'
	keep if _merge==3
	drop _merg
	sort countrycode year
	}
	display "itération"`it'
}
drop zind
save WDIall, replace

forvalues it=1(1)`maxzind' {
	erase temp`it'.dta
}

drop if year < 2000 | year > 2020
*Sélection de 20 ans entre 2000 et 2020

save donnee_final,replace

* Chargement du fichier propre
use donnee_final.dta, clear

rename ENATMCO2EPC CO2
rename NYGDPPCAPPPCD PIB
rename NETRDGNFSZS commerce
rename SPURBTOTLINZS urbanisation
rename EGEGYPRIMPPKD energie

save donnee_final,replace




* Chargement du fichier propre
use donnee_final.dta, clear

*Valeurs manquantes*
misstable summarize

*Stat descriptive variables brutes*

* Résumé statistique 
summarize CO2 PIB commerce urbanisation energie

* Statistiques descriptives pour la variable CO2
summarize CO2
summarize CO2, detail

* Statistiques descriptives pour la variable PIB
summarize PIB
summarize PIB, detail

* Statistiques descriptives pour la variable commerce
summarize commerce
summarize commerce, detail

* Statistiques descriptives pour la variable urbanisation
summarize urbanisation
summarize urbanisation, detail

* Statistiques descriptives pour la variable energie
summarize energie
summarize energie, detail

* Graphique de dispersion de  CO2 vs PIB
twoway scatter CO2 PIB

* Graphique de dispersion de CO2 vs commerce 
twoway scatter CO2 commerce

* Graphique de dispersion de  CO2 vs urbanisation
twoway scatter CO2 urbanisation

* Graphique de dispersion de CO2 vs energie 
twoway scatter CO2 energie

*Stat descriptive variable transformé*

*Avec ln(PIB)² pour avoir une forme quadratique
gen ln_CO2 = ln(CO2)
gen ln_commer = ln(commerce)
gen ln_urba = ln(urbanisation)
gen ln_ener = ln(energie)
gen ln_PIB = ln(PIB)
gen ln_PIB_carre = ln_PIB^2

* Statistiques descriptives pour la variable ln_CO2
summarize ln_CO2
summarize ln_CO2, detail

* Statistiques descriptives pour la variable ln_PIB
summarize ln_PIB
summarize ln_PIB, detail

* Statistiques descriptives pour la variable ln_PIB_carre
summarize ln_PIB_carre
summarize ln_PIB_carre, detail

* Statistiques descriptives pour la variable ln_commer
summarize ln_commer
summarize ln_commer, detail

* Statistiques descriptives pour la variable urbanisation
summarize ln_urba
summarize ln_urba, detail

* Statistiques descriptives pour la variable energie
summarize ln_ener
summarize ln_ener, detail

* Corrélation 
corr ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener

* Vif 
reg ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener
vif


* Créer une variable numérique pour CountryName
encode countrycode, gen(countrycode_num)
xtset countrycode_num year

*1. Estimateur a effet fixe*
xtreg ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener, fe /* fe = fixed effect*/

*2. Estimateur à effets aléatoires*
xtreg ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener, re

*3. Test de Haussman
xtreg ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener,i(countrycode_num) fe /* fe = fixed effect*/
estimates store lnefixe

xtreg ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener,i(countrycode_num) re /*random effect= re*/
estimates store lnealeatoir

hausman  lnefixe lnealeatoir
hausman  lnefixe lnealeatoir, sigmamore

*Conclusion : Le modèle à effet fixe est préféré *

*4. Graphique omparaison FE RE
xtreg  ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener, i(countrycode_num) fe
scalar fec=_b[_cons]
scalar fex=_b[ln_PIB]
scalar fex2 = _b[ln_PIB_carre]

xtreg  ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener, i(countrycode_num) re
scalar rec=_b[_cons]
scalar rex=_b[ln_PIB]
scalar rex2 = _b[ln_PIB_carre]

* Graphique avec PIB² et effets fixes/aléatoires
twoway (function y = fec + fex*x + fex2*x^2, range(5 13) lcolor(red) lpattern(solid)) || ///
       (function y = rec + rex*x + rex2*x^2, range(5 13) lcolor(blue) lpattern(dash)), ///
       legend(order(1 "Fixed Effects" 2 "Random Effects") cols(2)) ///
       xtitle("GDP per capita (log)", size(vsmall)) ytitle("CO2 emissions per capita (log)", size(vsmall)) ///
       title("Kuznets Curve with Fixed and Random Effects", size(small))

summarize ln_PIB

*Nombre de pays après le point de retournement*
count if ln_PIB >= 10.45

count if ln_PIB >= 10.45 & year == 2020

count if ln_PIB >= 10.45 & year == 2000

*1. Classement des effet fixe des pays*
xtreg ln_CO2 ln_PIB ln_PIB_carre ln_commer ln_urba ln_ener, fe /* fe = fixed effect*/
predict effets_fixes, u
list countrycode_num effets_fixes

egen rank = rank(effets_fixes)
sort rank
graph bar effets_fixes, over(countrycode_num, sort(rank)) title("Effets fixes par pays (classés)")

collapse (mean) effets_fixes, by(countrycode_num countryname)
gsort -effets_fixes
list countryname effets_fixes

*Boxplot*
graph box CO2
graph box PIB
graph box commerce
graph box urbanisation
graph box energie

graph box ln_CO2
graph box ln_PIB
graph box ln_commer
graph box ln_urba
graph box ln_ener
graph box ln_PIB_carre


