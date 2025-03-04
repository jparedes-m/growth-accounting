global dir "C:\Users\lnick\Desktop\Macro\Ex"
cd "$dir"
clear 

** SET UP **

* Define variables here: (varname)=(filename)/(sheetname): 
* code will clean and format each variable, stored in a tempfile
local vars ///
"Y=ES_national-accounts/VA_Q"   	/// real total output
"L=ES_national-accounts/EMP"    	/// total employment
"K=ES_capital-accounts/Kq_GFCF"		/// real capital stock
"Y_nom=ES_national-accounts/VA_Q"	/// nominal total output (for labor share)
"WL=ES_national-accounts/COMP"	  	/// nominal labor compensation (for labor share)
"K_I1=ES_capital-accounts/Kq_Soft_DB"		/// intangibles
"K_I2=ES_capital-accounts/Kq_Rstruc"		/// 
"K_I3=ES_capital-accounts/Kq_RD"			///
"gy=ES_growth-accounts/LP2_G" 		/// growth rates/contributions
"gA=ES_growth-accounts/LP2ConTFP" 	///
"gK1=ES_growth-accounts/LP2ConTangNICT" 		 ///
"gK2=ES_growth-accounts/LP2ConTangICT" 		 ///
"gK3=ES_growth-accounts/LP2ConIntang" 		 //

foreach var in "`vars'"  {
	local var = subinstr("`var'","-"," ",.)
	local varname = substr("`var'",1,strpos("`var'","=")-1)
	local filename = substr("`var'",length("`varname'")+2,strpos("`var'","/")-2-length("`varname'"))
	local sheetname = substr("`var'",strpos("`var'","/")+1,.)
	di `"`varname' `filename' `sheetname'"'

	import excel "$dir/RawData/`filename'.xlsx", firstrow clear sheet("`sheetname'")  
	local t = 1995
	foreach col of varlist F-AF {
		rename `col' `varname'`t'
		local t = `t'+1
	}
	
	drop var geo* 
	reshape long `varname', i(nace_r2_code) j(t)
	keep if 2010 <= t & t <=2019

	replace nace_r2_code = "O-U" if nace_r2_code=="O"|nace_r2_code=="P"|nace_r2_code=="Q"|nace_r2_code=="R"|nace_r2_code=="S"|nace_r2_code=="T"|nace_r2_code=="U"
	collapse (sum) `varname', by(nace_r2_code t)
	gen keep = .
	foreach x in TOT MARKT A B C D-E F G H I J K L M-N O-U {
		replace keep=1 if nace_r2_code=="`x'"
	}
	keep if keep==1 
	drop keep
	
	tempfile `varname'
	save ``varname'' 
}

** B ** 
use `Y', clear
merge 1:1 nace_r2_code t using `L' 
drop _merge

encode nace_r2_code, gen(nace)
xtset nace t 

gen y = Y / L 
twoway (tsline y if nace_r2_code == "TOT")  (tsline y if nace_r2_code == "MARKT"),  name(b, replace) legend(order(1 "TOT" 2 "MARKT")) xtitle("")
graph export b.png, replace

/* Comments 
increased rapidly after GFC and did not come down until Covid 
lowest earning workers laid off in 2008, increasing output per worker? 
would be consistent if value added fell, but labor fell more. seems to be the case? 
*/ 

** C **

* (# workers in educ in sector / total workers in sector) = shareE
* (total wages paid in educ in sector / total wages paid in sector) = shareW
* shareE/shareW = 
* (# workers in in educ in sector / total wages in educ in sector) * (total wages in sector/ total workers in sector)
* (total wages paid = wage per worker * worker)
* so = (wage per worker in sector / wage per worker in educ in sector)
* shareW/shareE = (wage per worker in educ in sector / wage per worker in sector) = relative returns of that sector (including education)


** merge in labor data 

preserve 
import excel "$dir/RawData/ES_labour accounts.xlsx", firstrow clear sheet("Share_E")
local t = 1995
foreach var of varlist F-AF {
	rename `var' shareE`t'
	local t = `t'+1
}
tempfile E 
save `E'
import excel "$dir/RawData/ES_labour accounts.xlsx", firstrow clear sheet("Share_W")
local t = 1995
foreach var of varlist F-AF {
	rename `var' shareW`t'
	local t = `t'+1
}
merge 1:1 country code education age gender using `E' 
drop country _merge 
rename code nace_r2_code 
reshape long shareE shareW, i(nace_r2_code education age gender) j(t)
collapse (sum) share*, by(nace_r2_code education t)

* method 1 
gen phi = shareW / shareE - 1
gen h_unw = exp(phi) 
gen h = h_unw * (shareE/100) //unweighted

* method 2
destring educ, replace
gen educ_high = (education==1)
gen educ_med = (education==2)
gen educ_low = (education==3)
gen phi_alt = educ_low*(0.134*education) + educ_med*(0.536 + 0.101*(education-4)) + educ_high*(0.94 + 0.068*(education-8))
gen h_alt_unw = exp(phi_alt)
gen h_alt = h_alt_unw * (shareE/100)

replace nace_r2_code = "D-E" if nace_r2_code=="D"|nace_r2_code=="E"
replace nace_r2_code = "O-U" if nace_r2_code=="O"|nace_r2_code=="P"|nace_r2_code=="Q"|nace_r2_code=="R"|nace_r2_code=="S"|nace_r2_code=="T"|nace_r2_code=="U"
replace nace_r2_code = "M-N" if nace_r2_code=="M"|nace_r2_code=="N"

collapse (sum) h h_alt share*, by(nace_r2_code t)
keep if 2010 <= t & t <=2019

tempfile h 
save `h'
restore 

merge 1:1 nace_r2_code t using `h' 
drop _merge

** merge in capital data
merge 1:1 nace_r2_code t using `K'
drop _merge 
merge 1:1 nace_r2_code t using `K_I1'
drop _merge 
merge 1:1 nace_r2_code t using `K_I2'
drop _merge 
merge 1:1 nace_r2_code t using `K_I3'
drop _merge 
gen K_alt = K - K_I1 - K_I2 - K_I3 // tangible capital

** construct A
drop nace 
encode nace_r2_code, gen(nace) 
xtset nace t 
gen KdivY = K / Y
gen K_altdivY = K_alt / Y
gen A = (y / h) * (KdivY)^(-1/2)
gen A_alth = ((y / h_alt) * (KdivY)^(-1/2))
gen A_altk = ((y / h) * (K_altdivY)^(-1/2))
gen A_altkh = ((y / h_alt) * (K_altdivY)^(-1/2))

** growth rates
foreach var of varlist y KdivY h A K {
	gen ln`var' = ln(`var')
	gen dln`var' = d.ln`var'
	drop ln`var'
}

** normalize all variables to start at 100 for easier comparison
foreach var in y KdivY K_altdivY h h_alt A A_alth A_altkh {
	bys nace_r2_code (t) : gen s_`var' = 100 * (`var' / `var'[1])
}

twoway (tsline s_y if nace_r2_code == "TOT") ///
(tsline s_KdivY if nace_r2_code == "TOT") ///
(tsline s_h if nace_r2_code == "TOT") ///
(tsline s_A if nace_r2_code == "TOT") ///
, legend(order(1 "y" 2 "K/Y" 3 "h" 4 "A")) name(c, replace) xtitle("")
graph export c.png, replace

twoway (tsline s_y if nace_r2_code == "TOT") ///
(tsline s_KdivY if nace_r2_code == "TOT") ///
(tsline s_h_alt if nace_r2_code == "TOT") ///
(tsline s_A_alth if nace_r2_code == "TOT") ///
, legend(order(1 "y" 2 "K/Y" 3 "h_alt" 4 "A_alth")) name(c_alt, replace) xtitle("")
graph export c_alth.png, replace

twoway (tsline s_y if nace_r2_code == "TOT") ///
(tsline s_K_altdivY if nace_r2_code == "TOT") ///
(tsline s_h if nace_r2_code == "TOT") ///
(tsline s_A_altk if nace_r2_code == "TOT") ///
, legend(order(1 "y" 2 "K_alt/Y" 3 "h" 4 "A_altk")) name(c_alt, replace) xtitle("")
graph export c_altk.png, replace

twoway (tsline s_y if nace_r2_code == "TOT") ///
(tsline s_K_altdivY if nace_r2_code == "TOT") ///
(tsline s_h_alt if nace_r2_code == "TOT") ///
(tsline s_A_altk if nace_r2_code == "TOT") ///
, legend(order(1 "y" 2 "K_alt/Y" 3 "h_alt" 4 "A_altkh")) name(c_alt, replace) xtitle("")
graph export c_altkh.png, replace

*gen dlny_hat = 0.5*dlnKdivY + dlnh + dlnA
*twoway (tsline dlny if nace_r2_code == "TOT", lwidth(2)) (tsline dlny_hat if nace_r2_code == "TOT", lwidth(1)), name(c2, replace) // identical, good

preserve // create stacked bar chart - kind of hard to do in stata, but this works
keep if nace_r2_code=="TOT"
gen zero = 0 
gen dlnKdivYhalf = 0.5*dlnKdivY
gen dlnKdivYhalf_pos = max(dlnKdivYhalf, 0)
gen dlnKdivYhalf_neg = min(dlnKdivYhalf, 0)
gen dlnA_pos = max(dlnA, 0)
gen dlnA_neg = min(dlnA, 0)
gen dlnh_pos = max(dlnh, 0)
gen dlnh_neg = min(dlnh, 0)
gen dlnA_top_pos = dlnKdivYhalf_pos + dlnA_pos
gen dlnh_top_pos = dlnA_top_pos + dlnh_pos
gen dlnA_top_neg = dlnKdivYhalf_neg + dlnA_neg
gen dlnh_top_neg = dlnA_top_neg + dlnh_neg
twoway ///
    rbar dlnh_top_neg dlnA_top_neg t, color(red) lcolor(none) || /// 
    rbar dlnA_top_neg dlnKdivYhalf_neg t, color(orange)  lcolor(none) || /// 
    rbar dlnKdivYhalf_neg zero t, color(blue)  lcolor(none) || /// 
    rbar zero dlnKdivYhalf_pos t, color(blue)  lcolor(none) || /// 
    rbar dlnKdivYhalf_pos dlnA_top_pos t, color(orange)  lcolor(none) || /// 
    rbar dlnA_top_pos dlnh_top_pos t, color(red)  lcolor(none) || /// 
    line dlny t, lcolor(black) lwidth(medthick)  || ///
    , legend(order(1 "dlnh contribution" 2 "dlnA contribution " 3 "0.5*dln(K/Y) contribution" 7 "Growth rate of y")) name(c3,replace) xtitle("") //
graph export c_contributions.png, replace

restore


** D **
*preserve
gen keep = .
foreach x in TOT A B C D-E F G H I J K L M-N O-U {
	replace keep=1 if nace_r2_code=="`x'"
}
keep if keep==1 
drop keep
tsline s_y s_KdivY s_h s_A, by(nace_r2_code) name(d, replace) xtitle("") legend(order(1 "y" 2 "K/Y" 3 "h" 4 "A"))
graph export d.png, replace

tsline s_y s_KdivY s_h_alt s_A_alth, by(nace_r2_code) name(d_alt, replace) xtitle("") legend(order(1 "y" 2 "K/Y" 3 "h_alt" 4 "A_alth"))
graph export d_alth.png, replace

tsline s_y s_K_altdivY s_h s_A_altk, by(nace_r2_code) name(d_alt, replace) xtitle("") legend(order(1 "y" 2 "K_alt/Y" 3 "h" 4 "A_altk"))
graph export d_altk.png, replace

tsline s_y s_K_altdivY s_h_alt s_A_altkh, by(nace_r2_code) name(d_alt, replace) xtitle("") legend(order(1 "y" 2 "K_alt/Y" 3 "h_alt" 4 "A_altkh"))
graph export d_altkh.png, replace

** E **

preserve 
use `Y_nom', clear 
merge 1:1 nace_r2_code t using `WL'
drop _merge
gen alpha_t = 1-(WL / Y_nom) // WL/Y = labor share = (1-a) -> alpha = 1 - WL/Y
egen alpha = mean(alpha_t), by(nace_r2_code)
tempfile alpha 
save `alpha'
restore

merge 1:1 nace_r2_code t using `alpha'
keep if _merge==3 // keep sectors of interest
drop _merge

gen Anew = (y / h) * (KdivY)^(-alpha/(1-alpha))
bys nace_r2_code (t) : gen s_Anew = 100 * (Anew/ Anew[1])
gen Anew_alth = (y / h_alt) * (KdivY)^(-alpha/(1-alpha))
bys nace_r2_code (t) : gen s_Anew_alth = 100 * (Anew_alth/ Anew_alth[1])
gen Anew_altk = (y / h) * (K_altdivY)^(-alpha/(1-alpha))
bys nace_r2_code (t) : gen s_Anew_altk = 100 * (Anew_altk/ Anew_altk[1])
gen Anew_altkh = (y / h_alt) * (K_altdivY)^(-alpha/(1-alpha))
bys nace_r2_code (t) : gen s_Anew_altkh = 100 * (Anew_altkh/ Anew_altkh[1])

tsline s_A s_Anew, by(nace_r2_code) name(e, replace) xtitle("") legend(order(1 "A" 2 "A_new"))
graph export e.png, replace

tsline s_A_alth s_Anew_alth, by(nace_r2_code) name(e_alt, replace) xtitle("") legend(order(1 "A_alth" 2 "A_alth_new"))
graph export e_alth.png, replace

tsline s_A_altk s_Anew_altk, by(nace_r2_code) name(e_alt, replace) xtitle("") legend(order(1 "A_altk" 2 "A_altk_new"))
graph export e_altk.png, replace

tsline s_A_altkh s_Anew_altkh, by(nace_r2_code) name(e_alt, replace) xtitle("") legend(order(1 "A_altkh" 2 "A_altkh_new"))
graph export e_altkh.png, replace


** F **  - not sure how to do this
/*
preserve 
use `Y', clear
gen keep = .
foreach x in A B C D-E F G H I J K L M-N O-U {
	replace keep=1 if nace_r2_code=="`x'"
}
keep if keep==1 
egen tot_Y = total(Y), by(t)
gen share_Y = Y / tot_Y 
tempfile tot_Y 
save `tot_Y'
restore

merge 1:1 nace_r2_code t using `tot_Y'
keep if _merge==3 
drop _merge
foreach var of varlist dlny dlnK dlnA {
	gen w_`var' = (share_Y) * `var'
} 
collapse (sum) w_*, by(t)
foreach var of varlist w_* {
	replace `var' = `var'*100
}



preserve 
use `gy', clear
merge 1:1 nace_r2_code t using `gK1'
drop _merge
merge 1:1 nace_r2_code t using `gK2'
drop _merge
merge 1:1 nace_r2_code t using `gK3'
drop _merge
gen gK = 2*(gK1 + gK2 + gK3) + gy
merge 1:1 nace_r2_code t using `gA'
drop _merge
keep if nace_r2_code=="TOT" 
collapse (sum) g*, by(t)
tempfile g 
save `g'
restore 

merge 1:1 t using `g'
drop if t==2010
foreach var in y K A {
	tsline w_dln`var' g`var', name(f`var', replace)
}


** question: what does bottom up mean in the data?



