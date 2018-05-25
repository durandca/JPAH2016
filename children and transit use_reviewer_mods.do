**********************************************************************
*Analysis of physical activity linked to transit use using CHTS data
*Update of original file to address reviewer comments
*Prepared by Casey P Durand
*Last updated 12/04/15
**********************************************************************

*Note: The following do file assumes you have already merged the household,
*person and place files from CHTS.  There should be 460858 records in the dataset

clear
use "C:\Users\Casey\Google Drive\Work\Papers\children, transit and pa\chts_household_person_place.dta"

order sampn perno plano, after(sampno)

sort sampn perno plano

*create single new variable to indicate individual people by
*combining household and person-within-household identifiers
egen sampnperno=concat(sampn perno)
order sampnperno, after(plano)
destring sampnperno, replace

*merge in RUCA data
*prep CHTS dataset
gen str ctfip_s = string(ctfip, "%05.0f")
gen str hctract_s = string(hctract, "%06.0f")
egen tractid=concat( ctfip_s hctract_s )
*complete merge
merge m:1 tractid using "C:\Users\Casey\Google Drive\Work\Papers\children, transit and pa\ruca2010_CA.dta", generate(_merge2)
drop if _merge2==2



********************************************************************************
*Clean dataset to recode missing/refused responses to Stata missing (.)*
********************************************************************************

foreach var of varlist hhbic resty ten incom hhsize vehop race1 wkstat jobs{

recode `var' (97 98 99 998 999=.), g (`var'2)

 }

foreach var of varlist own gend hisp ntvty lic trans emply wsched disab educa{

recode `var' (7 8 9=.), g (`var'2)

 }

recode age (998 999=.), g(age2)

recode wloc (7 8 9=.), g (wloc3)

*Create new version of residence type to remove category 7 (boat/rv/van)

recode resty2 (7=.), g(resty3)

*Create variable indicating month of year data was collected
g month=substr(recdate, 6,2)
destring month, replace

*Recode 1/2 variables to 0/1
foreach var of varlist own2 gend2 hisp2 ntvty2 lic2 emply2 disab2 {
recode `var'(2=0)
}

*generate new mode variable
*default is "other"
g mode2=5

*collapse all transit modes into one overall transit category
replace mode2=3 if inlist(mode, 15, 16, 17, 19, 24, 26, 27)

*walking
replace mode2=1 if mode==1

*biking
replace mode2=2 if mode==2

*auto
replace mode2=4 if inlist(mode, 5, 6, 7, 8)

*add missing responses back
replace mode2=. if mode==.

la def mode 1 Walk 2 Bike 3 Transit 4 Automobile 5 Other
la val mode2 mode
la var mode2 "Mode of travel"

*categorize age
g catage=.
replace catage=1 if age2>=5 & age2<11
replace catage=2 if age2>=11 & age2<16
replace catage=3 if age2>=16 & age2<18

la def age 1 "5-10" 2 "11-15" 3 "16-18"
la val catage age
la var catage "Age"

*categorize household income
g catincom=.
replace catincom=1 if inlist(incom2, 1, 2)
replace catincom=2 if inlist(incom2, 3, 4)
replace catincom=3 if incom2==5
replace catincom=4 if incom2==6
replace catincom=5 if incom2<. & incom2>6

la def inc 1 "<$25,000" 2 "$25,000-$49,999" 3 "$50,000-$74,999" 4 "$75,000-$99,999" 5 "$100,000+"
la val catincom inc
la var catincom "Household income"

*categorize household size
g cathhsize=.
replace cathhsize=1 if inlist(hhsize2, 1,2)
replace cathhsize=2 if inlist(hhsize2, 3,4,5,6)
replace cathhsize=3 if hhsize2>6 & hhsize2<.

la def hhsiz 1 "1-2" 2 "3-6" 3 "7+"
la val cathhsize hhsiz
la var cathhsize "Household size"

*modify lic2 and emply2 to account for the fact that these questions were not asked if respondent under age of 15
replace lic2=0 if age2<=15
replace emply2=0 if age2<=15

*generate variable indicating the total number of licensed drivers in household
bys sampn: egen tot_lic=total(lic2)

*generate variable indicating hhveh/licensed drivers
g vehperlic=0 
replace vehperlic=hhveh/tot_lic if tot_lic!=0 & hhveh!=.

*categorize household vehicles per licensed driver
g cathhveh=.
replace cathhveh=1 if vehperlic==0
replace cathhveh=2 if vehperlic>0 & vehperlic<=1
replace cathhveh=3 if vehperlic>1 & vehperlic<.

la def hhveh 1 "None" 2 ">0-1" 3 ">1"
la val cathhveh hhveh
la var cathhveh "Vehicles at home per licensed driver"

*categorize months into seasons
g season=.
replace season=1 if inlist(month, 3,4,5) /*spring*/
replace season=2 if inlist(month, 6, 7, 8 ) /*summer*/
replace season=3 if inlist(month, 9, 10, 11 ) /*fall*/
replace season=4 if inlist(month, 12, 1, 2 ) /*winter*/

la def season 1 Spring 2 Summer 3 Fall 4 Winter
la val season season
la var season "Season"

*categorize county size
g cntysize=1
replace cntysize=2 if inlist(ctfip, 6019, 6029, 6075, 6077, 6081, 6099, 6111)
replace cntysize=3 if inlist(ctfip, 6001, 6013, 6037, 6059, 6065, 6067, 6071, 6073, 6085)

la def cnty 1 "<500,000" 2 "500,000-1M" 3 "1M+"
la val cntysize cnty
la var cntysize "County size"

*generate one combined race/ethnicity variable.  Responding yes to Hispanic Q takes precedence over response to race Q
g racecmb=race12
replace racecmb=6 if hisp2==1

la def racecmb 1 "White" 2 "Black/African-American" 3 "American Indian/Alaskan Native" 4 "Asian" 5 "Native Hawaiian/Pacific Islander" 6 "Hispanic"
la val racecmb racecmb
la var racecmb "Race/Ethnicity"

*generate variable indicating if adults in the house used transit
*create marker for adults who used transit in the last week
g adult_transit=0 if age2>=18
replace adult_transit=1 if ttrip!=0 & age2>=18
*create marker for each person in household to show that at least one adult used transit
bys sampn: egen tot_tran= total(adult_transit)
g hh_adult_transit=0 
replace hh_adult_transit=1 if tot_tran!=0 & tot_tran!=.
la def adulttranuse 1 "Adult(s) in house use transit" 0 "No adults in house use transit"
la val hh_adult_transit adulttranuse
la var hh_adult_transit "Adult transit use"

*categorize RUCA codes that were merged in
g urbanicity=.
replace urbanicity=1 if inlist(PrimaryRUCACode2010, 1, 2, 3)
replace urbanicity=2 if inlist(PrimaryRUCACode2010, 4, 5, 6)
replace urbanicity=3 if inlist(PrimaryRUCACode2010, 7, 8, 9, 10)
la def urbanicity 1 "Metropolitan" 2 "Micropolitan" 3 "Small town/Rural" 
la val urbanicity urbanicity
la var urbanicity "Urbanicity"


*add labels to other variables

*recode of age
la var age2 "Age"

*gender
la def gend 1 Male 0 Female
la val gend2 gend 
la var gend2 "Gender"

*Hispanic
la def hisp 1 Yes 0 No
la val hisp2 hisp
la var hisp2 "Hispanic"

*race
la def race 1 "White" 2 "Black/African-American" 3 "American Indian/Alaskan Native" 4 "Asian" 5 "Native Hawaiian/Pacific Islander" 
la val race12 race
la var race12 "Race"

*Driver license
la def lic 1 Yes 0 No
la val lic2 lic 
la var lic2 "Driver license"

*Employed
la def emply 1 Yes 0 No
la val emply2 emply
la var emply2 "Employed"

*Residence type
la def resty 1 "Single-family detached" 2 "Duplex/Triplex" 3 "Mobile Home" 4 "2-4 unit building" 5 "5-19 unit building" 6 "20+ unit building"
la val resty3 resty
la var resty3 "Residence type"

*rent or own home
la def own 1 Own 0 Rent
la val own2 own
la var own2 "Family owns home"

*day of week
la def dow 1 Monday 2 Tuesday 3 Wednesday 4 Thursday 5 Friday 6 Saturday 7 Sunday
la val dow dow 
la var dow "Day of week"

*household vehicles
la var hhveh "Vehicles in household"

*recode of hhsize
la var hhsize2 "Household size"

*****************************************
*set up data for complex survey analysis*
*****************************************

svyset sampno [pweight= exptcfperwgt], strata(strata)

************************
*Descriptive statistics*
************************

*Unique trips broken down by various characteristics. Note: trip is unit of analysis in this part.
tabout catage gend2 racecmb lic2 emply2 resty3 own2 dow catincom cathhsize cathhveh hh_adult_transit season urbanicity mode2 ///
if age2<18 & age2>=5 using descript2.txt, svy percent replace c(row) ptotal(single) pop nwt(exptcfperwgt) 

*Descriptives of the overall sample

*Need to reduce dataset so each individual is only represented on one line in order to generate person-level descriptives.
*Unit of analysis is now person.

*Preserve existing file
preserve

*Keep only the first instance of any person-level ID number
duplicates drop sampnperno, force

*change sampling weight for person level analysis.
svyset sampno [pweight= expperwgt], strata(strata)

*Generate descriptives
tabout catage gend2 racecmb lic2 emply2 resty3 own2 dow catincom cathhsize cathhveh hh_adult_transit season urbanicity ///
if age2<18 & age2>=5 using person_descript2.txt, svy percent oneway c(p) replace ptotal(single) pop nwt(expperwgt) 

*Restore old file
restore

***********************************
*Prep data for regression modeling*
***********************************

**Necessary definitions using CHTS coding scheme:
*Active travel: 1 (walking)
*Transit: 15 (local bus/rapid bus), 16 (express bus), 17 (premium bus), 19 (public transit shuttle)
*24 (BART, Metro Red/Purple line, aka heavy rail), 26 (light rail), 27 (street car/cable car)

*First, need to capture trips to reach transit

*Create a variable to flag those trips that preceded a transit trip.  We are assuming
*these are how they accessed transit.

gen accessflag=0
la var accessflag "Flag obs if preceded a transit trip"
order accessflag, after(mode)
bys sampnperno: replace accessflag=1 if inlist(mode[_n+1], 15, 16, 17, 19, 24, 26, 27)

*Create a variable to indicate if access mode was active (walking)

gen activtrans=0
la var activtrans "Active transit access(1) or not (0)"
order activtrans, after(accessflag)
bys sampnperno: replace activtrans=1 if accessflag==1 & mode==1


*Now need to capture trips leaving transit

*Create a variable to flag those trips that followed a transit trip.  We are assuming
*these are how they departed from transit.

gen egressflag=0
la var egressflag "Flag obs if followed a transit trip"
order egressflag, after(mode)
bys sampnperno: replace egressflag=1 if inlist(mode[_n-1], 15, 16, 17, 19, 24, 26, 27)

*Create a variable to indicate if departure mode was active (walking/biking)

bys sampnperno: replace activtrans=1 if egressflag==1 & mode==1

*Create variable capturing transit access and egress trip minutes that were active
g activmin=tripdur*activtrans

*create variable containing summ of activmins for each trip
bys sampnperno: egen totalmins=total(activmin)

*create variable indicating if transit mode was bus (1) or rail (2)
g trantype=.
replace trantype=1 if inlist(mode, 15, 16, 17, 19)
replace trantype=2 if inlist(mode, 24, 26, 27)

*create variable to indicate if transit use was all bus, all rail or both
*first create intermediate variable to average over trantype to serve as 
*indicator for subsequent step
bys sampnperno: egen trantype2=mean(trantype)

*main indicator "trantype3" is based on mean value generated in previous step
g trantype3=.
replace trantype3=1 if trantype2==1
replace trantype3=2 if trantype2==2
replace trantype3=3 if trantype2!=1 & trantype2!=2 & trantype2!=.

la def trantype3 1 "Bus" 2 "Rail" 3 "Bus and rail"
la val trantype3 trantype3
la var trantype3 "Type of transit used"

*keep only over 5 and under 18 and the first observation within each person since this is 
*a person-level analysis, not a trip analysis
keep if age2>=5 & age2<18
bys sampnperno: keep if _n == 1

*generate dichotomous transit use variable
g tranuse=0
replace tranuse=1 if trantype3!=.

la def tranuse 0 No 1 Yes
la val tranuse tranuse
la var tranuse "Transit user"


*********************
*Regression modeling*
*********************

*first model is of transit use v. not
eststo raw_probit: probit tranuse age2 gend2 i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit, vce(clus sampn)

*output regression table
esttab raw_probit using raw_probit2.rtf, label wide star(* 0.05) b(2) se(2) replace

*marginal effects
eststo mfx_probit: margins, dydx(*) post 

*output to table
esttab mfx_probit using probit_margin2.rtf, label nostar not replace

*second model is of transit-related walking minutes.  Adds transit type to list of predictors
eststo raw_nbreg: nbreg totalmins age2 gend2 i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit i.trantype3, vce(clus sampn)

*output regression table
esttab raw_nbreg using raw_nbreg2.rtf, label wide star(* 0.05) b(2) se(2) replace

*marginal effects
eststo mfx_nbreg: margins, dydx(*) post 

*output to table
esttab mfx_nbreg using nbreg_margin2.rtf, label nostar not replace

*********************
*Interaction testing*
*********************

**first model
*ageXgender
probit tranuse c.age2##i.gend2 i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit, vce(clus sampn)
*test interaction term
testparm c.age2#i.gend2

*ageXrace
probit tranuse c.age2##i.racecmb gend2  lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit, vce(clus sampn)
*test interaction term
testparm c.age2#i.racecmb

*genderXrace
probit tranuse c.age2 i.gend2##i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit, vce(clus sampn)
*test interaction term
testparm i.gend2#i.racecmb

*ageXgenderXrace
probit tranuse c.age2##i.gend2##i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit, vce(clus sampn)
*test interaction term
testparm c.age2#i.gend2#i.racecmb


**second model
*ageXgender
nbreg totalmins c.age2##i.gend2 i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit i.trantype3, vce(clus sampn)
*test interaction term
testparm c.age2#i.gend2

*ageXrace
nbreg totalmins c.age2##i.racecmb i.gend2 lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit i.trantype3, vce(clus sampn)
*test interaction term
testparm c.age2#i.racecmb

*genderXrace
nbreg totalmins c.age2 i.gend2##i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit i.trantype3, vce(clus sampn)
*test interaction term
testparm i.racecmb#i.gend2

*ageXgenderXrace
nbreg totalmins c.age2##i.gend2##i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit i.trantype3, vce(clus sampn)
*test interaction term
testparm c.age2#i.gend2#i.racecmb

**Follow-up with significant ageXrace interaction in the transit use model
margins racecmb,  at(age2=(5(2)17))
marginsplot, noci scheme(sj) title(Race-Specific Age Associations with Transit Use) ytitle(Pr(Transit Use))

*test interactions between transit type and other variables in model in the nbreg min/day transit-related activity models
*note: urbanicity is not included because the trantype#urbanicity interaction contains all empty cells.
*note: proceed with caution; interaction with some other variables include many empty cells.
foreach var in c.age2 i.gend2 i.racecmb lic2 emply2 i.resty3 own2 i.catincom c.hhsize2 i.cathhveh i.hh_adult_transit {
nbreg totalmins i.trantype3#`var' c.age2 i.gend2 i.racecmb lic2 emply2 i.resty3 own2 i.catincom hhsize2 i.cathhveh i.urbanicity i.hh_adult_transit i.trantype3, vce(clus sampn)
testparm i.trantype3#`var'
}

**Follow-up with significant transtype interactions in the nbreg model
*hhsize
margins trantype3,  at(hhsize2=(2(2)8))
marginsplot, noci scheme(sj) title(Household Size Association with Transit Related Activity by Transit Type Used) ytitle(Transit-Related Activity (Minutes))
*income
margins trantype3,  at(catincom=(1(1)5))
marginsplot, noci scheme(sj) title(Income Association with Transit Related Activity by Transit Type Used) ytitle(Transit-Related Activity (Minutes))
*residence type
margins trantype3,  at(resty3=(1(1)5))
marginsplot, noci scheme(sj) title(Residence Type Association with Transit Related Activity by Transit Type Used) ytitle(Transit-Related Activity (Minutes))
*employment status
margins trantype3,  at(emply2=(0 1))
marginsplot, noci scheme(sj) title(Employment Status Association with Transit Related Activity by Transit Type Used) ytitle(Transit-Related Activity (Minutes))
*race
margins trantype3,  at(race=(1 2 3 4 6 ))
marginsplot, noci scheme(sj) title(Race Association with Transit Related Activity by Transit Type Used) ytitle(Transit-Related Activity (Minutes))
*gender
margins trantype3,  at(gender=(0 1))
marginsplot, noci scheme(sj) title(Gender Association with Transit Related Activity by Transit Type Used) ytitle(Transit-Related Activity (Minutes))
*age
margins trantype3,  at(age=(1(2)18))
marginsplot, noci scheme(sj) title(Age Association with Transit Related Activity by Transit Type Used) ytitle(Transit-Related Activity (Minutes))



