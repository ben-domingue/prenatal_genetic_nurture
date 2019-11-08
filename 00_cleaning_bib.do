{\rtf1\ansi\ansicpg1252\cocoartf1671\cocoasubrtf600
{\fonttbl\f0\froman\fcharset0 TimesNewRomanPSMT;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\ri0\partightenfactor0

\f0\fs24 \cf0 clear all\
set more off, perm\
set matsize 5000\
sysdir set PLUS "S:\\group\\bdomingu\\ado\\plus"\
sysdir set PERSONAL "S:\\group\\bdomingu\\ado\\personal"\
\
global edu "S:\\group\\bdomingu\\BiB\\data\\edu_record"\
global eclipse "S:\\group\\bdomingu\\BiB\\data\\eclipse"\
global start "S:\\group\\bdomingu\\BiB\\data\\starting_school"\
global pgs "S:\\group\\bdomingu\\BiB\\data\\pgs"\
global biox "S:\\group\\bdomingu\\BiB\\data\\bio_xwalk"\
global base "S:\\group\\bdomingu\\BiB\\data\\baseline_survey"\
global dofile "S:\\group\\bdomingu\\BiB\\dofile"\
global out "S:\\group\\bdomingu\\BiB\\data\\clean"\
global results "S:\\group\\bdomingu\\BiB\\results"\
\
use "$dofile\\data_merge.dta"\
\
********************************************************\
* identify ancestry\
********************************************************\
***identify british and pakistani by self report\
gen bri_report=eth0eth9gp==1\
gen pak_report=eth0eth9gp==7 \
\
*self-identify as white, have PC1 -.002 and -.005 and PC2 .004 and .011\
gen bri=0\
replace bri=1 if m_pc_all_1<-.002 & m_pc_all_1>-.005 & m_pc_all_2>.004 ///\
			& m_pc_all_2<.011 & eth0eth9gp==1\
** sam please confirm this. \
gen pak=0\
replace pak=1 if m_pc_all_1>-.002 | m_pc_all_1<-.005 | m_pc_all_2<.004 ///\
			| m_pc_all_2>.011 & eth0eth9gp==7\
\
*counting participant ancestries\
display 2077+3696 //(european plus pakistanis)\
*5773 (this many are either or)\
display 6124-5773\
*351 (this many are neither bri nor pak)\
*so 60.35% pakistani, 33.92% european\
display 351/6124 //5.73 neither \
\
*checking it adds up\
display 60.35+33.65+5.73 \
\
***************************************************************************\
*** Clean Pregnancy Baseline Questionnaire Variables\
***************************************************************************\
***age\
rename agemy_mbqall age \
\
***pgs\
center m_pgs_ea, standardize inplace\
center c_pgs_ea, standardize inplace\
\
***education\
recode edu0mumeuk (1=1) (2=2) (3=3)(4=3)(5=1)(6=2)(7=3)(8=4)(9=4)(10=4) ///\
				  (11=.) (12=.) (13=1) (14=.)\
rename edu0mumeuk edu\
\
***bmi\
rename mms0mbkbmi bmi\
center bmi, standardize\
\
***health\
rename ghq0fctrsc hlth\
center hlth, standardize\
\
*cohabitation\
tab hhd0cohabt\
gen solo=0 if hhd0cohabt<.\
replace solo=1 if hhd0cohabt>2 & hhd0cohabt<.\
\
***employment\
replace job0mumemp=0 if job0mumemp>1\
rename job0mumemp job\
\
***cigerrettes\
replace smk0smkprg=0 if smk0smkprg>1\
rename smk0smkprg cig\
\
***smoke exposure\
gen smk=0 if smk0expcig==2\
replace smk=1 if smk0expcig!=2 & smk0expcig!=.\
drop smk0expcig\
\
***alcohol\
gen alc3=0\
replace alc3=1 if inrange(alc0drfr3m, 1, 2)\
gen alc4=0\
replace alc4=1 if inrange(alc0dr4thm, 1, 2)\
gen alc=0\
replace alc=1 if alc3==1 | alc4==1\
\
***drugs\
tab drg0drguse\
replace drg0drguse=. if drg0drguse==3\
replace drg0drguse=1 if drg0drguse==1.5\
replace drg0drguse=0 if drg0drguse==2\
rename drg0drguse drg\
tab drg\
\
***vitamins\
gen vit=0 if vit0vitipr==2\
	replace vit=1 if vit0vitipr<2\
drop vit0vitipr\
\
***maternity leave\
gen mlv=0 if job0matscl<.\
replace mlv=1 if inrange(job0matscl, 1, 1.9)\
drop job0matscl\
tab mlv\
\
***index of multiple deprivation\
rename imddecileswithinbradford mdi\
recode mdi (1=10)(2=9)(3=8)(4=7)(5=6)(6=5)(7=4)(8=3)(9=2)(10=1)\
\
********************************************************************************\
*** Create Composite Measures\
********************************************************************************\
*governmental benefits\
recode ben0carall ben0chdben ben0chdtxc ben0disbla ben0incapb ///\
	   ben0incsup ben0intjsa ben0otherb (1=1)(2=0)			\
pca ben0carall ben0chdben ben0chdtxc ben0disbla ben0incapb ///\
	   ben0incsup ben0intjsa ben0otherb\
predict benefits_received\
center benefits_received, standardize \
\
*subjective financial difficulty\
recode fin0manfin(1=1)(2=2)(3=3)(4=4)(5=5)(6=.) \
gen sub_fin_diff=fin0manfin\
center sub_fin_diff, standardize\
\
*financial stress \
recode fin0bllctx fin0bllelc fin0bllfeu fin0bllgas fin0bllins ///\
	   fin0bllohp fin0blltph fin0blltvb fin0bllwat (1=1)(2=0)\
pca fin0bllctx fin0bllelc fin0bllfeu fin0bllgas fin0bllins ///\
	fin0bllohp fin0blltph fin0blltvb fin0bllwat\
predict behind_bills\
center behind_bills, standardize \
\
*maternal stress\
pca ghq0ques10 ghq0ques11 ghq0ques12 ghq0ques13 ghq0ques14 ghq0ques26\
predict maternal_strain\
center maternal_strain, standardize \
\
*sleep problems.\
pca ghq0ques08 ghq0ques09 \
predict sleep_probs\
center sleep_probs, standardize \
\
*Caffeine-generate missing variable identifier\
global caf cdr0clcfpd cdr0cldcpd cdr0dccfpd cdr0dcdcpd cdr0fccfpd cdr0fcdcpd ///\
	       cdr0htcfpd cdr0htdcpd cdr0iccfpd cdr0icdcpd cdr0ktcfpd cdr0ktdcpd ///\
	       cdr0clcfpw cdr0cldcpw cdr0dccfpw cdr0dcdcpw cdr0fccfpw cdr0fcdcpw ///\
	       cdr0htcfpw cdr0iccfpw cdr0icdcpw cdr0ktdcpw ///\
\
gen miss_caf=1 \
foreach var in $caf \{\
	replace miss_caf=0 if `var'!=. \
	replace `var'=0 if `var'==.\
\} \
*miligrams caffeine per day (plus each weekly catagory divided by 7) \
gen caff_day = (cdr0clcfpd*39)+(cdr0cldcpd*0)+(cdr0dccfpd*30)+(cdr0dcdcpd*0)+ ///\
	(cdr0fccfpd*85)+ (cdr0fcdcpd*14)+ (cdr0htcfpd*47)+(cdr0htdcpd*5)+(cdr0iccfpd*47)+ ///\
	(cdr0icdcpd*14)+(cdr0ktcfpd*30)+(cdr0ktdcpd*5)+((cdr0clcfpw*39)/7)+((cdr0cldcpw*0)/7)+ ///\
	((cdr0dccfpw*30)/7)+((cdr0dcdcpw*0)/7)+((cdr0fccfpw*85)/7)+((cdr0fcdcpw*14)/7) + ///\
	((cdr0htcfpw*47)/7)+((cdr0iccfpw*47)/7)+((cdr0icdcpw*14)/7)+((cdr0ktdcpw*5)/7)\
replace caff_day=. if miss_caf==1\
center caff_day, standardize\
\
\
**********************************************************************************************************\
*** CLEAN EYSFP  EDUCATION VARIABES\
**********************************************************************************************************\
***gen indicators for which first stage test was taken (pre or post 2016)\
gen ks1_post16=0\
replace ks1_post16=1 if has_edks12==1\
gen ks1_pre16=abs(ks1_post16-1)\
\
***cleaning key stage variables.\
foreach var in ks1_pre2016_maths ks1_pre2016_reading ks1_pre2016_writing ///\
	ks1_post2016_maths ks1_post2016_reading ks1_post2016_writing \{\
	replace `var'=. if `var'>5 & `var'<.\
	center `var', standardize\
\}\
\
foreach var in ks1_pre2016_science ks1_post2016_science \{\
	replace `var'=. if `var'>2 & `var'<.\
	center `var', standardize\
\}\
\
gen c_ks1_math=c_ks1_pre2016_maths if ks1_post16==0\
replace c_ks1_math=c_ks1_post2016_maths if ks1_post16==1\
gen c_ks1_read=c_ks1_pre2016_read if ks1_post16==0\
replace c_ks1_read=c_ks1_post2016_read if ks1_post16==1\
gen c_ks1_science=c_ks1_pre2016_science if ks1_post16==0\
replace c_ks1_science=c_ks1_post2016_science if ks1_post16==1\
gen c_ks1_writing=c_ks1_pre2016_writing if ks1_post16==0\
replace c_ks1_writing=c_ks1_post2016_writing if ks1_post16==1\
egen ks1=rmean(c_ks1_math c_ks1_read c_ks1_science c_ks1_writing)\
egen zks1 = std(ks1) \
center ks1, standardize inplace\
\
*EYSFP-generate indicators for which EYFSP was taken (pre or post 2013)\
gen eyfsp_post2013=0\
replace eyfsp_post2013=1 if has_eyfsp2==1\
gen eyfsp_pre2013=1 if has_eyfsp1==1\
\
***cleaning variables from post 2013.\
foreach var in eyfsp_post2013_com_elg01 eyfsp_post2013_com_elg02 eyfsp_post2013_com_elg03 ///\
		eyfsp_post2013_exp_elg16 eyfsp_post2013_exp_elg17 eyfsp_post2013_lit_elg09 ///\
		eyfsp_post2013_lit_elg10 eyfsp_post2013_mat_elg11 eyfsp_post2013_mat_elg12 ///\
		eyfsp_post2013_phy_elg04 eyfsp_post2013_phy_elg05 eyfsp_post2013_pse_elg06 ///\
		eyfsp_post2013_pse_elg07 eyfsp_post2013_pse_elg08 eyfsp_post2013_utw_elg13 ///\
		eyfsp_post2013_utw_elg14 eyfsp_post2013_utw_elg15 \{\
	replace `var'=. if `var'>3\
	center `var', standardize\
\}\
\
*creating subscale and total scores for EYFSP\
egen eyfsp_post2013_communication=rmean(c_eyfsp_post2013_com_elg01 ///\
					c_eyfsp_post2013_com_elg02 c_eyfsp_post2013_com_elg03)\
egen eyfsp_post2013_arts=rmean(c_eyfsp_post2013_exp_elg16 c_eyfsp_post2013_exp_elg17)\
egen eyfsp_post2013_literature=rmean(c_eyfsp_post2013_lit_elg09 c_eyfsp_post2013_lit_elg10)\
egen eyfsp_post2013_math=rmean(c_eyfsp_post2013_mat_elg11 c_eyfsp_post2013_mat_elg12)\
egen eyfsp_post2013_physical=rmean(c_eyfsp_post2013_phy_elg04 c_eyfsp_post2013_phy_elg05)\
egen eyfsp_post2013_socioemotional=rmean(c_eyfsp_post2013_pse_elg06 ///\
					c_eyfsp_post2013_pse_elg07 c_eyfsp_post2013_pse_elg08)\
egen eyfsp_post2013_understand=rmean(c_eyfsp_post2013_utw_elg13 c_eyfsp_post2013_utw_elg14 ///\
					c_eyfsp_post2013_utw_elg15)\
\
egen eyfsp=rmean(eyfsp_post2013_communication eyfsp_post2013_arts ///\
				 eyfsp_post2013_literature eyfsp_post2013_math ///\
				 eyfsp_post2013_physical eyfsp_post2013_socioemotional /// \
				 eyfsp_post2013_understand)\
egen zeyfsp = std(eyfsp) \
center eyfsp, standardize inplace\
\
*Create percentile variables\
	xtile ks1_pct = (ks1), nq(100)\
	xtile eyfsp_pct = eyfsp, nq(100)\
	\
*****************************************************************************\
*** Clean eclipse data\
*****************************************************************************\
*recode birth small and large for gestational age.\
recode eclsgaukwho (1=0)(2=1) \
recode ecllgaukwho (1=0)(2=1) \
\
*standardize birthweight \
center eclbirthwt, standardize inplace\
\
*create mean APGAR score\
egen apgar=rowmean(eclapgar1m eclapgar5m)\
\
******************************************************************************\
*create composites\
pca bmi hlth vit smk cig alc c_caff_day drg sleep_probs\
predict hlth_comp\
center hlth_comp, standardize inplace\
\
pca edu solo job mlv mdi benefits_received sub_fin_diff\
predict ses_comp\
center ses_comp, standardize inplace 		\
\
*Exploratory factor analysis with birth characteristics\
\
pca apgar eclgestday eclsgaukwho ecllgaukwho eclbirthwt\
predict birth_comp\
center birth_comp, standardize inplace\
\
revrs ses_comp, replace\
revrs hlth_comp, replace\
\
\
******************************************************************************\
\
********************************************************************************\
*** label variables.\
********************************************************************************\
label var ses_comp "Maternal SES"\
label var hlth_comp "Maternal Health"\
label var m_pgs_ea "Maternal Education PGS"\
label var c_pgs_ea "Child Education PGS"\
label var bmi "Body Mass Index"\
label var hlth "Health Factor Score"\
label var vit "Vitamin Use"\
label var smk "Smoke Exposure"\
label var cig "Cigarette Use"\
label var alc "Alcohol Use"\
label var drg "Drug Use"\
label var solo "Single"\
label var job "Employed"\
label var mlv "Maternity Leave"\
label var mdi "Neighborhood Deprivation"\
label var sleep_probs "Sleep Problems" \
label var behind_bills "Behind on Bills" \
label var sub_fin_diff "Perceived Financial Difficulty" \
label var benefits_received "Benefits Received"\
label var maternal_strain "Maternal Stress"\
label var caff_day "Caffeine Mg/Day"\
label var c_caff_day "Caffeine Mg/Day"\
label var eclgestday "Gestational Age (days)" \
label var eclapgar1m "APGAR score at 1 min" \
label var eclapgar5m "APGAR score at 5 min" \
label var apgar "APGAR Score Mean"\
label var eclbirthwt "Birthweight (g)"\
label var eclsgaukwho "Small for gestational age"\
label var ecllgaukwho "Large for gestational age" \
label var eyfsp "EYFSP Child Development"\
label var ks1 "Key Stage Assessment"\
label var phonics_mark1 "Phonics Assessment"\
label var age "Maternal Age"\
\
save "$dofile\\data_clean.dta", replace\
\
use "$dofile\\data_clean.dta"\
\
reg eyfsp m_pgs_ea c_pgs_ea m_pc_bri* if bri==1\
reg ks1 m_pgs_ea c_pgs_ea m_pc_bri* if bri==1\
\
reg zeyfsp m_pgs_ea c_pgs_ea m_pc_bri* if bri==1\
reg zks1 m_pgs_ea c_pgs_ea m_pc_bri* if bri==1\
\
}