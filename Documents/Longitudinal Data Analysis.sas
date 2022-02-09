* Result and Discussion;
* IBRAHIM SANUSI*;

/*creating SAS Format Library*/ 
libname H1 "C:\Users\sanus\Downloads\GTY 750\Codes and Format\Dataset for HW1";

/*To prevent formating error, I use cntlin to use information from control dataset */ 
proc format library=H1 
cntlin=H1.sasfmts; run;
options fmtsearch = (H1.formats); 


/* 1.	How many observations and variables are in the dataset?*/
/*This code was used to explore the contents of the data*/
proc contents data=H1.randhrs1992_2016v2; run;


/* 2.	Get rid of all variables related to spouse and household (Note: using : , - , -- are helpful). 
        Keep the variable HHIDPN which is the unique key.*/
/* This only keep the response 'R' and 'I' variables*/
data step1; set H1.randhrs1992_2016v2 
(keep= R: I: HHIDPN );
run;
proc print data=step1 (obs=10); run;


/* 3a.	The baseline for my study is wave 4. Hence, delete all participants not present in wave 4. */
/* This code keeps all response variables at wave >4  */
data step2; set step1;
if INW4 ne 1 then DELETE;
run;
proc print data=step2 (obs=10); run;

/* 4.	My outcome and exposure of interest are stroke and diabetes, respectively. mental health raeduc drink psych lung heart 
        proxy smoking bp bmi age raracem rahispan ragender are the confounding variables I want to consider. 
        Hence, remove all the variables that I may not need. (Note: using : , - , -- are helpful).*/
/* 5.	Using arrays convert the wide form of data into long-form */
/* 6.	Create a new variable named "wave" that indicates the wave-number of each survey. How many waves are there?*/

/* This code combine Questions #4,#5 and #6 and the array get rid off waves 1-3, convert into long form, create a variable that represents time “a” and rename the variable "a" as "wave" */
Data step3(rename=(a=Wave)); set step2;
ARRAY AR_all  {11, 4:13}  r4BMI--r13BMI r4diabe--r13diabe r4agey_m--r13agey_m r4stroke--r13stroke r4hibpe--r13hibpe r4smokev--r13smokev 
r4proxy--r13proxy r4heart--r13heart r4lung--r13lung r4psych--r13psych r4drink--r13drink;  
DO a = 4 to 13;
BMI = AR_all {1, a} ;
Diabetes = AR_all {2, a} ;
Age = AR_all {3, a} ;
Stroke = AR_all {4, a} ;
HBP = AR_all {5, a} ;
Smoking = AR_all {6, a} ;
Proxy = AR_all {7, a} ;
Heart = AR_all {8, a} ;
Lung = AR_all {9, a} ;
Psychiatric = AR_all {10, a} ;
Drink = AR_all {11, a} ;
output ;end;
keep  a   raeduc Drink Psychiatric Lung Heart Proxy Smoking HBP Stroke BMI Diabetes Age raracem rahispan ragender HHIDPN;
run;
proc print data=step3 (obs=10); run;

/*3b. I am not interested in a proxy response.  Hence, delete all the proxy responses */
/* This code answer the second part of question #3 by deleting all the proxy response */
Data step4; set step3;
if proxy=1 then delete;
run;
proc print data=step4 (obs=10); run;

/* 7.	Assign all don't know, missing, refused, etc., as a missing value.*/
/* This code assign all don't know, missing, refused, etc., as a missing value.*/
Data step5; set step4;
array _rdmiss  raeduc Drink Psychiatric Lung Heart Proxy Smoking HBP Stroke BMI Diabetes Age raracem rahispan ragender;
		        	do over _rdmiss;
		        	if _rdmiss in ('.D', '.M', '.R', '.Q','.S','.T','.U','.V') then _rdmiss=.;
end; run;
proc print data=step5 (obs=10); run;

/* 8.	Following the standard weight status associated with BMI ranges for adults FROM CDC, 
        recode BMI into different weight categories by creating a new variable named "weight."*/
/*  This code recoded BMI into different weight categories and create a new variable named "weight."*/
Data step6; set step5;
if bmi=. then weight=.;
	else if bmi < 18.5 then weight = 1;
	else if 18.5 <= bmi <= 24.9 then weight = 2;
	else if 25 <= bmi <= 29.9 then weight = 3;
	else if bmi >= 30 then weight = 4;
	run;
proc print data=step6 (obs=10); run;

/* 9.	Label the variable "weight" as "Self-reported weight status*/
/* 10.	Format the variable "weight" by using the CDC information*/
/*This code combine Questions #9 and #10 and was used to label and format the new variable 'weight' */;
Proc format;
value weight
1='Underweight'
2='Normal or Healthy Weight'
3='Overweight'
4='Obese';
run;
proc print data=step6 (obs=10); run;

Data step7; set step6;
label
weight='Self-reported weight status';
format
weight weight.;
run;
proc print data=step7 (obs=10) label; run;

*11. Regrouping and formatting age;
Data step8; set step7;
if Age=. then Age=.;
if Age < 55 then delete;
if 55 <= Age <65 then Age=1 ; *55-65 group;
else if 65 <= Age <75 then Age=2 ; *65-75 group;
else Age=3 ; *75 and over group;
run;

Proc format;
value Age
1='55 - 65'
2='65 - 75'
3='>75';
run;
Data step9; set step8;
format
Age Age.;
run;
proc print data=step9 (obs=10) label; run;

*12. Regrouping and formatting raracem and rahispa to form "race";
data step10; set step9;
if RAHISPAN=1 then Race =3;
else if RAHISPAN=0 and RARACEM =1  then Race =1; *NHW;
else if RAHISPAN=0 and RARACEM = 2 then Race =2; *NHB;
else Race =4 ;
run;
proc format ;
value Race 
1 ='NHW' 
2= 'NHB' 
3= 'Hispanic'
4='NH others';
run;
Data step11; set step10;
format
Race Race.;
run;
proc print data=step11 (obs=10) label; run;

*13. Regrouping and formatting education;
Data step12; set step11;
if RAEDUC in (2,3) then Education=2;
else if RAEDUC in (1) then Education=1;
else if RAEDUC in (4) then Education=3;
else if RAEDUC in (5) then Education=4;
else Education=. ; 
run;
proc format ;
value raGENDER 
1 = 'Male' 
2 = 'Female';
value Education 
1= 'Less than high school'  
2= 'High school (or GED)'   
3='Some college'   
4='College and above'  ;
run;
Data step13; set step12;
format
Education Education.
raGENDER raGENDER.
run;
proc print data=step13 (obs=10); run;

*14. Regrouping and formatting Lung, psych;
Data step14; set step13;
if lung in (0) then lung=0;
else lung =1 ; 
if Psychiatric in (0) then Psychiatric=0;
else Psychiatric =1 ; 
if heart in (0) then heart=0;
else heart =1 ; 
run;
proc print data=step14 (obs=10); run;

/*15  Desciptive statistics for baseline*/
proc freq data=step14;
where wave =4;
table stroke Age*stroke RAGENDER diabetes weight Race education Psychiatric drink lung heart smoking hbp wave/list missing;
run;

/*16  Desciptive statistics for by outcome variable*/
proc freq data=step14;
where wave =4;
table stroke*stroke Age*stroke RAGENDER*stroke diabetes*stroke weight*stroke Race*stroke education*stroke Psychiatric*stroke drink*stroke lung*stroke heart*stroke smoking*stroke hbp*stroke/list missing;
run;

*17. Running GEE Exchangeable;
proc gee data=step14;
class HHIDPN  RAGENDER weight diabetes Age Race education Psychiatric drink lung heart smoking hbp;
model stroke = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp/ dist=bin link=logit;
repeated subject=HHIDPN / corr=exch  ;
run;

*18. Running GEE Autoregressive;
proc gee data=step14;
class HHIDPN  RAGENDER weight diabetes Age Race education Psychiatric drink lung heart smoking hbp;
model stroke = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp/ dist=bin link=logit;
repeated subject=HHIDPN / corr=AR;
run;

*18. Running GEE Unstructured;
proc gee data=step14;
class HHIDPN  RAGENDER weight diabetes Age Race education Psychiatric drink lung heart smoking hbp;
model stroke = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp/ dist=bin link=logit;
repeated subject=HHIDPN / corr=UN;
run;

*19. Running GEE Mdependent;
proc gee data=step14;
class HHIDPN  RAGENDER weight diabetes Age Race education Psychiatric drink lung heart smoking hbp;
model stroke = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp/ dist=bin link=logit;
repeated subject=HHIDPN / corr=MDEP;
run;

*20. Running GEE Independent;
proc gee data=step14;
class HHIDPN  RAGENDER weight diabetes Age Race education Psychiatric drink lung heart smoking hbp;
model stroke = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp/ dist=bin link=logit;
repeated subject=HHIDPN / corr=IND corrw;
run;

*21. Running Proc Logistic to obtain class level information;
proc logistic data=step14;
class HHIDPN  RAGENDER weight diabetes Age Race education Psychiatric drink lung heart smoking hbp wave;
model stroke = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp wave; 
run;

*21. Running Proc Logistic to obtain class level information;
proc logistic data=step14;
class HHIDPN  RAGENDER (ref='Male') weight(ref='Normal or Healthy Weight') diabetes(ref='0') Age(ref='55 - 65') Race (ref='NH others')
education(ref='Some college') Psychiatric(ref='0') drink(ref='0') lung(ref='0') heart(ref='0') smoking(ref='0') hbp(ref='0') wave(ref='4');
model stroke = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp wave; 
run;

*21. Calculating for Odd Ratio (OR)GEE Exchangeable;
proc gee data=step14;
class HHIDPN  RAGENDER (ref='Female') weight(ref='Normal or Healthy Weight') diabetes(ref='0') Age(ref='55 - 65') Race (ref='NH others')
education(ref='College and above') Psychiatric(ref='0') drink(ref='0') lung(ref='0') heart(ref='0') smoking(ref='0') hbp(ref='0') wave(ref='4');
model stroke(EVENT='1') = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp wave/ dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR Age 65 - 75" age -1 1	0/ exp cl;
estimate "OR Age  >75" age -1 0	1/ exp cl;
estimate "OR Male" RAGENDER 1 -1 / exp cl;
estimate "OR Diabetes" Diabetes 1 -1 / exp cl;
estimate "OR Obese" weight -1 1 0 0/ exp cl;
estimate "OR Overweight" weight -1 0 1 0/ exp cl;
estimate "OR Underweight" weight -1 0 0 1/ exp cl;
estimate "OR Hispanics" race 1 -1 0 0/ exp cl;
estimate "OR NHB" race   0 -1 1 0/ exp cl;
estimate "OR NHW" race   0 -1 0 1/ exp cl;
estimate "OR College and above" Education   1 0 0 -1 / exp cl;
estimate "OR High School (or GED)" Education   0 1 0 -1/ exp cl;
estimate "OR Less than High School" Education   0 0 1 -1/ exp cl;
estimate "OR Drink" Drink 1 -1 / exp cl;
estimate "OR Psychiatric" Psychiatric 1 -1 / exp cl;
estimate "OR Lung" Lung 1 -1 / exp cl;
estimate "OR Heart" Heart 1 -1 / exp cl;
estimate "OR Smoking" Smoking 1 -1 / exp cl;
estimate "OR HBP" HBP 1 -1 / exp cl;
estimate "OR wave5" wave 1 0 0 0 0 0 0 0 0 -1 / exp cl;
estimate "OR wave6" wave 0 1 0 0 0 0 0 0 0 -1 / exp cl;
estimate "OR wave7" wave 0 0 1 0 0 0 0 0 0 -1 / exp cl;
estimate "OR wave8" wave 0 0 0 1 0 0 0 0 0 -1 / exp cl;
estimate "OR wave9" wave 0 0 0 0 1 0 0 0 0 -1 / exp cl;
estimate "OR wave10" wave 0 0 0 0 0 1 0 0 0 -1 / exp cl;
estimate "OR wave11" wave 0 0 0 0 0 0 1 0 0 -1 / exp cl;
estimate "OR wave12" wave 0 0 0 0 0 0 0 1 0 -1 / exp cl;
estimate "OR wave13" wave 0 0 0 0 0 0 0 0 1 -1 / exp cl;
run;

*22. run corelation;
proc corr data=step14 noprob outp=Corr;
var  RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp wave;
run;

proc print data=Corr; where _TYPE_ in ("CORR");run;

*22. Collinearity check;
proc reg data=step14  plots (MAXPOINTS=NONE) ;
model Stroke = RAGENDER  diabetes weight Age Race education Psychiatric drink lung heart smoking hbp wave /vif tol;
    run;quit;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for gender;
proc gee data=step14;
class HHIDPN  RAGENDER (ref='Female');
model stroke(EVENT='1') = RAGENDER / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR Male" RAGENDER 1 -1 / exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for diabetes;
proc gee data=step14;
class HHIDPN  diabetes(ref='0');
model stroke (EVENT='1') = diabetes / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR Diabetes" Diabetes 1 -1 / exp cl;
run;


*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for weight;
proc gee data=step14;
class HHIDPN  weight(ref='Normal or Healthy Weight');
model stroke (EVENT='1')= weight / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR Obese" weight -1 1 0 0/ exp cl;
estimate "OR Overweight" weight -1 0 1 0/ exp cl;
estimate "OR Underweight" weight -1 0 0 1/ exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for age;
proc gee data=step14;
class HHIDPN  Age(ref='55 - 65');
model stroke (EVENT='1')= Age / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR Age 65 - 75" age -1 1	0/ exp cl;
estimate "OR Age  >75" age -1 0	1/ exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for race;
proc gee data=step14;
class HHIDPN  Race (ref='NH others');
model stroke (EVENT='1')= Race / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR Hispanics" race 1 -1 0 0/ exp cl;
estimate "OR NHB" race   0 -1 1 0/ exp cl;
estimate "OR NHW" race   0 -1 0 1/ exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for education;
proc gee data=step14;
class HHIDPN  education(ref='College and above');
model stroke (EVENT='1')= education / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR College and above" Education   1 0 0 -1 / exp cl;
estimate "OR High School (or GED)" Education   0 1 0 -1/ exp cl;
estimate "OR Less than High School" Education   0 0 1 -1/ exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for lung;
proc gee data=step14;
class HHIDPN  Lung(ref='0');
model stroke (EVENT='1')= Lung / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR Lung" Lung   1 -1 / exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for heart;
proc gee data=step14;
class HHIDPN  Heart(ref='0');
model stroke (EVENT='1')= Heart / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "Heart" Heart   1 -1 / exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for hbp;
proc gee data=step14;
class HHIDPN  hbp(ref='0');
model stroke (EVENT='1')= hbp / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "hbp" hbp   1 -1 / exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for psychiatric;
proc gee data=step14;
class HHIDPN  psychiatric(ref='0');
model stroke (EVENT='1')= psychiatric / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "psychiatric" psychiatric  1 -1 / exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for drink;
proc gee data=step14;
class HHIDPN  drink(ref='0');
model stroke (EVENT='1')= drink / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "drink" drink  1 -1 / exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for smoking;
proc gee data=step14;
class HHIDPN  smoking(ref='0');
model stroke (EVENT='1')= smoking / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "smoking" smoking  1 -1 / exp cl;
run;

*21. Calculating for Raw Odd Ratio (OR)GEE Exchangeable for wave;
proc gee data=step14;
class HHIDPN  wave(ref='4');
model stroke (EVENT='1')= wave / dist=bin link=logit;
repeated subject=HHIDPN / corr=exch;
estimate "OR wave5" wave 1 0 0 0 0 0 0 0 0 -1 / exp cl;
estimate "OR wave6" wave 0 1 0 0 0 0 0 0 0 -1 / exp cl;
estimate "OR wave7" wave 0 0 1 0 0 0 0 0 0 -1 / exp cl;
estimate "OR wave8" wave 0 0 0 1 0 0 0 0 0 -1 / exp cl;
estimate "OR wave9" wave 0 0 0 0 1 0 0 0 0 -1 / exp cl;
estimate "OR wave10" wave 0 0 0 0 0 1 0 0 0 -1 / exp cl;
estimate "OR wave11" wave 0 0 0 0 0 0 1 0 0 -1 / exp cl;
estimate "OR wave12" wave 0 0 0 0 0 0 0 1 0 -1 / exp cl;
estimate "OR wave13" wave 0 0 0 0 0 0 0 0 1 -1 / exp cl;
run;
