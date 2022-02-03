/*program name: project.sas                             */
/*programmer: Jing Xie                                        */
/*date: 04-16-2020                                            */
/*description: 1820 project                            */

/*set sas options to make the output easier to read*/

options ls=72 ps=65 /*nodate*/ nonumber;

/*
the file is atuomatically produced using the import wizard through the file drop down menu in sas.
importing data into sas using the virtual lab from a local drive on the computer.
*/
proc import out=work.hers
           datafile = "H:\HERS.xls"
		   dbms=xls replace;
	 getnames=yes;
	 datarow=2;
	 label LDL = "Low density lipoprotein cholesterol"  /* Low density lipoprotein cholesterol (mg/dl)*/
		  DBP = "Diastolic blood pressure" /*Diastolic blood pressure (mmHg)*/
          SBP = "Systolic blood pressure" /*Systolic blood pressure(MMHg)*/ 
          age = "age"   /*age  in years*/
		  TG  = "triglycerides"   /*triglycerides(mg/dl)*/ 
       	  BMI = "Body mass Index"   /*Body mass Index  (Kg/m2)*/
		  HDL = "High density lipoprotein"   /*High density lipoprotein cholesterol (mg/dl)*/
		  glucose="Glucose level" /*Glucose level (mg/dl)*/
		  smoking ="Smoking Yes=1 No=0" 
          raceth = "race-ethnicity 1= Caucasian 2=African-American 3=others";  /*1= Caucasian 2=African-American 3=others*/	  
run;

data hers;    /*replace the missing by averager of the variable */
	set work.hers;
	if DBP = . then DBP=73.4867897;
	if TG = . then TG=167.3550489;
	if BMI =. then BMI=30.3353493;
	if HDL = . then HDL=54.0390879;
	if LDL = . then LDL=148.4382918;
run;

/*qualitative data analysis preparetion*/
/*Add dummy variables to the dataset*/
data hers;
	set hers;
	id+1;
	if raceth=1 then caucasian=1; else caucasian=0;
	if raceth=2 then african=1; else african=0;
	/* set other as reference group*/
run;
proc print data=hers(obs=20);
	title "HERS dataset (first 20 ob)";
run;


/* split the dataset into training set and testing set*/
proc surveyselect data=hers out=training
	seed=3832
	method=srs   /*simple random sample*/
	samprate=0.66  /*proportion in training dataset*/
	rep=1;
run;

data trainingid;
	set training;
	train=1;
	keep id train;
run;

proc sort data=trainingid;
	by id;
run;

proc sort data=hers;
	by id;
run;

data validation;
	merge hers trainingid;
		by id;
	if train eq 1 then delete;
	drop train;
run;
 
proc print data=validation (obs=10);
run;

/*ods rtf file= 'H:\PH1820-Appl.l.regression\Group assignement\groupAss-out3.rtf';*/
/*this tells sas to save the output to an .rtf file to your local drive using the paralells client*/
 
/*scatter plot matrix of all the quantitative variables*/
title "scatterplot matrix of quantitative variables in training data";
proc sgscatter data = training;
	matrix DBP SBP age TG BMI HDL glucose LDL;
run;

proc corr data = training spearman;
	var DBP SBP age TG BMI HDL glucose LDL;
run;

title "scatterplot matrix of categorical data in training data";
proc sgscatter data = training;
	matrix smoking raceth LDL;
run;

/*check normality of LDL*/
title "check normality of LDL";
proc univariate data = training normal;
	var LDL;
	histogram /normal;
	qqplot /normal (mu=est sigma=est);
run;

/*fit preliminary model*/
title "preliminary model full model first order";
proc reg data = training;
	model LDL = DBP SBP age TG BMI HDL glucose smoking caucasian african/vif;
	output out =trainingregout1 r=resids p=preds;
run;

/*check normality of residuals*/
title "check normality of residuals";
proc univariate data = trainingregout1 normal;
	var resids;
	histogram /normal;
	qqplot /normal (mu=est sigma=est);
run;

title "Scatter plots of residuals in traing set with loess smooth";
proc sgscatter data = trainingregout1;
	plot resids*DBP resids*SBP resids*age resids*TG resids*BMI resids*HDL resids*glucose resids*smoking resids*caucasian resids*african resids*preds/ loess=(smooth=0.5);
run;

data trainingregout1;
	set trainingregout1;
	sqresid=resids**2;
	semistudresid=resids/37.87214; /*looked up root mse in sas output*/;
run;

proc sgscatter data = trainingregout1;
	title "Scatter plots of semistudentized residuals with loess smooth";
	plot semistudresid*glucose semistudresid*DBP semistudresid*SBP semistudresid*age semistudresid*TG semistudresid*BMI semistudresid*HDL semistudresid*smoking semistudresid*caucasian semistudresid*african semistudresid*preds/ loess=(smooth=0.5);
run;

/*look for constant variance*/
proc sgscatter data = trainingregout1;
	title "Scatterplots of squared residuals with lowess smooth";
	plot sqresid*DBP sqresid*SBP sqresid*age sqresid*TG sqresid*BMI sqresid*HDL sqresid*glucose sqresid*smoking sqresid*caucasian sqresid*african sqresid*preds/ loess=(smooth=0.5);
run;

ods rtf close;*closes rtf file for printing;
/*end*/
quit;

title 'Box cox for LDL';
proc transreg data=training ss2 details pbo;*uses box cox to find the appropriate transformation;
  model boxcox(LDL)=identity(DBP SBP age TG BMI HDL glucose smoking caucasian african);
run;

/*try log charges and look at log LDL, scatter plots and correlations */
title'transforming data and fitting a new regression model';
data training1;
  set training;
  logldl= log(LDL);*generates transformed variable logldl;
run;

/*check normality of log LDL*/
title "check normality of logldl";
proc univariate data = training1 normal;
	var logldl;
	histogram /normal;
	qqplot /normal (mu=est sigma=est);
run;

title "scatterplot matrix of quantitative variables with logldl";
proc sgscatter data = training1;
	matrix DBP SBP age TG BMI HDL glucose logldl;
run;
proc corr data = training1 spearman;
	var DBP SBP age TG BMI HDL glucose logldl;
run;

title "scatterplot matrix of categorical variables with logldl";
proc sgscatter data = training1;
	matrix smoking raceth logldl;
run;

/*second model using logldl*/
title "regression of logldl";
proc reg data = training1;
	model logldl = DBP SBP age TG BMI HDL glucose smoking caucasian african/vif;
	output out =trainingregout2 r=resids p=preds;
run;

title "Scatter plots of residuals after transformation with loess smooth";
proc sgscatter data = trainingregout2;
	plot resids*DBP resids*SBP resids*age resids*TG resids*BMI resids*HDL resids*glucose resids*smoking resids*caucasian resids*african resids*preds/ loess=(smooth=0.5);
run;

/*check normality of residuals*/
title "check normality of residuals";
proc univariate data = trainingregout2 normal;
	var resids;
	histogram /normal;
	qqplot /normal (mu=est sigma=est);
run;

data trainingregout2;
	set trainingregout2;
	sqresid=resids**2;
	semistudresid=resids/0.25756; /*looked up root mse in sas output*/;
run;

proc sgscatter data = trainingregout2;
	title "Scatter plots of semistudentized residuals with loess smooth";
	plot semistudresid*DBP semistudresid*SBP semistudresid*age semistudresid*TG semistudresid*BMI semistudresid*HDL semistudresid*glucose semistudresid*smoking semistudresid*caucasian semistudresid*african semistudresid*preds/ loess=(smooth=0.5);
run;

/*look for constant variance*/
proc sgscatter data = trainingregout2;
	title "Scatterplots of squared residuals with lowess smooth";
	plot sqresid*DBP sqresid*SBP sqresid*age sqresid*TG sqresid*BMI sqresid*HDL sqresid*glucose sqresid*smoking sqresid*caucasian sqresid*african sqresid*preds/ loess=(smooth=0.5);
run;

ods rtf close;*closes rtf file for printing;
/*end*/
quit;






/* Bekana Tadese */
ods rtf file= 'H:\PH1820-Appl.l.regression\Group assignement\groupAss-out3.rtf';
/*this tells sas to save the output to an .rtf file to your local drive using the paralells client*/
 

/*Assessing Interaction and Model selection */

data training1;
set training1;
dbpage=dbp*age;
dbphdl=dbp*hdl;
dbpbmi=dbp*bmi;
dbptg=dbp*tg;
dbpglucose=dbp*glucose;
dbpsmoking=dbp*smoking;
dpbcaucasian=dbp*caucasian;
dbpafrican=dbp*african;
sbptg=sbp*tg;
sbpbmi=sbpbmi;
sbpage=sbp*age;
sbphdl=sbp*hdl;
sbpglucose=sbp*glucose;
sbpsmokiong=sbp*smoking;
sbpcaucasian=sbp*caucasian;
sbpafrican=sbp*african;
agetg=age*tg;
agebmi=age*bmi;
agehdl=age*hdl;
ageglucose=age*glucose;
agesmoking= age*smoking;
agecaucasian=age*caucasian;
ageafrican=age*african;
tgbmi=tg*bmi;
tghdl=tg*hdl;
tgglucose=tg*glucose;
tgsmoking=tg*smoking;
tgcaucasian=tg*caucasian;
tgafrican= tg*african;
bmihdl=bmi*hdl;
bmiglucose=bmi*glucose;
bmismoking=bmi*smoking;
bmicaucasian= bmi*caucasian;
bmiafrican= bmiafrican;
hdlglucose=hdl*glucose;
hdlsmoking=hdl*smoking;
hdlcaucasian=hdl*caucasian;
hdlafrican=hdl*african;
glucosesmoking=glucose*smoking;
glucosecaucasian=glucose*caucasian;
glucoseafrican=glucose*african;
smokingcaucasian=smoking*caucasian;
smokingafrican=smoking*african;
run;

 proc print data=training1 (obs=20);   /* this is just for me to check if my codes worked or printed out */
 run;



 /* let me first see interactions with few variables one by one */

/*SBP age TG BMI HDL glucose smoking caucasian african */

Proc reg data = training1 ;
model logldl = DBP age dbpage ;run;
Proc reg data = training1 ;
model logldl = DBP hdl dbphdl ;run;
Proc reg data = training1 ;
model logldl = DBP bmi dbpbmi ;run;
Proc reg data = training1 ;
model logldl = DBP tg dbptg ;run;

Proc reg data = training1 ;
model logldl = DBP smoking dbpsmoking ;run;

Proc reg data = training1 ;
model logldl = DBP african dbpafrican ;run; 

Proc reg data = training1 ;
model logldl = sbp tg sbptg ;run;
Proc reg data = training1 ;
model logldl = sbp bmi sbpbmi ;run;
 Proc reg data = training1 ;
model logldl = sbp age sbpage ;run;
  Proc reg data = training1 ;
model logldl = sbp hdl sbphdl ;run;
 Proc reg data = training1 ;
model logldl = sbp glucose sbpglucose ;run;
 Proc reg data = training1 ;
model logldl = sbp smoking sbpsmokiong ;run;
 Proc reg data = training1 ;
model logldl = sbp african sbpafrican ;run; 
 Proc reg data = training1 ;
model logldl = age hdl agehdl ;run;
 Proc reg data = training1 ;
model logldl = age glucose ageglucose ;run;
  Proc reg data = training1 ;
model logldl = age bmi agebmi ;run; 
 Proc reg data = training1 ;
model logldl = age caucasian agecaucasian ;run;
 Proc reg data = training1 ;
model logldl = age african ageafrican ;run;

 Proc reg data = training1 ;
model logldl = tg bmi tgbmi ;run;

 Proc reg data = training1 ;
model logldl = tg glucose tgglucose ;run;
 Proc reg data = training1 ;
model logldl = tg smoking tgsmoking ;run;
Proc reg data = training1 ;
model logldl = tg caucasian tgcaucasian ;run;
Proc reg data = training1 ;
model logldl = tg african tgafrican ;run;
Proc reg data = training1 ;
model logldl = bmi glucose bmiglucose ;run;
Proc reg data = training1 ;
model logldl = bmi smoking bmismoking ;run;
Proc reg data = training1 ;
model logldl = bmi caucasian bmicaucasian ;run;
Proc reg data = training1 ;
model logldl = bmi african bmiafrican ;run;
Proc reg data = training1 ;
model logldl = hdl caucasian hdlcaucasian ;run;
Proc reg data = training1 ;
model logldl = hdl african hdlafrican ;run;

Proc reg data = training1 ;
model logldl = glucose smoking glucosesmoking ;run;
Proc reg data = training1 ;
model logldl = glucose caucasian glucosecaucasian ;run;
Proc reg data = training1 ;
model logldl = glucose african glucoseafrican ;run;

Proc reg data = training1 ;
model logldl = smoking african smokingcaucasian ;run;
Proc reg data = training1 ;
model logldl = smoking african smokingafrican;run;
quit;



/* Conclusion:
Based on available evidence AND individual assesment of interactions, the following are statistically significant and worth for further assessment in the model*/

Proc reg data = training1 ;
model logldl = DBP glucose dbpglucose ;run;  
 Proc reg data = training1 ;
model logldl = DBP caucasian dpbcaucasian ;run;
 Proc reg data = training1 ;
model logldl = sbp caucasian sbpcaucasian ;run; 
Proc reg data = training1 ;
model logldl = age smoking agesmoking ;run; 
 Proc reg data = training1 ;
model logldl = tg hdl tghdl ;run;  
Proc reg data = training1 ;
model logldl = bmi hdl bmihdl ;run; 
Proc reg data = training1 ;
model logldl = hdl glucose hdlglucose ;run; 
Proc reg data = training1 ;
model logldl = hdl smoking hdlsmoking ;run; 


/* I want only the following be print as rtf */
ods rtf file= 'H:\PH1820-Appl.l.regression\Group assignement\groupAss-out3.rtf';



Title " Regression model: First order and interaction terms";    /*this is the final model with all first order and interaction terms */  

Proc reg data = training1 ;
model logldl = DBP SBP age TG BMI HDL glucose smoking caucasian african agesmoking tghdl bmihdl hdlglucose hdlsmoking ;
	output out = training1out p=preds r=resids;
run;
quit;



Title "Models Selection according to Mallow’s Cp Criterion"; 

Proc reg data=training1 outest=modelsel;
model logldl = DBP SBP age TG BMI HDL glucose smoking caucasian african agesmoking tghdl bmihdl hdlglucose hdlsmoking /selection= rsquare adjrsq sse mse cp;
run;
quit;


/* list the variable names in the data set modelsel*/
proc contents data = modelsel;
run;

/* then, sort the Cp */
data modelselcpsort;
set modelsel;
run;

data modelselcpsort;
set modelselcpsort;
Id = _CP_;

proc sort data = modelselcpsort;
by  _CP_ ;
run;


/*print the data as a table of CP and variables*/
title "Top 5 best model:data sorted by Cp low to high with variables";
proc print data = modelselcpsort (FIRSTOBS=21 OBS=40)label;
var _IN_ _P_ _ADJRSQ_ _CP_ DBP SBP age TG BMI HDL glucose smoking caucasian african agesmoking tghdl bmihdl hdlglucose hdlsmoking;
run;
quit;

/*Best model1*/
Title " Best model1 of predictors of LDL";
Proc reg data=training1;
model logldl =DBP age TG bmi HDL glucose smoking agesmoking TGhdl BMIhdl HDLsmoking ;
output out = outfinal p=preds r=resids;
run;
quit;

/*Best model2*/
Title " Best model2 of predictors of LDL";
Proc reg data=training1;
model logldl =DBP TG HDL glucose smoking TGhdl HDLsmoking ;
output out = outfinal p=preds r=resids;
run;
quit;
******************************************************Barsha's work***********************************************;

ods rtf file='C:\Users\bthakur\Documents\barshawork.rtf';

/* Influential diagnostics for best model 1*/
ods output on;
ods graphics on/ labelmax=1900;
Title " Best model1 of predictors of LDL";
Proc reg data=training1 plots(only)=(cooksd(label) dffits(label));
model logldl =DBP age TG bmi HDL glucose smoking agesmoking TGhdl BMIhdl HDLsmoking/influence ;
output out = outfinal p=preds r=resids student=studentresids rstudent=studdelresid
cookd=cooksd dffits=dffits h=hatmat;
ods output outputstatistics=out1;/*for calcualting DFBETAS values*/
run;
quit;
ods graphics off;
ods output off;
proc print data=out1 (obs=10);
run;


/*identifying outliers for model 1*/
data outfinal;
set outfinal;
tvalue=tinv(0.9999,1812);/*calculating t value t(1-alpha/2n;n-p-1)*/
if (abs(studdelresid)) gt tvalue then outliery=1;
if (abs(studdelresid)) le tvalue then outliery=0;
run;
proc print data=outfinal;
title 'outliers for model 1';
where outliery=1;
var logldl resids studdelresid tvalue outliery;
run;


/*identifying leverage points for model 1*/
data outfinal;
set outfinal;
if (hatmat) gt 0.0120614 then leverage=1;/*2p/n=0.0120614*/
if (hatmat) le 0.0120614 then leverage=0;
run;
proc print data=outfinal;
title 'leverage for model 1';
where leverage=1;
var logldl hatmat leverage;
run;
/*PLOTTING LEVERAGE POINTS*/
ODS GRAPHICS ON/LABELMAX=1900;
goptions reset= all;
axis1 label=(a=90);
symbol pointlabel=('#id' c=Orange h=1 NODROPCOLLISIONS) value=POINT; 
title"plot of leverage points with reference =0.0120614";
proc gplot data= outfinal;
plot hatmat*id /vref= 0.0120614 vaxis=axis1 lvref=5 CVREF=BLUE ; 
run;
ODS GRAPHICS OFF;

/* identifying influential dffits values for model1*/
data outfinal;
set outfinal;
if (abs(dffits)) gt 0.15531 then influential_dffits=1;/*0.15531=2*under root p/n*/
if (abs(dffits)) le 0.15531 then influential_dffits=0;
absdffits=abs(dffits);
run;
proc print data=outfinal;
title 'Influential dffits for model 1';
where influential_dffits=1;
var logldl dffits influential_dffits;
run;
/*Plotting influential dffits values*/
ODS GRAPHICS ON/LABELMAX=1900;
goptions reset= all;
axis1 label=(a=90);
symbol pointlabel=('#id' c=Orange h=1 NODROPCOLLISIONS) value=POINT; 
title"plot of dffits with reference =0.15531";
proc gplot data= outfinal;
plot absdffits*id /vref= 0.15531 vaxis=axis1 lvref=5 CVREF=BLUE ; 
run;
ODS GRAPHICS OFF;

/*plot for COOK's distance*/
data outfinal;
set outfinal;
fval=finv(0.5,11,1813);/*this computes the threshold for cooks d*/
run;
goptions reset= all;
axis1 label=(a=90);
symbol pointlabel=('#id'c=brown h=1 nodropcollisions) value=none; 
title"plot of cook's distance with reference f(p,n-p) percentile =0.50";
proc gplot data= outfinal;
plot cooksd*id /vref= 0.9404 vaxis=axis1 lvref=5 ; /*0.9404 is from the fvalabove*/
run;

/*Influential diagnostics for best MODEL 2*/
ods output on;
ods graphics on/labelmax=1900;
Title " Best model2 of predictors of LDL";
Proc reg data=training1  plots(only)=(cooksd(label) dffits(label)) ;
model logldl =DBP TG HDL glucose smoking TGhdl HDLsmoking/influence ;
output out = outfinal2 p=preds r=resids student=studentresids rstudent=studdelresid
cookd=cooksd dffits=dffits h=hatmat;
ods output outputstatistics=out2;/*for calculating DFBETAS values*/
run;
quit;
ods graphics off;
ods output off;
proc print data=outfinal2 (obs=10);
var  studdelresid;

/*identifying outliers for model 2*/
data outfinal2;
set outfinal2;
tvalue=tinv(0.9999,1816);/*calculating t value t(1-alpha/2n;n-p-1)*/
if (abs(studdelresid)) gt tvalue then outliery=1;
if (abs(studdelresid)) le tvalue then outliery=0;
run;
proc print data=outfinal2;
title 'outliers for model 2';
where outliery=1;
var logldl resids studdelresid tvalue outliery;
run;

/*identifying leverage points for model 2*/
data outfinal2;
set outfinal2;
if (hatmat) gt 0.007675 then leverage=1;/*2p/n=0.007675*/
if (hatmat) le 0.007675 then leverage=0;
run;
proc print data=outfinal2;
title 'leverage for model 2';
where leverage=1;
var logldl hatmat leverage;
run;
/*PLOTTING LEVERAGE POINTS*/
ODS GRAPHICS ON/LABELMAX=1900;
goptions reset= all;
axis1 label=(a=90);
symbol pointlabel=('#id' c=orange h=1 NODROPCOLLISIONS) value=POINT; 
title"plot of leverage points with reference =0.007675";
proc gplot data= outfinal2;
plot hatmat*id /vref= 0.007675 vaxis=axis1 lvref=5 CVREF=BLUE ; 
run;
ODS GRAPHICS OFF;


/* identifying influential dffits values for model2*/
data outfinal2;
set outfinal2;
if (abs(dffits)) gt 0.12389 then influential_dffits=1;/*0.1238=2*under root p/n*/
if (abs(dffits)) le 0.12389 then influential_dffits=0;
absdffits=abs(dffits);
run;
proc print data=outfinal2;
title 'Influential dffits for model 2';
where influential_dffits=1;
var logldl dffits influential_dffits;
run;
/*PLOTTING influential DFFITS points*/
ODS GRAPHICS ON/LABELMAX=1900 width=4in;
goptions reset= all;
axis1 label=(a=90);
symbol pointlabel=('#id' c=Orange h=1 NODROPCOLLISIONS) value=POINT; 
title"plot of influential dffits points with reference =0.12389";
proc gplot data= outfinal2;
plot absdffits*id /vref= 0.12389 vaxis=axis1 lvref=5 CVREF=BLUE ; 
run;
ODS GRAPHICS OFF;

/*finding influential COOK's distance*/
data outfinal2;
set outfinal2;
fval=finv(0.5,7,1817);/*this computes the threshold for cooks d*/
run;
data outfinal2;
set outfinal2;
if (cooksd) gt fval then influential_cooksd=1;
if (cooksd) le fval then influential_cooksd=0;
run;
proc print data=outfinal2;
title"Influential cook's d for model 2";
where influential_cooksd=1;
var cooksd influential_cooksd;
run;
/*plot for COOK's distance*/
goptions reset= all;
axis1 label=(a=90);
symbol pointlabel=('#id' c=brown h=1) value=none; 
title"plot of cook's distance with reference f(p,n-p) percentile =0.50";
proc gplot data= outfinal2;
plot cooksd*id /vref= 0.90688 vaxis=axis1 lvref=2 ; /*0.90688 is from the fvalabove*/
run;

*****************************MODEL VALIDATION***************************************;
proc print data=validation (obs=10);
run;


title'creating transformed variable and interaction terms for validation set';
data validation;
  set validation;
  Vlogldl= log(LDL);*generates transformed variable logldl;
  agesmoking= age*smoking;
  TGhdl= TG*hdl;
  BMIhdl= BMI*hdl;
  HDLsmoking= HDL*smoking;
run;

ods pdf file='C:\Users\bthakur\Documents\trainingfinalmodel1.pdf';
Title " Best model1 on training set";
Proc reg data=training1 ;
model logldl =DBP age TG bmi HDL glucose smoking agesmoking TGhdl BMIhdl HDLsmoking ;
output out = outfinal1 p=preds r=resids;
run;
quit;
ods pdf close;

ods pdf file='C:\Users\bthakur\Documents\testfinalmodel1.pdf';
Title " Best regression model1 on validation set";
Proc reg data=validation;
model Vlogldl =DBP age TG bmi HDL glucose smoking agesmoking TGhdl BMIhdl HDLsmoking ;
output out = outfinal2 p=preds r=resids;
run;
quit;
ods pdf close;


data validation;
set validation;
   label   
           LDL = " V Low density lipoprotein cholesterol"  /* Low density lipoprotein cholesterol (mg/dl)*/
		   Vlogldl="V log transformed low density lipoprotein cholestrol"
		   DBP = " V Diastolic blood pressure" /*Diastolic blood pressure (mmHg)*/
           SBP = "V Systolic blood pressure" /*Systolic blood pressure(MMHg)*/ 
           age = "V age"   /*age  in years*/
		   TG  = "V triglycerides"   /*triglycerides(mg/dl)*/ 
       	   BMI = "V Body mass Index"   /*Body mass Index  (Kg/m2)*/
		   HDL = "V High density lipoprotein"   /*High density lipoprotein cholesterol (mg/dl)*/
		   glucose="V Glucose level" /*Glucose level (mg/dl)*/
		   smoking ="V Smoking Yes=1 No=0" 
           raceth = "V race-ethnicity 1= Caucasian 2=African-American 3=others"
		  ;
run;

/*Now let's prepare the data for MSPR note that because we named the response variable something different*/
/*when we read in the validation set, when we stack the data sets using the set command, we get missing values*/
/* for all the rows of the logsurvtime */

data allhers;
set training1 validation;
run;

/*lets look at model1 first*/
/* model 1: logldl =DBP+age+TG+bmi+HDL+glucose+smoking+agesmoking+TGhdl+BMIhdl+HDLsmoking ; */
/* performing proc reg with missing response variable on the validation set generates predicted values */
/* for the validation predictors using the model fit on the training data. */ 
title "preparing for computing MSPR";
proc reg data = allhers;
	model logldl =DBP age TG bmi HDL glucose smoking agesmoking TGhdl BMIhdl HDLsmoking ;
	output out = allhersregout p=predicted r= resids;
run;
quit;

/*separate the validation set from the full set, and now this data set has the predicted values as well as predictors and response for validation set*/
data hersvalid1;
	set allhersregout;
	if _N_ le 1824 then delete; /*recall 1824 is the number of observations of the training set*/
run;
data hersvalid1;
	set hersvalid1;
	preddiffsq=(Vlogldl-predicted)**2;/*compute the squared deviations*/
run;
/*compute the sum over n* of the squared deviations*/
title "model 1 MSPR";
proc means data =hersvalid1;
	var preddiffsq;
run;

/* Validation For final model 2*/

Title " Best model2 on training set";
Proc reg data=training1 ;
model logldl =DBP TG HDL glucose smoking TGhdl HDLsmoking ;;
output out = outfinal3 p=preds r=resids;
run;
quit;


Title "Best model2 on validation set";
Proc reg data=validation;
model Vlogldl =DBP TG HDL glucose smoking TGhdl HDLsmoking ;
output out = outfinal4 p=preds r=resids;
run;
quit;


/*lets look at model1 first*/
/* model 2: logldl =DBP TG HDL glucose smoking TGhdl HDLsmoking  */
/* performing proc reg with missing response variable on the validation set generates predicted values */
/* for the validation predictors using the model fit on the training data. */ 
title "preparing for computing MSPR for model2";
proc reg data = allhers;
	model logldl =DBP TG HDL glucose smoking TGhdl HDLsmoking;
	output out = allhersregout2 p=predicted r= resids;
run;
quit;

/*separate the validation set from the full set, and now this data set has the predicted values as well as predictors and response for validation set*/
data hersvalid2;
	set allhersregout2;
	if _N_ le 1824 then delete; /*recall 1824 is the number of observations of the training set*/
run;
data hersvalid2;
	set hersvalid2;
	preddiffsq=(Vlogldl-predicted)**2;/*compute the squared deviations*/
run;
/*compute the sum over n* of the squared deviations*/
title "model 2 MSPR";
proc means data =hersvalid2;
	var preddiffsq;
run;

***************************Final model**********************;
ods graphics on;
Title " Final regression model of predictors of LDL";
Proc reg data=training1;
model logldl =DBP TG HDL glucose smoking TGhdl HDLsmoking;
output out = outfinal2 p=preds r=resids;
run;
quit;


/*looking whether dropping outliers and leverages improves anything*/
TITLE'Regression model fitted on data with deleted outliers and leverage points';
data outfinal3;
set outfinal2;/*outfinal 2 has the same variables as that in trainind set plus the leverages and outliers therefore this dataset has been used*/
If outliery=1 then delete;
Else if leverage=1 then delete;
run;
Proc reg data=outfinal3;
model logldl =DBP TG HDL glucose smoking TGhdl HDLsmoking;
output out = outfinal4 p=preds r=resids;
run;
quit;
ods rtf close;
