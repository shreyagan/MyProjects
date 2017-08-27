*STAT 4360 FINAL PROJECT - Shreya Ganeshan;

dm 'log;clear;output;clear;';
options ps=50 ls=75 pageno=1;

data data;
	infile 'C:\Users\PWD193\Downloads\gasd96.txt' expandtabs firstobs=2;
	input obs COUNTY$ T8 T3 T5 ENRL DENS IMTR IM IL PLUN;
run;

proc print data = data;
run;

* DATA SUMMARY SECTION; 

*summary statistics of all variables;
proc means data = data mean std min max;
	var T8 T3 T5 ENRL DENS IMTR IM IL PLUN; 
	* these are all of the variables of interest;
	* don't need COUNTY and OBS in the regression analysis;
	* IQR is displayed in R code;
run;

* distribuion of all variables - the numerical variables are the only ones of interest;
* we see that all numerical variables are fairly symmetrical and normally distributed ;
* this is evident in the histograms;
proc univariate data = data;
	var T8 T3 T5 ENRL DENS IMTR IM IL PLUN;
	histogram;
run;

* frequency tables of all binary categorical variables;
* boolean values are recoded to reflect relevant variable interpretations in R code;
proc freq data = data;
	tables IMTR IM IL;
run;


* plotting T8 against all numerical variables ;
* does not make sense to plot categorical variables on a scatter plot;
proc gplot data = data;
	plot T8*T5 T8*T3 T8*ENRL T8*DENS T8*PLUN;
run;	

* plotting T8 against all categorical variables;
* need to sort data by each variable first to separate out the binary values;

* IMTR;
proc sort data = data out = IMTRdata;
	by IMTR;
run;
proc boxplot data = IMTRdata;
	plot T8*IMTR;
run;

*IM;
proc sort data = data out = IMdata;
	by IM;
run;
proc boxplot data=IMdata;
	plot T8*IM;
run;

*IL;
proc sort data = data out = ILdata;
	by IL;
run;
proc boxplot data = ILdata;
	plot T8*IL;
run;

* ANALYSIS SECTION; 

* correlation matrix
* measures strength of linear assocation between T8 and all variables;
* don't have to separate by data type in SAS like you must do in R; 
* can find correlation between numerical and categorical vars;
* p-values also displayed in SAS;
proc corr data = data;
	var T8 T5 T3 ENRL DENS IMTR IM IL PLUN;
run;

* regression on with all variables - excluding COUNTY and OBS; 
proc reg data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM IL PLUN;
run;
* all residual diagnostics can be seen use just the code above;
* this output also gives R2 value;

* model/variable selection;
* backward selection based on p-value of 0.01 - best model is: T5 T3 PLUN; 
proc reg data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM IL PLUN / selection=backward slstay=.01;
run;

* backward selection based on p-value of 0.05 - best model is: T5 T3 ENRL DENS IM PLUN; 
proc reg data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM IL PLUN / selection=backward slstay=.05;
run;
* the smaller the p-value threshold the narrower the model gets;

* backward selection based on AIC - best model is: T5 T3 ENRL DENS IMTR IM PLUN;
* stepAIC is more reliable than p-value;
proc glmselect data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM IL PLUN / selection=backward(choose=AIC);
run;

* regression with bet predictors;
* final model;
proc reg data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM PLUN;
run;

* residual diagnostics - checking assumptions;
* normality;
* constant variance or homoskedasticity;
* indepenence;
* outliers and influential points;
proc reg data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM PLUN / r p dw;
	output out = data2 r = residuals p = predicted; 
	* cook's distance calulated along with regression output;
run;

* check to see that data2 was actually stored with predicted and residual values;
* HAVE to have predicted and residual values to plot residual diagnostic plots;
proc print data = data2; 
run;

* residual diagnostic plots;
goptions reset=all;

* observed vs predcted values;
title1'Observed vs Predicted Values';
proc gplot data = data2;
	plot T8*predicted / haxis=axis1 vaxis=axis2 frame grid;
	axis1 label = (a=90 'Predicted');
	axis2 label = ('Observed');
run;

* residual vs fitted alues;
title1'Residual vs Fitted Values';
proc gplot data = data2;
	plot residuals*predicted / haxis=axis1 vaxis=axis2 frame grid;
	axis1 label = (a=90 'Fitted');
	axis2 label = ('Residuals');
run;	

* qq-plot;
title1'QQ Plot';
proc univariate data = data2 normal; 
	* "normal" options checks to see if resid. are normalyly dist;
	var residuals;
	qqplot residuals / normal(sigm = est mu = est) square;
run;
* residuals are fairly on normal line;

* durbin-watson test - to see if residuals are time dependent;
proc reg data = data; 
	* can use regular "data" data set now;
	model T8 = T5 T3 ENRL DENS IMTR IM PLUN / dw;
	* will get more info in output if you add in "r, p, dwprob" options;
run;

* white test - to see if there is no homoskedasticity;
* null hypothesis = homoskedasticity
* bc of a high p-value we fail to reject the null hypothesis;
proc reg data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM PLUN / spec;
run;

* model comparisons;
* null - basically, the expected value of T8;
proc univariate data = data;
	var T8; 
run;
*full - all variables in the dataset of interest;
* same code from above;
proc reg data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM IL PLUN;
run;

*final - only variables: T3, T5, ENRL, DENS, IMTR, IM, PLUN;
proc reg data = data;
	model T8 = T5 T3 ENRL DENS IMTR IM PLUN;
run;

* model comparison tables were just done by hand for the report, only using R2 values;
* R2 values are most relevant to the reader;
