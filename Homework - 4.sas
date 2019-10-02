data a1;
infile 'H:\Homework4\WAGE.dat' firstobs=2 missover;
input edu hr wage famearn selfempl salaried  married numkid age locunemp;
lnwage=log(wage);
run;
proc contents; run;

/*/*ID and Year Variables are created*/
data a2;
do cs_id= 1 to 334;
    do year_id=1984 to 1986;
        output;
        end;
end;
/*a1 and a2 datasets are merged*/
data a3;
merge a2 a1;
proc print data=a3(obs=10);
run;

data wage_final;
set a3;
edu_sq=edu**2;
locunemp_sq=locunemp**2;
famearn_sq=famearn**2;
age_locunemp=age*locunemp;
hr_sq=hr**2;
married_kids=married*numkid;
RUN;

/*Choosing the best linear regression model and Checking for Multicollinearity*/
proc reg data=wage_final;
model lnwage=edu hr selfempl salaried  married numkid age locunemp/vif stb collin;
run;

/*Testing for Non-linear effects of some variables in the model*/
proc reg data=wage_final;
model lnwage=edu edu_sq hr selfempl salaried  married numkid age locunemp age_locunemp/vif stb collin;
run;

/*Using the same above model, estimating FIXEDONE, FIXEDTWO, RANONE, RANTWO parameters for comparison */
proc panel data=wage_final outest=estimates(rename=(TYPE=Model));
id cs_id year_id;
model lnwage=edu edu_sq hr selfempl salaried  married numkid age locunemp age_locunemp/fixone;
model lnwage=edu edu_sq hr selfempl salaried  married numkid age locunemp age_locunemp/ranone bp2;
model lnwage=edu edu_sq hr selfempl salaried  married numkid age locunemp age_locunemp/fixtwo;
model lnwage=edu edu_sq hr selfempl salaried  married numkid age locunemp age_locunemp/rantwo bp2;
model lnwage=edu edu_sq hr selfempl salaried  married numkid age locunemp age_locunemp/pooled;
run;

proc print data=estimates(drop=_: lnwage);run;

/*Reporting the findings: Model fit, t-values, meaning of coefficients, collinearity diagnostics, White test, Breusch-Pagan test*/
proc reg data=wage_final;
model lnwage=edu edu_sq hr selfempl salaried  married numkid age locunemp age_locunemp/vif stb collin;
run;

proc model data=wage_final;
wt_edu=1/edu;
locunemp_inv=1/locunemp;
parms const bedu bedu_sq bhr bselfempl bsalaried bmarried bnumkid bage blocunemp bage_locunemp;
lnwage = const + bedu*edu + bedu_sq*edu_sq + bhr*hr + bselfempl*selfempl + bsalaried*salaried + bmarried*married + bnumkid*numkid + bage*age + blocunemp*locunemp + bage_locunemp*age_locunemp;
fit lnwage/white BREUSCH=(1 edu edu_sq hr selfempl salaried  married numkid age locunemp age_locunemp) collin;
weight wt_edu;
run;
quit;

proc means data=wage_final;run;


/*Question 2*/

PROC IMPORT OUT= WORK.xls 
            DATAFILE= "H:\Homework4\pims.xls" 
            DBMS=EXCEL REPLACE;
     RANGE="pims$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

proc contents;run;

/*Executing 2SLS test and estimating the effects of pioneering on Market Share (MS)*/
PROC SYSLIN 2SLS SIMPLE;
ENDOGENOUS ms qual plb price dc;
INSTRUMENTS pion ef phpf plpf psc papc ncomp mktexp tyrp pnp cap rbvi emprody union penew ;
MODEL ms = qual plb price pion ef phpf plpf psc papc ncomp mktexp ;
MODEL qual = price dc pion ef tyrp mktexp pnp ;
MODEL plb = dc pion tyrp ef pnp custtyp ncust custsize ;
MODEL price = ms qual dc pion ef tyrp mktexp pnp;
MODEL dc = ms qual pion ef tyrp penew cap rbvi emprody union ;
RUN;

/*Calculating means of the interaction variables with pioneering*/
proc means; var phpf plpf psc papc pion ;run;

/*Estimating the effect of pioneering on market share using this simple regression model*/
proc reg ;
model ms = qual plb price pion ef phpf plpf psc papc ncomp mktexp;
run;
/* Estimating the change in effect of pioneering across different models.*/

proc reg;
MODEL qual = price dc pion ef tyrp mktexp pnp ;
run;

proc reg;
model PLB=dc pion tyrp ef pnp custtyp ncust custsize;
run;

proc reg;
model Price=ms qual dc pion ef tyrp mktexp pnp;run;

proc reg;
MODEL dc = ms qual pion ef tyrp penew cap rbvi emprody union;
run;
