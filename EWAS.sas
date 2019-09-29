ods graphics off; ods html close; ods listing close;
options nonotes nosource nosource2 errors=0;

proc printto log="C:\Users\akaushl1\Documents\IgE_MIXED\filename1.log";
run;

PROC IMPORT OUT= WORK.IgEt 
            DATAFILE= "C:\Users\akaushl1\Documents\IgE_MIXED\Residual_IgE_7Cell_Batch_with_IgE_0_to_11.csv" 
            DBMS=CSV REPLACE;
     
RUN;
Data work.IgEt1;
set work.IgEt(obs=7);
run;

Data Work.IgEt2;
 SET Work.IgEt (firstobs=8 obs=70008);
run;
Data work.IgEt;
  Set Work.IgEt1 Work.IgEt2;
run;
proc transpose data= Work.IgEt let 
     out= IgE_new (where=(upcase(_name_) ne 'ID_POs'));        
     var _all_;                                                      
   id id_pos; 
run;

Data IgE_new1;
Set IgE_new(firstobs=2);
run;

DATA IgE_new_long ;
  SET IgE_new1;
  IgE = total_IgE_5Y; time = 5; OUTPUT;
  IgE = total_IgE_8Y; time = 8; OUTPUT;
  IgE = total_IgE_11Y; time = 11; OUTPUT;
  DROP total_IgE_5Y total_IgE_8Y total_IgE_11Y ;
RUN;

data IgE_new_long1; 
  set IgE_new_long; 
  tcat=time; 
  newIgE = input(IgE,best32.);
  lIgE=log10(newIgE);
  cbige=input(CB_IgE_run_50,best32.);
  Weight1=input(Weight,best32.);
  gen=gender;
  drop IgE newIgE CB_IgE_run_50;
  rename lIgE=IgE;
run;

 data IgE1_new; 
  set IgE_new1(drop=CB_IgE_run_50
                   total_IgE_5Y total_IgE_8Y total_IgE_11Y _NAME_ ID Weight gender); 
 run;

proc contents data = IgE1_new 
out = vars(keep = varnum name) 
noprint; 
run; 
*options nonotes nosource nosource2 errors=0; 
data _null_;

   mVars +1;                            *** contract counter ;

   len = 0;                             *** amount of macro variable text used so far ;

   length str $100;                     *** to hold quoted contract number  ;

   ***  we are going to load as many &contractNNN as needed ;

   *call execute( cats( '%nrstr( %%let name)', mVars , '=' ) );
      call execute( cats( '%nrstr( %%let name)', mVars , '=' ) );

   do WHILE( NOT eof );                 *** and will LEAVE once macro var is filled ;

      set work.vars(keep= name) end= eof;

      have+1;                           *** just for infor report, count rows read ;

      *str = quote(trim( name ));
	  str = trim( name );

      call execute( trim(str) !!' ' ); *** adding another contract number to the %let statement ;

      len + ( 3 + length(str) );        *** update amount of macro var used ;;

      if EOF OR len gT 35000 then LEAVE;

   end;

   call execute( trim(str) !!' ; ' );   *** complete the %let statement ;

   putlog 'info: ' have= 'in ' mVars=;

   if eof then

      do;

         call symputx( 'n_name_vars', mVars );

         stop;

      end;

run;
*options notes source source2 errors=20; 
%macro split;
%do i = 1 %to &n_name_vars;
data IgE1_new&i.;
set IgE1_new (keep=&&name&i);
run;
%end; ;
%mend split;
%split;
/*
data IgE1_new1;
 set IgE1_new (keep=&x1);
 run;
*/


%macro split1;
%do i = 1 %to &n_name_vars;

proc contents data = IgE1_new&i.
out = vars&i.(keep = name type) 
noprint; 
run; 
%end;;
%mend split1;
%split1;

%macro split2;

%do k = 1 %to &n_name_vars;
data vars&k.;                                                
set vars&k.;                                                 
if type=2 and name ne 'id';                               
newname=trim(left(name))||"_n";                                                                               

/*The macro system option SYMBOLGEN is set to be able to see what the macro*/
/*variables resolved to in the SAS log.                                    */                                                       

options symbolgen;                                        

/*PROC SQL is used to create three macro variables with the INTO clause.  One  */
/*macro variable named c_list will contain a list of each character variable   */
/*separated by a blank space.  The next macro variable named n_list will       */
/*contain a list of each new numeric variable separated by a blank space.  The */
/*last macro variable named renam_list will contain a list of each new numeric */
/*variable and each character variable separated by an equal sign to be used on*/ 
/*the RENAME statement.                                                        */                                                        

proc sql noprint;                                         
select trim(left(name)), trim(left(newname)),             
       trim(left(newname))||'='||trim(left(name))         
into :c_list separated by ' ', :n_list separated by ' ',  
     :renam_list separated by ' '                         
from vars&k.;                                                
quit;                                                                                                               
 
/*The DATA step is used to convert the numeric values to character.  An ARRAY  */
/*statement is used for the list of character variables and another ARRAY for  */
/*the list of numeric variables.  A DO loop is used to process each variable   */
/*to convert the value from character to numeric with the INPUT function.  The */
/*DROP statement is used to prevent the character variables from being written */
/*to the output data set, and the RENAME statement is used to rename the new   */
/*numeric variable names back to the original character variable names.        */                                                        

data IgE_new_long2&k.;                                               
set IgE_new_long1 (keep=IgE ID cbige TIME TCAT weight1 gen &&name&k.);  
array ch(*) $ &c_list;                                    
array nu(*) &n_list;                                      
do i = 1 to dim(ch);                                      
  nu(i)=input(ch(i),8.);                                  
end;                                                      
drop i &c_list;                                           
rename &renam_list;                                                                                      
run;            

options nosymbolgen;
%end;

%mend split2;
%split2;
*options notes source source2 errors=20; 

%macro mylogita(indata, indvars, dep, myout =_out );
  %let k=1;
  %let ind = %scan(&indvars, &k);
  %do %while(&ind NE);
    title "The dependent variable is &dep";
    title2 "The independent variables are &ind";
*ods trace on;
ODS output SolutionF = est1&k  Tests3=est2&k;

proc mixed data=&indata method=ML ;*NOPROFILE ITDETAILS IC;
      class ID gen tcat;
      model &dep= &ind cbige time gen weight1/solution ddfm=bw;
	  *random intercept/Subject=ID type=VC;
      random int time/Subject=ID type=AR(1);*R=1,2 RCORR;
run;

ODS OUTPUT CLOSE;
*ods trace off;

%let k = %eval(&k + 1);
    %let ind = %scan(&indvars, &k);
  %end;

  data &myout;
    set 
    %do i = 1 %to &k - 1;
      est1&i est2&i
    %end; 
    ;
  run;

%mend;

*run the program;

%macro split3;

%do j = 1 %to &n_name_vars;
%mylogita(work.IgE_new_long2&j., &&name&j., IgE, myout = myparms&j.)
%end;
%mend split3;
%split3;

*ods trace off;

%macro split4;

%do k = 1 %to &n_name_vars;

Data myparm1 (Keep = Effect Estimate StdErr df);
 Set myparms&k.(where=(probt NE . and Effect not in ('time','Intercept','cbige','Weight1','gen')));
 run;
title;
Data myparm2 (Keep = Effect probf);
 Set myparms&k.(where=(probf NE . and Effect not in ('time','Intercept','cbige','Weight1','gen')));
 run;

* 1. Sort myparms1 by "Effect" & save sorted file as mayparms11 ; 
PROC SORT DATA=myparm1 OUT=myparms11; 
  BY Effect; 
RUN; 

* 2. Sort myparms2 by "Effect" & save sorted file as mayparms21 ; 

PROC SORT DATA=myparm2 OUT=myparms21; 
  BY Effect; 
RUN; 

* 3. Merge myparms11 and myparms21 by Effect in a data step ; 

DATA myparms12&k. ; 
  MERGE myparms11 myparms21; 
  BY Effect; 
RUN;
%end;
%mend split4;
%split4;


%macro combine;

data big;
  set
  %do i = 1 %to &n_name_vars;
   
    myparms12&i.
  %end;
  ;
run;

%mend;

%combine;

Data big1;
set big(rename=(probf=RAW_P));
run;
proc multtest pdata=big1 ADAPTIVEHOCHBERG out=big1p;
run; 

PROC EXPORT DATA= big1p
            OUTFILE= "C:\Users\akaushl1\Documents\IgE_MIXED\Results_mixed_Adaptive Hochberg1_april2016_1.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
