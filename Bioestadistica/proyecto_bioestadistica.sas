* cargamos la base de datos;
PROC IMPORT DATAFILE='/home/u61370388/sasuser.v94/Universidad/epilepsia_final.csv'
	DBMS=CSV
	OUT=epilepsia replace;
	GETNAMES=YES;
RUN;

* imputamos los datos;
proc surveyimpute data=epilepsia method=hotdeck(selection=srswr);
   var y1 y2 y3 y4;
   output out=epilepsia_hd;
run;

* vemos el resultado;
proc print data=epilepsia_hd;
run;

* Observaciones: 1) Al momento de crear la base de datos utilizada no se fijo una semilla,
al ejercutar el codigo se obtendrá otras base de datos;

* SURVIVAL;
PROC IMPORT DATAFILE='/home/u61370388/sasuser.v94/Universidad/surv_hd.csv'
	DBMS=CSV
	OUT=survival replace;
	GETNAMES=YES;
RUN;

data survival;
    set survival;
    if sexo="NA" then sexo='';
run;

proc surveyimpute data=survival method=hotdeck(selection=srswr);
   var edad sexo comorbilidades hospitalizacion;
   output out=surv_hd;
run;

proc print data=surv_hd;
run;

* con el siguiente codigo se pueden guardar las bases de datos definidas anteriormente;
proc export data=surv_hd
    outfile="/home/u61370388/sasuser.v94/Universidad/surv_hotdeck2.csv"
    dbms=csv;
run;