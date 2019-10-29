(* ::Package:: *)

(* ::Title:: *)
(*PV System Analysis*)


(* ::Chapter:: *)
(*Constants*)


q=1.6*10^-19;
k=1.381*10^-23;
c=3*10^8;
h=6.626*10^-34;


(* ::Chapter:: *)
(*General functions*)


(* ::Subsection:: *)
(*Reduce DateObject*)


Options[fnReduceDateObject]={dateFormat->{"Day","/","Month","/","Year"," ","Hour",":","Minute"},datePosition->1};
fnReduceDateObject[dataset_,opt:OptionsPattern[]]:=Module[{reducedOutput,format,position},
format=OptionValue[dateFormat];
position=OptionValue[datePosition];

If[format==="DateList",
reducedOutput=Parallelize@Map[MapAt[DateList,#,position]&,dataset];
,
reducedOutput=Parallelize@Map[MapAt[DateString[#,format]&,#,position]&,dataset];];

Return[reducedOutput]
];


(* ::Subsection:: *)
(*Convert to dataset*)


(* ::Text:: *)
(*Convert a flat table to Dataset (DataFrame) object assuming first row as titles (column names). *)


fnToDataset[table_]:=Module[{dataset},
dataset=AssociationThread[First@table->#]&/@Rest[table]//Dataset;

Return[dataset]
];


(* ::Text:: *)
(*When table does not contain column names, the title row can be supplied as the second argument. *)


fnToDataset[table_,titles_]:=Module[{dataset},
dataset=AssociationThread[titles->#]&/@table//Dataset;

Return[dataset]
];


(* ::Chapter:: *)
(*General data processing*)


(* ::Subchapter:: *)
(*Data import and manipulation*)


(* ::Subsection::Closed:: *)
(*Import multiple files*)


fnMultiImport[files_]:=Module[{output},
output=Import[#]&/@files;
output=Flatten[output,1];

Return[output]
];

fnMultiImport[files_,format_]:=Module[{output},
output=Import[#,format]&/@files;
output=Flatten[output,1];

Return[output]
];


(* ::Subsection::Closed:: *)
(*Merge data*)


(* ::Text:: *)
(*This function merges multiple datasets with a common key (e.g. timestamp). The basic method is the convert these datasets into associations and use Mathematica built-in function JoinAcross to perform data merging. *)
(*The option keyPosition indicates the column at which this common key resides. The option joinMethod specifies the joining method which will be used in function JoinAcross. *)
(*The input "data" needs to be a table of datasets: {dataset1, dataset2, ...}. *)
(*It is not advisable to use DateObject as the timestamp as sometimes they are not compared correctly. *)


Options[fnMergeData]={keyPosition->1,header->True,joinMethod->"Outer",keyCollisionFunction->Right};
fnMergeData[data_,opt:OptionsPattern[]]:=Module[{keyIndex,toAssoc,associationTable,joinedSet},

If[OptionValue@header,
keyIndex=OptionValue[keyPosition];
If[NumericQ[keyIndex],keyIndex=data[[1,1]][[keyIndex]]]; (*when there is header line, assign keyIndex to be the name of the variable*)
toAssoc[table_]:=With[{index=First@table},AssociationThread[index->#]&/@Rest[table]];
associationTable=toAssoc/@data;
joinedSet=Fold[JoinAcross[#1,#2,keyIndex,OptionValue[joinMethod],KeyCollisionFunction->OptionValue[keyCollisionFunction]]&,associationTable];
,
(*when there is no header line*)
keyIndex=OptionValue[keyPosition];
(*assign a unique index (as string) to each column in the format of table#-column#, except the column# of the keyPosition, which will remain as it is*)
toAssoc[table_,tableNumber_]:=With[
{index=If[Length@Dimensions@table>1,
ReplacePart[ToString@tableNumber<>"-"<>ToString[#]&/@Range@Dimensions[table][[2]],keyIndex->ToString@keyIndex]
, (*else*)
ToString@keyIndex]},
AssociationThread[index->#]&/@table];
(*convert all datasets into associations*)
associationTable=Table[toAssoc[data[[i]],i],{i,Length@data}];

(*merge datasets from left to right, i.e. ...JoinAcross[JoinAcross[dataset1,dataset2],dataset3]...*)
(*when using "Left" or "Right" as joining method, arrange datasets so that the leftmost or rightmost dataset has the master keys*)
joinedSet=Fold[JoinAcross[#1,#2,ToString@keyIndex,OptionValue[joinMethod],KeyCollisionFunction->OptionValue[keyCollisionFunction]]&,associationTable];
];

(* returned output may be unsorted, can apply sort function afterwards*)
Return[joinedSet//Dataset]
];


(* ::Text:: *)
(*Fast dataset merging algorithm with inner joining and the first column as the common key. *)


fnMergeData[list_,"Fast"]:=Flatten[{#,Rest/@{##2}}]&@@@(Pick[#,Length@#>Length@list-1]&/@GatherBy[Join@@list,First]);


(* ::Subsection::Closed:: *)
(*Running average*)


(* ::Text:: *)
(*This function select non-missing and daytime data, obtain averaged values for every x minute interval.*)
(*It starts with the first timestamp in the dataset, and finds the average of all data that falls within the time step (desiredTimeStep) interval from that timestamp, then jump to the next timestamp beyond this interval. Default assumption of the time step in the original dataset is 1 minute. *)


(*Options[fnRunningAverage]={originalTimeStep->1};
fnRunningAverage[data_,desiredTimeStep_,opt:OptionsPattern[]]:=Module[{t,timeStamp1,dataBin,dataOut,output},
output={};
t=1; (*this is the row index number of the dataset*)

While[t<=Length[data]-desiredTimeStep,
timeStamp1=data[[t,1]];
dataBin=Select[data[[t;;t+Ceiling[desiredTimeStep/OptionValue[originalTimeStep]]]],#[[1]]<(timeStamp1+Quantity[desiredTimeStep,"Minutes"])&];
dataOut=Total[dataBin[[All,2;;-1]]]/Length[dataBin];
AppendTo[output,Prepend[dataOut,timeStamp1]];
t=t+Length[dataBin];];

Return[output]
];*)


Options[fnRunningAverage]={reportTimeStep->Quantity[10,"Minutes"]};
fnRunningAverage[data_,opt:OptionsPattern[]]:=Module[{dataDimension,ts,timeStep,output},
output={};
dataDimension=Dimensions@data;
timeStep=OptionValue[reportTimeStep];

If[dataDimension[[2]]>2,
ts=TimeSeries[{First@#,Rest@#}&/@data];
, (*else*)
ts=TimeSeries@data;
];

output=With[{d=ts["ValueDimensions"]},
MovingMap[If[Length@#!=0,Mean@#,If[d>1,Table[Null,d],Null]]&,ts,{timeStep,Left,{ts["FirstDate"],ts["LastDate"],timeStep}},None]
];

Return[output]
];


(* ::Subsection::Closed:: *)
(*Remove data points during night  time*)


(* ::Text:: *)
(*If the timestamp is in strings, dateFormat specifies the format. *)


Options[fnRemoveNightTime]={dateFormat->{"Day","/","Month","/","Year"," ","Hour",":","Minute"},location->GeoPosition[{1.3667,103.8}]};
fnRemoveNightTime[data_,opt:OptionsPattern[]]:=Module[{loc,groupedData,sunrise,sunset,dailySet,output},
loc=OptionValue[location];

(*convert timestamps to DateLists, and group the dataset by days*)
groupedData=GroupBy[MapAt[If[Head@#===String,DateList[{#,OptionValue[dateFormat]}],DateList@#]&,1]/@data,First[#][[1;;3]]&];

output={};
Do[
sunrise=Sunrise[loc,DateObject@day]//DateObject[#,"Instant"]&; (*function Sunrise and Sunset gives DateObject with "Minute" granularity after version 12, need to convert to "Instant" in order to compare with timestamps in the dataset*)
sunset=Sunset[loc,DateObject@day]//DateObject[#,"Instant"]&;
dailySet=MapAt[DateObject,1]/@groupedData[day];
AppendTo[output,Select[dailySet,sunrise<First@#<sunset&]]
,
{day,Keys@groupedData}];

Return[Flatten[output,1]]
];


(*Options[fnRemoveNightTime]={dateFormat\[Rule]{"Day","/","Month","/","Year"," ","Hour",":","Minute"},nightHourBoundary\[Rule]{4,20},location->GeoPosition[{1.3667,103.8}]};
fnRemoveNightTime[data_,opt:OptionsPattern[]]:=Module[{boundary,select,output},
boundary=OptionValue[nightHourBoundary];

select=With[{time=If[Head@First@#===String,DateList[{First@#,OptionValue[dateFormat]}],DateList@First@#]},
If[First@boundary<time[[-3]]<Last@boundary,
If[time[[-3]]<8||time[[-3]]>17,
If[DaylightQ[GeoPosition[{1.3667,103.8}],DateObject@time],#],#
]
]
]&;

Parallelize[output=select/@data];

Return[Cases[output,Except[Null]]]
];*)


(* ::Subsection::Closed:: *)
(*Data cleaning for SERIS datasets*)


fnSerisDataClean[dataDir_]:=Module[{files,output,title,data,cleanedData},
files=FileNames["*.txt",{dataDir}]; 
output=Import[#,"Table"]&/@files;
output=Flatten[output,1];
title=First[output];
(* delete title lines *)
data=DeleteCases[output,{__String}];
Print["data dimenstion is ",Dimensions[data]];
(* change time stamp and tag missing values *)
data=Join[List/@DateObject/@Map[StringRiffle,data[[All,1;;2]]],data[[All,3;;-1]],2];
data=Replace[data,"NaN"->Missing["NaN"],2];
(* remove night time data *)
data=Select[data,6<=DateValue[First[#],"Hour"]<=20&];
Parallelize[cleanedData=Select[data,DaylightQ[GeoPosition[{1.3667,103.8}],#[[1]]]&]];
Print["cleaned data dimenstion is ",Dimensions[cleanedData]];
Print[{Range[Length[title]],title,Flatten[{"min",Min/@Transpose[cleanedData[[All,2;;-1]]]}],Flatten[{"max",Max/@Transpose[cleanedData[[All,2;;-1]]]}],Flatten[{"mean",Mean[cleanedData[[All,2;;-1]]]}]}//TableForm];

Return[{title,cleanedData}]
];


(* ::Subsection::Closed:: *)
(*Summarize data shape*)


(* ::Text:: *)
(*Simple summary*)


fnDataSummary[dataset_]:=Module[{title=FirstCase[dataset,{__String}]},
If[title=!=Missing["NotFound"],
With[{x=DeleteCases[dataset,{__String}]},
Print[{Range[Length[title]],title,Flatten[{"min",Min/@Transpose@x}],Flatten[{"max",Max/@Transpose[x]}],Flatten[{"mean",Mean@x}]}//TableForm];
];
,(*else*)
Print[{Range[Dimensions[dataset][[2]]],Flatten[{"min",Min/@Transpose[dataset]}],Flatten[{"max",Max/@Transpose[dataset]}],Flatten[{"mean",Mean@dataset}]}//TableForm];
];

];


(* ::Subchapter:: *)
(*Quick period statistics*)


(* ::Text:: *)
(*Usually from monthly data. *)
(*Data should have the format {{time, var1, var2, ...}, ...}. *)
(*minimum data points are set assuming minute resolution data. *)


Options[fnPeriodSum]={minDataPts->60,period->"Day"};
fnPeriodSum[data_,opt:OptionsPattern[]]:=Module[{minPts,groupedData,periodResults,periodSet,periodSum},
minPts=OptionValue[minDataPts];

groupedData=Switch[OptionValue[period],
"Month",GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
"Day",GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
"Hour",GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
"10 Minutes",GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]]; 
(* timestamp represents the start of a time step *)

periodResults={};
(* correct the minimum data points required for each time step if user setting is not meaningful *)
If[OptionValue[period]=="10 Minutes"&&minPts>10,minPts=1;]; 

Do[
periodSet=groupedData[timestep];

If[Length[periodSet]<=minPts,Print[DateObject[timestep]," missing or has too little data points"],
periodSum=Total[periodSet[[All,2;;-1]]];

AppendTo[periodResults,Flatten[{DateObject[timestep],periodSum}]];];
,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];

Options[fnPeriodSpan]={minDataPts->60,period->"Day"};
fnPeriodSpan[data_,opt:OptionsPattern[]]:=Module[{minPts,groupedData,periodResults,periodSet,periodSpan},
minPts=OptionValue[minDataPts];

groupedData=Switch[OptionValue[period],
"Month",GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
"Day",GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
"Hour",GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
"10 Minutes",GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]];

periodResults={};
If[OptionValue[period]=="10 Minutes",minPts=1;];

Do[
periodSet=groupedData[timestep];

If[Length[periodSet]<=minPts,Print[DateObject[timestep]," missing or has too little data points"],
periodSpan=periodSet[[-1,2;;-1]]-periodSet[[1,2;;-1]];

AppendTo[periodResults,Flatten[{DateObject[timestep],periodSpan}]];];
,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];

Options[fnPeriodAverage]={minDataPts->60,period->"Day"};
fnPeriodAverage[data_,opt:OptionsPattern[]]:=Module[{minPts,groupedData,periodResults,periodSet,periodAverage},
minPts=OptionValue[minDataPts];

groupedData=Switch[OptionValue[period],
"Month",GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
"Day",GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
"Hour",GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
"10 Minutes",GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]];

periodResults={};
If[OptionValue[period]=="10 Minutes",minPts=1;];

Do[
periodSet=groupedData[timestep];

If[Length[periodSet]<=minPts,Print[DateObject[timestep]," missing or has too little data points"],
periodAverage=Mean[periodSet[[All,2;;-1]]];

AppendTo[periodResults,Flatten[{DateObject[timestep],periodAverage}]];];
,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];

Options[fnPeriodWeightedAvg]={minDataPts->60,period->"Day"};
fnPeriodWeightedAvg[data_,weight_,opt:OptionsPattern[]]:=Module[{minPts,groupedData,groupedWgtData,periodResults,periodSet,periodWgtSet,periodAverage},
minPts=OptionValue[minDataPts];

groupedData=Switch[OptionValue[period],
"Month",GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
"Day",GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
"Hour",GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
"10 Minutes",GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]];

groupedWgtData=Switch[OptionValue[period],
"Month",GroupBy[weight,DateValue[#[[1]],{"Year","Month"}]&],
"Day",GroupBy[weight,DateValue[#[[1]],{"Year","Month","Day"}]&],
"Hour",GroupBy[weight,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
"10 Minutes",GroupBy[weight,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]];

periodResults={};
If[OptionValue[period]=="10 Minutes",minPts=1;];

Do[
periodSet=groupedData[timestep];
periodWgtSet=groupedWgtData[timestep];

If[Length[periodSet]<=minPts,Print[DateObject[timestep]," missing or has too little data points"],
If[Length[periodSet]!=Length[periodWgtSet],
Print[DateObject[timestep]," does not have matching data and weights"],
periodAverage=Total[#*periodWgtSet[[All,2]]]/Total[periodWgtSet[[All,2]]]&/@Table[periodSet[[All,i]],{i,2,Dimensions[periodSet][[2]]}];

AppendTo[periodResults,Flatten[{DateObject[timestep],periodAverage}]];
];
];
,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];


(* ::Text:: *)
(*More generic treatment using built-in functionality of TimeSeries: *)
(**)


(* fnAvg performs averaging to the binned dataset x with data dimension d, without including non-numeric data *)
fnAvg[x_,d_:1]:=Module[{output},
If[Length@x!=0,
If[d>1,
output=ReplaceAll[Mean@Cases[#,_?NumericQ]&/@Transpose[x],_Mean->Missing["NoData"]];
, (*else*)
output=Mean@Cases[Flatten@x,_?NumericQ]/._Mean->Missing["NoData"];
];
, (*else*)
If[d>1,output=Table[Missing["DateNotFound"],d],output=Missing["DateNotFound"]]
];
Return[output]
];

(* fnCount simply counts the number of timestamps present in each bin of time window *)
fnCount=Length@#1&;

(* fnCountValid counts the number of valid numeric datapoints present in each bin of time window *)
fnCountValid[x_,d_:1]:=Module[{output},
If[Length@x!=0,
If[d>1,
output=Length@Cases[#,_?NumericQ]&/@Transpose[x];
, (*else*)
output=Length@Cases[Flatten@x,_?NumericQ];
];
, (*else*)
If[d>1,output=Table[Missing["DateNotFound"],d],output=Missing["DateNotFound"]]
];
Return[output]
];

(* fnSum gives the sum of valid numeric datapoints present in each bin of time window *)
fnSum[x_,d_:1]:=Module[{output},
If[Length@x!=0,
If[d>1,
output=Total@Cases[#,_?NumericQ]&/@Transpose[x];
, (*else*)
output=Total@Cases[Flatten@x,_?NumericQ];
];
, (*else*)
If[d>1,output=Table[Missing["DateNotFound"],d],output=Missing["DateNotFound"]]
];
Return[output]
]

(* fnWgtAvg performs weighted averaging to the binned dataset x with data dimension d, without including non-numeric data or non positive weights *)
fnWgtAvg[x_,d_?Positive,weight_]:=Module[{output},
If[Length@x>1,
	If[d>1,
		output=ReplaceAll[
			With[{wd=Cases[{#,weight}\[Transpose],{_?NumericQ,_?Positive}]},
			If[Length@wd>1,Mean[WeightedData@@Transpose@wd],If[Length@wd==1,wd[[1,1]],Missing["NoData"]]]]&/@Transpose[x]
		,_Mean->Missing["NoData"]];
	, (*else, perform simple averaging without weighting*)
		output=Mean@Cases[Flatten@x,_?NumericQ]/._Mean->Missing["NoData"];
	];
, (*else*)
	If[Length@x==1,
		output=Flatten@x;
	,(*else, x contains no element*)
		If[d>1,output=Table[Missing["DateNotFound"],d];,output=Missing["DateNotFound"];];
	];
];

Return[output]
];



Options[fnPeriodStats]={timeStep->Quantity[1,"Days"],function->fnAvg,windowAlignment->Left,windowPadding->None,weightPosition->Null};
fnPeriodStats[data_,start_,end_,opt:OptionsPattern[]]:=Module[{fn,tstep,wdAlignment,padding,ts=Null,dataDimension,wgtPos,output="No result"},
fn=OptionValue[function];
tstep=OptionValue[timeStep];
wdAlignment=OptionValue[windowAlignment];
padding=OptionValue[windowPadding];
wgtPos=OptionValue[weightPosition];

(* define time series *)
If[Head@data===TemporalData,
	ts=data;
,(*else*)
	If[Dimensions[data][[2]]>2,
		ts=TimeSeries[{First@#,Rest@#}&/@If[Head@data===Dataset,Normal@Values@data,data]];
	,(*else*)
		If[Dimensions[data][[2]]==2,ts=TimeSeries@If[Head@data===Dataset,Normal@Values@data,data]];
	];
];
	
dataDimension=ts["ValueDimensions"];
If[wgtPos=!=Null,
	If[1<=wgtPos<=dataDimension,
		output=If[dataDimension>1,
			MovingMap[fnWgtAvg[#,dataDimension,#[[All,wgtPos]]]&,ts,{tstep,wdAlignment,{start,end,tstep}},padding]
		,(*else, when dataset does not contain valid weighting data, simply apply averaging without weighting*)
			Print@"warning: input dataset does not contain weighting data, simple averaging is applied";
			MovingMap[fnAvg[#,dataDimension]&,ts,{tstep,wdAlignment,{start,end,tstep}},padding]
		];
	,
		Print@"warning: weighting data incorrectly defined, simple averaging is applied";
		output=MovingMap[fnAvg[#,dataDimension]&,ts,{tstep,wdAlignment,{start,end,tstep}},padding];
	];
,(*else*)
	output=MovingMap[fn[#,dataDimension]&,ts,{tstep,wdAlignment,{start,end,tstep}},padding];
];

Return[output]
];

(* start and end time can be in DateList format of a DateObject *)
(*  weighted average is triggered by specifying option weightPosition,
	needs dataset to contain weighting data, 
	weight position should be specified as: column_number_of_the_weight - 1 (minus timestamp column) 
	weighted average for the weighting data should not be used *)
(* no minimum data point requirement is defined, results will not be Missing as long as there is data *)


(* ::Subchapter::Closed:: *)
(*Speedy post-processing of plots*)


fnSerisShow[plot_]:=Show[plot,
LabelStyle->{FontFamily->"Helvetica",FontSize->22,FontWeight->Bold,FontColor->Black},
ImageSize->600,ImageMargins->15];


(*Options[fnSerisShow]={frameLabel->{"x","y"}};
fnSerisShow[plot_,opt:OptionsPattern[]]:=Show[plot,
Frame->True,
Axes->False,
LabelStyle->{FontFamily->"Helvetica",FontSize->22,FontWeight->Bold,FontColor->Black},
FrameTicks->{{Automatic,None},{Automatic,None}},
FrameLabel->OptionValue[frameLabel],
ImageSize->600,ImageMargins->15];*)


fnExtractPlotData[plot_]:=Module[{points},
points=Cases[plot,x_Point:>First@x,Infinity];
If[Length[points]==0,
points=Cases[plot,x_Line:>First@x,Infinity];];
If[Length[points]==0,points=$Failed;,points=points//First];

Return[points]
];


Options[fnAddTrendline]={showEquation->True,placeEquation->Scaled[{0.6,0.8}]};
fnAddTrendline[plot_,opt:OptionsPattern[]]:=Module[{data,fit},
data=Cases[plot,x_Point:>First@x,Infinity]//First;
fit=Fit[data,{1,x},x];

Show[plot,Plot[fit,{x,Min[data[[All,1]]],Max[data[[All,1]]]},PlotStyle->Red],Epilog->Text[Style[fit,"Helvetica",Bold,15],OptionValue[placeEquation],{-1,0}]]
];


(* ::Chapter::Closed:: *)
(*Solar geometry and meteorological*)


(* ::Subchapter:: *)
(*Daytime duration*)


(* ::Text:: *)
(*Input is Julian day number from start of the year, and latitude is in degrees (value from 89 to -89, positive being northern hemisphere). *)


fnDayLength[j_,lat_]:=Module[{\[Theta],\[Phi],dayLength},
\[Theta]=0.2163108+2*ArcTan[0.9671396*Tan[0.00860*(j-186)]];
\[Phi]=ArcSin[0.39795*Cos[\[Theta]]];
dayLength=24-24/Pi*ArcCos[(Sin[0.8333*Pi/180]+Sin[lat*Pi/180]*Sin[\[Phi]])/(Cos[lat*Pi/180]*Cos[\[Phi]])];
(*there is no real solution when it's polar day or polar night*)
Return[Re@dayLength];
];


(* ::Chapter::Closed:: *)
(*PV system related calculations*)


(* ::Subchapter:: *)
(*Performance ratio*)


(* ::Text:: *)
(*For temperature corrected or weather corrected PR (Ref NREL report: weather corrected performance ratio, Timothy Dierauf et al. ), temperature coefficient (sTC) and average temperature (weighted with in-plane irradiance) throughout the period (usually a year), avgT, are needed. *)
(*sTC in the unit of /K (usually negative).  *)
(*avgT and sT should be same unit, in \[Degree]C. *)
(**)
(*Weather corrected PR is most meaningful when monitoring is done for a limited period but it is desired to infer the annual PR. *)


fnPR[power_,ratedPower_,irradiance_]:=Module[{sPR},
sPR=power/(ratedPower*irradiance/1000);
Return[sPR]
];

fnPRcorrT[power_,ratedPower_,irradiance_,sT_,sTC_]:=Module[{sPRTcorr},
sPRTcorr=power/(ratedPower*irradiance/1000*(1-sTC*(25-sT)));
Return[sPRTcorr]
];

(*make sure power, irradiance, sT are lists of same length, same timestep*)
fnPRcorrW[power_,ratedPower_,irradiance_,sT_,sTC_,avgT_]:=Module[{sPRWcorr},
sPRWcorr=Total[power]/Total[ratedPower*irradiance/1000*(1-sTC*(avgT-sT))];
Return[sPRWcorr]
];



(* ::Subchapter::Closed:: *)
(*Cabling loss*)


(* ::Text:: *)
(*Energy losses in a cable is mainly due to resistive heating of the cable.*)
(*It is given by the following formula :*)
(*                                      E = a x R x Ib^2*)
(*Where :*)
(**)
(*E : energy losses in wires, Watt (W)*)
(*a : number of line coefficient, a=1 for single line, a = 3 for 3-phase circuit.*)
(*R : resistance of one active line*)
(*Ib : current in Ampere (A), must be non-negative. *)
(**)
(*R is given by the next formula :*)
(*                                    R = b x \[Rho]1 x L / S*)
(**)
(*b : length cable factor, b=2 for single phase wiring, b=1 for three-phased wiring.*)
(*\[Rho]1 : resistivity of the material conductor, 0.023 for copper and 0.037 for aluminum (ambient temperature = 25celcius degreeC) in ohm.mm2/m.*)
(*L : simple length of the cable (distance between the source and the appliance), in meters (m).*)
(*S : cross section of the cable in mm2*)
(**)
(*NB : for direct current the energy losses in percent is equal to the voltage drop in percent.*)
(*source: http://photovoltaic-software.com/DC_AC_drop_voltage_energy_losses_calculator.php*)


Options[fnCablingLoss]={noLineCoeff->1,lengthCableFactor->2,resistivity->0.023};
fnCablingLoss[current_,cableLength_,crossSection_,opt:OptionsPattern[]]:=Module[{loss,sR,b,phi,a},
a=OptionValue[noLineCoeff];
b=OptionValue[lengthCableFactor];
phi=OptionValue[resistivity];

If[current>0,
sR=b*phi*cableLength/crossSection;
loss=a*sR*current^2;
,
loss=0;
];

Return[loss]
];


(* ::Subchapter:: *)
(*Thermal models*)


moduleTCalc[airT_,irrLv_,tCoeff_,\[Eta]0_]:=Module[{\[Eta]c,cellT,t,\[Tau],\[Alpha],uL},
\[Tau]=0.95; (* transmittance of glazing *)
\[Alpha]=0.8; (* fraction of solar spectrum absorbed *)
uL=25; (* heat loss coefficient, adjustable, depends on module thermal environment *)
\[Eta]c[T_]:=\[Eta]0-tCoeff*(T-airT)*\[Eta]0;
cellT=t/.(FindRoot[t==airT+irrLv*((\[Tau]*\[Alpha])/uL)*(1-\[Eta]c[t]/(\[Tau]*\[Alpha])),{t,airT+10}]);
Return[cellT]
];


(* ::Subchapter:: *)
(*Fault detection*)


(* ::Text:: *)
(*Each input lists should be in the format of {timestamp, values}. *)
(*Faults are only considered for irradiance levels greater than 100 W/m2. *)
(*Currently two types of faults:*)
(*low current (likely open circuit, MPPT error, inverter protection on);*)
(*disconnected (no current AND voltage);*)


fnSimpleFaultDetect[listGVIP_,listPR_,listIratio_,listVratio_]:=Module[{combined,faultData},
combined=Flatten[{#,Rest/@{##2}}]&@@@(Pick[#,Length@#>3]&/@GatherBy[Join[listGVIP,listPR,listIratio,listVratio],First]);
faultData={};
If[
#[[2]]>50 && #[[7]]<0.15 && #[[8]]>0.15,AppendTo[faultData,Append[#,"low current"]];,
If[#[[2]]>50 && #[[7]]<0.05 && #[[8]]<0.05,AppendTo[faultData,Append[#,"disconnected"]]];
]&
/@combined;

Return[faultData]
];


(* ::Chapter:: *)
(*Analytical monitoring*)


(* ::Subchapter:: *)
(*Time series inspection*)


(* ::Text:: *)
(*Note: *)
(*Make sure column names are properly defined and correspond to data. *)
(*Make sure the unit for power in the input data is W, which indicate average power in a time step. Input temporal resolution should be specified in number of minutes (default is 1 minute). Reporting unit will be in KWh, which indicate energy in a certain period (default is 1 day, alternative can be 1 month, 1 hour, 10 minutes). *)
(*Each timestamp indicate the start of a time interval. *)
(*Make sure nominal power and irradiance (insolation) is in the same unit as Pdc and Pac. *)
(*Make sure all irradiance, voltage, current and power are positive in value. *)
(**)
(*Insolation is calculated whenever there is valid irradiance reading, valid range of is 0-2000W/m^2. *)
(*Yield is calculated whenever there is valid power reading. *)
(*PR is calculated only when there is valid irradiance reading (values between 20-2000W/m^2) AND power reading. *)
(**)
(*Full choice of column names is: columnNames->{"Timestamp","G","Vdc","Idc","Pdc","Vac","Iac","Pac","cum_meter_reading", "Tmod"}. (Tmod still not implemented yet)*)


Options[fnTimeSeriesInspection]={columnNames->{"Timestamp","G","Vdc","Idc","Pdc","Vac","Iac","Pac"},NominalPower->Null,Tc->Null,inputResolution->1,reportResolution->"Day"};
fnTimeSeriesInspection[data_,opt:OptionsPattern[]]:=Module[{columns,nominalP,resolution$input,resolution$output,irradianceThreshold,colIndex,powerDC=Null,integrateYieldDC=Null,specificYieldDC=Null,powerAC=Null,integrateYieldAC=Null,specificYieldAC=Null,insolation=Null,PRdc=Null,PRac=Null,PRdcTcorr=Null,PRacTcorr=Null,cumEnergy=Null,\[Eta]inverter=Null,plotOptions,outputData=<||>,plots=<||>},
columns=OptionValue[columnNames];
nominalP=OptionValue[NominalPower];
resolution$input=OptionValue[inputResolution];
resolution$output=OptionValue[reportResolution];
irradianceThreshold={20,2000};
plotOptions={Mesh->Full,ImageSize->Large,GridLines->Automatic,LabelStyle->{FontFamily->"Helvetica",FontSize->14,FontWeight->Bold,FontColor->Black}};

If[Length[columns]!=Dimensions[data][[2]],
Print["error: columns are not properly defined"];
Abort[];
];

colIndex=AssociationThread[columns->Range[Length[columns]]];

(* ----------------- DC yield and PR --------------------------- *)
If[MemberQ[columns,"Vdc"]&&MemberQ[columns,"Idc"],

(* calculate yield *)
powerDC={First@#,If[#[[2]]>0&&#[[3]]>0,#[[2]]*#[[3]],0]}&/@data[[All,{colIndex["Timestamp"],colIndex["Vdc"],colIndex["Idc"]}]];
integrateYieldDC={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[powerDC,minDataPts->1,period->resolution$output];

(*test if Vdc Idc and Pdc are consistent*)
If[MemberQ[columns,"Pdc"],
If[Abs@Mean@With[{x=Select[data[[All,{colIndex["Vdc"],colIndex["Idc"],colIndex["Pdc"]}]],Last@#>0&]},
(Times@@@x[[All,{1,2}]]-x[[All,{3}]])/x[[All,{3}]]]>0.05,
(*return warning if two values deviate more than 5%-rel*)
Print["warning: discrepancy between DC voltage current product and power"]; 
];
];

If[NumberQ[nominalP],
(* calculate specific yield *)
specificYieldDC={First@#,#[[2]]/nominalP*1000}&/@integrateYieldDC;

(* calculate insolation and PR *)
If[MemberQ[columns,"G"],
insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
PRdc={First@#,#[[3]]/nominalP/#[[2]]*1000}&/@fnPeriodSum[{#[[1]],#[[2]],#[[3]]*#[[4]]}&/@Select[data[[All,{colIndex["Timestamp"],colIndex["G"],colIndex["Vdc"],colIndex["Idc"]}]],irradianceThreshold[[1]]<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
,
If[MemberQ[columns,"G"],
insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
];

, (*else*)

If[MemberQ[columns,"Pdc"],
(*non valid readings like "NA" or negative values are removed*)
powerDC=Select[data[[All,{colIndex["Timestamp"],colIndex["Pdc"]}]],NumberQ[#[[2]]]&&NonNegative[#[[2]]]&];
integrateYieldDC={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[powerDC,minDataPts->1,period->resolution$output];

If[NumberQ[nominalP],
(* calculate specific yield *)
specificYieldDC={First@#,#[[2]]/nominalP*1000}&/@integrateYieldDC;

(* calculate insolation and PR *)
If[MemberQ[columns,"G"],
insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
PRdc={First@#,#[[3]]/nominalP/#[[2]]*1000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"],colIndex["Pdc"]}]],irradianceThreshold[[1]]<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
,
If[MemberQ[columns,"G"],
insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
];
];

];

AppendTo[outputData,"DC integrated yield"->integrateYieldDC];
AppendTo[plots,"DC integrated yield"->If[integrateYieldDC=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@integrateYieldDC,PlotLabel->"DC integrated yield",FrameLabel->{None,"yield (kWh)"},plotOptions]]];
AppendTo[outputData,"DC specific yield"->specificYieldDC];
AppendTo[plots,"DC specific yield"->If[specificYieldDC=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@specificYieldDC,PlotLabel->"DC specific yield",FrameLabel->{None,"yield (kWh)"},plotOptions]]];
AppendTo[outputData,"PRdc"->PRdc];
AppendTo[plots,"PRdc"->If[PRdc=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@PRdc,PlotLabel->"PRdc",FrameLabel->{None,"PR"},plotOptions]]];

(* ----------------- AC yield and PR --------------------------- *)
If[MemberQ[columns,"Vac"]&&MemberQ[columns,"Iac"],

(* calculate yield *)
powerAC={First@#,If[#[[2]]>0&&#[[3]]>0,#[[2]]*#[[3]],0]}&/@data[[All,{colIndex["Timestamp"],colIndex["Vac"],colIndex["Iac"]}]];
integrateYieldAC={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[powerAC,minDataPts->1,period->resolution$output];

(*test if Vac Iac and Pac are consistent*)
If[MemberQ[columns,"Pac"],
If[Abs@Mean[(Times@@@data[[All,{colIndex["Vac"],colIndex["Iac"]}]]-data[[All,{colIndex["Pac"]}]])/data[[All,{colIndex["Pac"]}]]]>0.05,Print["discrepancy between AC voltage current and power"];];
];

If[NumberQ[nominalP],
(* calculate specific yield *)
specificYieldAC={First@#,#[[2]]/nominalP*1000}&/@integrateYieldAC;

(* calculate insolation and PR *)
If[MemberQ[columns,"G"],
If[insolation===Null,
insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
PRac={First@#,#[[3]]/nominalP/#[[2]]*1000}&/@fnPeriodSum[{#[[1]],#[[2]],#[[3]]*#[[4]]}&/@Select[data[[All,{colIndex["Timestamp"],colIndex["G"],colIndex["Vac"],colIndex["Iac"]}]],irradianceThreshold[[1]]<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
,
If[MemberQ[columns,"G"]&&insolation===Null,
insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
];

, (*else*)

If[MemberQ[columns,"Pac"],
(*non valid readings like "NA" or negative values are removed*)
powerAC=Select[data[[All,{colIndex["Timestamp"],colIndex["Pac"]}]],NumberQ[#[[2]]]&&NonNegative[#[[2]]]&];
integrateYieldAC={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[powerAC,minDataPts->1,period->resolution$output];

If[NumberQ[nominalP],
(* calculate specific yield *)
specificYieldAC={First@#,#[[2]]/nominalP*1000}&/@integrateYieldAC;

(* calculate insolation and PR *)
If[MemberQ[columns,"G"],
If[insolation===Null,
insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
PRac={First@#,#[[3]]/nominalP/#[[2]]*1000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"],colIndex["Pac"]}]],irradianceThreshold[[1]]<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
,
If[MemberQ[columns,"G"]&&insolation===Null,
insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@fnPeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],minDataPts->1,period->resolution$output];
];
];
];

];

AppendTo[outputData,"AC integrated yield"->integrateYieldAC];
AppendTo[plots,"AC integrated yield"->If[integrateYieldAC=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@integrateYieldAC,PlotLabel->"AC integrated yield",FrameLabel->{None,"yield (kWh)"},plotOptions]]];
AppendTo[outputData,"AC specific yield"->specificYieldAC];
AppendTo[plots,"AC specific yield"->If[specificYieldAC=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@specificYieldAC,PlotLabel->"AC specific yield",FrameLabel->{None,"yield (kWh)"},plotOptions]]];
AppendTo[outputData,"insolation"->insolation];
AppendTo[plots,"insolation"->If[insolation=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@insolation,PlotLabel->"insolation",FrameLabel->{None,"insolation (kWh)"},plotOptions]]];
AppendTo[outputData,"PRac"->PRac];
AppendTo[plots,"PRac"->If[PRac=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@PRac,PlotLabel->"PRac",FrameLabel->{None,"PR"},plotOptions]]];

(* ---------- report cumulative energy meter readings, if applicable -------------- *)
If[MemberQ[columns,"cum_meter_reading"],
cumEnergy=fnPeriodSpan[data[[All,{colIndex["Timestamp"],colIndex["cum_meter_reading"]}]],minDataPts->2,period->resolution$output];
AppendTo[outputData,"meter reading"->cumEnergy];
AppendTo[plots,"meter reading"->If[cumEnergy=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@cumEnergy,PlotLabel->"meter reading",FrameLabel->{None,"yield (kWh)"},plotOptions]]];
];

(* --------------- calculate inverter efficiency if applicable ------------------- *)
If[powerDC=!=Null && powerAC=!=Null,
\[Eta]inverter=Flatten[{#,Rest/@{##2}}]&@@@(Pick[#,Length@#>1]&/@GatherBy[Join[powerDC,powerAC],First]);
\[Eta]inverter={First@#,#[[3]]/#[[2]]}&/@fnPeriodSum[Cases[\[Eta]inverter,{_,_?Positive,_}],minDataPts->1,period->resolution$output];
AppendTo[outputData,"\[Eta]inverter"->\[Eta]inverter];
AppendTo[plots,"\[Eta]inverter"->If[\[Eta]inverter=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@\[Eta]inverter,PlotLabel->"\[Eta]inverter",FrameLabel->{None,"\[Eta]"},plotOptions]]];
];

Return[{outputData,plots}];
];


(* ::Subchapter:: *)
(*Cross-sectional inspection*)


(* ::Text:: *)
(*Note: *)
(*make sure column names are properly defined and correspond to data. *)
(*make sure nominal power is in the same unit as Pdc and Pac. *)


Options[fnCrossSectionInspection]={columnNames->{"Gpoa","Vdc","Idc","Pdc","Tmod","Vac","Iac","Pac","Tamb"},NominalPower->Null};
fnCrossSectionInspection[data_,opt:OptionsPattern[]]:=Module[{columns,colIndex,nominalP,PRdc=Null,PRac=Null,\[Eta]inverter,dcPlots=<||>,acPlots=<||>,tempData,tempData2},
columns=OptionValue[columnNames];
nominalP=OptionValue[NominalPower];

SetOptions[ListPlot,ImageSize->Large,Frame->{{True,True},{True,True}},FrameStyle->Directive[Black,Thickness[0.003]],FrameTicks->{{Automatic,None},{Automatic,None}},GridLines->Automatic,LabelStyle->{FontFamily->"Helvetica",FontSize->18,FontWeight->Bold,FontColor->Black},ImageMargins->15];

If[Length[columns]!=Dimensions[data][[2]],
Print["error: columns are not properly defined"];
Abort[];
];

colIndex=AssociationThread[columns->Range[Length[columns]]];

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~ All plots involve irradiance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
If[MemberQ[columns,"Gpoa"],

(* ------------------------- plots involving DC power --------------------------------*)
If[MemberQ[columns,"Pdc"],
(*DC power-irradiance plot*)
AppendTo[dcPlots,"Pdc-Gpoa"->ListPlot[data[[All,{colIndex["Gpoa"],colIndex["Pdc"]}]],PlotLabel->"Pdc-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC power"}]];

If[NumberQ[nominalP],
(*Yf_dc-Yr plot*)
AppendTo[dcPlots,"Yf_dc-Yr"->ListPlot[{#[[1]]/1000,#[[2]]/nominalP}&/@data[[All,{colIndex["Gpoa"],colIndex["Pdc"]}]],PlotLabel->"Yf_dc-Yr",FrameLabel->{"reference yield","DC final yield"}]];

(*calculate PR_dc*)
PRdc=fnPR[data[[All,colIndex["Pdc"]]],nominalP,data[[All,colIndex["Gpoa"]]]];
];

,

(*alternative DC power-irradiance and Yf_dc-Yr plot and PR_dc calculation using Vdc and Idc*)
If[MemberQ[columns,"Vdc"]&&MemberQ[columns,"Idc"],
tempData={#[[1]],#[[2]]*#[[3]]}&/@(data[[All,{colIndex["Gpoa"],colIndex["Vdc"],colIndex["Idc"]}]]//Cases[{_?Positive,_?Positive,_?Positive}]);
AppendTo[dcPlots,"Pdc-Gpoa"->ListPlot[tempData,PlotLabel->"Pdc-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC power"}]];

If[NumberQ[nominalP],
AppendTo[dcPlots,"Yf_dc-Yr"->ListPlot[{#[[1]]/1000,#[[2]]/nominalP}&/@tempData,PlotLabel->"Yf_dc-Yr",FrameLabel->{"reference yield","DC final yield"}]];
PRdc=fnPR[tempData[[All,2]],nominalP,tempData[[All,1]]];
];
];
];

(* ------------------------- plots involving AC power --------------------------------*)
If[MemberQ[columns,"Pac"],
(*AC power-irradiance plot*)
AppendTo[acPlots,"Pac-Gpoa"->ListPlot[data[[All,{colIndex["Gpoa"],colIndex["Pac"]}]],PlotLabel->"Pac-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","AC power"}]];

If[MemberQ[columns,"Pdc"],
(*Pac-Pdc plot*)
AppendTo[acPlots,"Pac-Pdc"->ListPlot[data[[All,{colIndex["Pdc"],colIndex["Pac"]}]],PlotLabel->"Pac-Pdc",FrameLabel->{"DC power","AC power"}]];

(*inverter efficiency-irradiance plot*)
AppendTo[acPlots,"eff_inverter-Gpoa"->ListPlot[{#[[1]],#[[3]]/#[[2]]}&/@data[[All,{colIndex["Gpoa"],colIndex["Pac"],colIndex["Pdc"]}]],PlotLabel->"eff_inverter-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","inverter efficiency"}]];
];

If[NumberQ[nominalP],
(*Yf_ac-Yr plot*)
AppendTo[acPlots,"Yf_ac-Yr"->ListPlot[{#[[1]]/1000,#[[2]]/nominalP}&/@data[[All,{colIndex["Gpoa"],colIndex["Pac"]}]],PlotLabel->"Yf_ac-Yr",FrameLabel->{"reference yield","AC final yield"}]];

(*calculate PR_ac*)
PRac=fnPR[data[[All,colIndex["Pac"]]],nominalP,data[[All,colIndex["Gpoa"]]]];

(*Vac-Yf_ac plot*)
If[MemberQ[columns,"Vac"],
AppendTo[acPlots,"Vac-Yf_ac"->ListPlot[{#[[2]]/nominalP,#[[1]]}&/@data[[All,{colIndex["Vac"],colIndex["Pac"]}]],PlotLabel->"Vac-Yf_ac",FrameLabel->{"AC final yield","AC voltage (V)"}]];
];
];

,

(*alternative plots using Vac and Iac. For three phase, Vac and Iac should be the average. *)
If[MemberQ[columns,"Vac"]&&MemberQ[columns,"Iac"],
(*alternative AC power-irradiance plot, tempData={Gpoa,Pac}*)
tempData={#[[1]],#[[2]]*#[[3]]}&/@(data[[All,{colIndex["Gpoa"],colIndex["Vac"],colIndex["Iac"]}]]//Cases[{_?Positive,_?Positive,_?Positive}]);
AppendTo[acPlots,"Pac-Gpoa"->ListPlot[tempData,PlotLabel->"Pac-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC power"}]];
If[MemberQ[columns,"Pdc"],
(*alternative Pac-Pdc plot*)
AppendTo[acPlots,"Pac-Pdc"->ListPlot[{data[[All,colIndex["Pdc"]]],tempData[[All,2]]}\[Transpose],PlotLabel->"Pac-Pdc",FrameLabel->{"DC power","AC power"}]];
(*alternative inverter efficiency-irradiance plot*)
AppendTo[acPlots,"eff_inverter-Gpoa"->ListPlot[{#[[4]]/(#[[3]]*#[[2]]),#[[1]]}&/@data[[All,{colIndex["Gpoa"],colIndex["Vac"],colIndex["Iac"],colIndex["Pdc"]}]],PlotLabel->"eff_inverter-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","inverter efficiency"}]];
,
If[MemberQ[columns,"Vdc"]&&MemberQ[columns,"Idc"],
tempData2=data[[All,colIndex["Vdc"]]]*data[[All,colIndex["Idc"]]];
(*alternative Pac-Pdc plot*)
AppendTo[acPlots,"Pac-Pdc"->ListPlot[{tempData2,tempData[[All,2]]}\[Transpose],PlotLabel->"Pac-Pdc",FrameLabel->{"DC power","AC power"}]];
(*alternative inverter efficiency-irradiance plot*)
AppendTo[acPlots,"eff_inverter-Gpoa"->ListPlot[{#[[3]]/#[[2]],#[[1]]}&/@Join[tempData,List@tempData2,2],PlotLabel->"eff_inverter-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","inverter efficiency"}]];
];
];

If[NumberQ[nominalP],
(*alternative Yf_ac-Yr plot*)
AppendTo[dcPlots,"Yf_ac-Yr"->ListPlot[{#[[1]]/1000,#[[2]]/nominalP}&/@tempData,PlotLabel->"Yf_ac-Yr",FrameLabel->{"reference yield","AC final yield"}]];
(*alternative PRac calculation*)
PRac=fnPR[tempData[[All,2]],nominalP,tempData[[All,1]]];
(*alternative Vac-Yf_ac plot*)
AppendTo[acPlots,"Vac-Yf_ac"->ListPlot[{#[[2]]*#[[3]]/nominalP,#[[1]]}&/@data[[All,{colIndex["Vac"],colIndex["Iac"]}]],PlotLabel->"Vac-Yf_ac",FrameLabel->{"AC voltage (V)","AC final yield"}]];
];
];
];

(* ------------------------- plots involving module temperature --------------------------------*)
If[MemberQ[columns,"Tmod"],
(*Vdc-Tmod plot*)
If[MemberQ[columns,"Vdc"],
AppendTo[dcPlots,"Vdc-Tmod"->ListPlot[data[[All,{colIndex["Tmod"],colIndex["Vdc"]}]],PlotLabel->"Vdc-Tmod",FrameLabel->{"module temperature","DC voltage"}]];
];

(*PR-Tmod plot*)
If[PRdc=!=Null,
AppendTo[dcPlots,"PRdc-Tmod"->ListPlot[{data[[All,colIndex["Tmod"]]],PRdc}\[Transpose],PlotLabel->"PRdc-Tmod",FrameLabel->{"module temperature","PR_dc"}]];
];
If[PRac=!=Null,
AppendTo[acPlots,"PRac-Tmod"->ListPlot[{data[[All,colIndex["Tmod"]]],PRac}\[Transpose],PlotLabel->"PRac-Tmod",FrameLabel->{"module temperature","PR_ac"}]];
];

(*(Tmod-Tamb)-Yr plot*)
If[MemberQ[columns,"Tamb"],
AppendTo[dcPlots,"delta_Tmod_Tamb-Yr"->ListPlot[{#[[2]]-#[[3]],#[[1]]/1000}&/@data[[All,{colIndex["Gpoa"],colIndex["Tmod"],colIndex["Tamb"]}]],PlotLabel->"delta_Tmod_Tamb-Yr",FrameLabel->{"reference yield","Tmod-Tamb"}]];
];
];

(* ------------------------- others --------------------------------*)
If[MemberQ[columns,"Vdc"],
(*Vdc-irradiance plot*)
AppendTo[dcPlots,"Vdc-Gpoa"->ListPlot[data[[All,{colIndex["Gpoa"],colIndex["Vdc"]}]],PlotLabel->"Vdc-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC voltage (V)"}]];
];

If[MemberQ[columns,"Idc"],
(*Idc-irradiance plot*)
AppendTo[dcPlots,"Idc-Gpoa"->ListPlot[data[[All,{colIndex["Gpoa"],colIndex["Idc"]}]],PlotLabel->"Idc-Gpoa",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC current (A)"}]];
];

,

Print["warning: no irradiance data present"];
If[MemberQ[columns,"Tmod"]&&MemberQ[columns,"Vdc"],
(*Vdc-Tmod plot*)
AppendTo[dcPlots,"Vdc-Tmod"->ListPlot[data[[All,{colIndex["Vdc"],colIndex["Tmod"]}]],PlotLabel->"Vdc-Tmod",FrameLabel->{"module temperature","DC voltage (V)"}]];
];

If[MemberQ[columns,"Pac"]&&MemberQ[columns,"Vac"]&&NumberQ[nominalP],
AppendTo[acPlots,"Vac-Yf_ac"->ListPlot[{#[[2]]/nominalP,#[[1]]}&/@data[[All,{colIndex["Vac"],colIndex["Pac"]}]],PlotLabel->"Vac-Yf_ac",FrameLabel->{"AC yield","AC voltage"}]];
];

If[MemberQ[columns,"Pac"]&&MemberQ[columns,"Pdc"],
AppendTo[acPlots,"Pac-Pdc"->ListPlot[data[[All,{colIndex["Pdc"],colIndex["Pac"]}]],PlotLabel->"Pac-Pdc",FrameLabel->{"DC power","AC power"}]];
];

];

Return[acPlots~Join~dcPlots]
];


(* ::Chapter:: *)
(*Spectrum related calculations*)


(* ::Text:: *)
(*This function calculates integrated intensity from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. Additional care is taken to deal with the highly oscillatory nature of spectrum shape. *)


fnIntensity[spec_]:=Module[{spectrum,wavelength,integrate,intensity},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,First[wavelength],Last[wavelength]},Method->"Trapezoidal",MaxRecursion->20]&;
intensity=integrate/@spectrum;

Return[intensity]
];


(* ::Text:: *)
(*This function calculates average spectrum from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. *)


fnAvgSpec[spec_]:=Module[{spectrum,wavelength,integrate,intensity,intensitySum,avgSpec},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,First[wavelength],Last[wavelength]}]&;
intensity=integrate/@spectrum;
intensitySum=Total[intensity];
avgSpec=Thread[Times[spectrum,intensity],1]/intensitySum//Total;

Return[avgSpec]
];


(* ::Text:: *)
(*This function calculates (the list of) average photon energy (APE) from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. All quantities should be in SI units. *)


fnAPECalc[spec_]:=Module[{spectrum,wavelength,integrate,intensity,flux,APE},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,First[wavelength],Last[wavelength]}]&;
intensity=integrate/@spectrum;
flux=#*wavelength/(h*c)&/@spectrum;
flux=integrate/@flux;
APE=intensity/flux/q;

Return[APE]
];


(* ::Text:: *)
(*This function calculates (the list of) implied short circuit current from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. All quantities should be in SI units. *)


fnPhotocurrent[spec_]:=Module[{spectrum,wavelength,integrate,intensity,flux,APE,Jsc},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,First[wavelength],Last[wavelength]}]&;
flux=#*wavelength/(h*c)&/@spectrum;
flux=integrate/@flux;
Jsc=flux*q;

Return[Jsc]
];


(* ::Text:: *)
(*This function calculates (the list of) implied short circuit current in a range of wavelengths from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. All quantities should be in SI units. *)


fnPhotocurrent[spec_,{wMin_,wMax_}]:=Module[{spectrum,wavelength,integrate,intensity,flux,APE,Jsc},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,Max[First[wavelength],wMin],Min[Last[wavelength],wMax]}]&;
flux=#*wavelength/(h*c)&/@spectrum;
flux=integrate/@flux;
Jsc=flux*q;

Return[Jsc]
];
