(* ::Package:: *)

(* ::Title:: *)
(*PV System Analysis*)


(* ::Chapter:: *)
(*Initialization*)


(* :Copyright: Haohui Liu  *)

(* :Name: PV System Analysis *)

(* :Author: Dr. Liu Haohui *)

(* :Package Version: master_main *)

(* :Mathematica Version during inception: 12.0 *)

(*:Summary:
	Provide functions to faciliate analysis of PV performance data.
*)

BeginPackage["PVSystemAnalysis`"];


(* ::Section:: *)
(*General functions*)


(* ::Subsection:: *)
(*General*)


If[ Not@ValueQ[pick::usage],
pick::usage = "pick[condition_][x_] picks the elements from list/array x where the corresponding elements from another list/array meets a certain condition. Always wraps results in a list even when there is only one element (similar behavior to Position, Pick, ...).
Condition must be spcified in the format: another_list/array > (n or list/array of same dimenstion), argument must be numeric, support >, \[GreaterEqual], <, \[LessEqual], ==, \[NotEqual] and logical operators. "]

If[ Not@ValueQ[Where::usage],
Where::usage = "Where[condition,na (default Null)][x] sets elements from list/array x where the corresponding elements from another list/array meets a certain condition to na. 
Condition must be spcified in the format: another_list/array > (n or list/array of same dimenstion), argument must be numeric, support >, \[GreaterEqual], <, \[LessEqual], ==, \[NotEqual] and logical operators. "]
Where::dmism="Dimension mismatch where boolean mask is `1` and target is `2`";

If[ Not@ValueQ[extract::usage],
extract::usage = "extract[x_,pattern_] gives an operator that can extract corresponding elements from another list. Unlike other functions, single results will be returned as is instead of inside a list. 
e.g. extract[{1,2,4,5},_?(#>3&)]@{1,2,3,9} gives {3,9}"]

If[ Not@ValueQ[LogSpace::usage],
LogSpace::usage = "LogSpace[a,b,n] generates n logarithmically spaced points in the interval [a,b]. "]


If[ Not@ValueQ[ToDataset::usage],
ToDataset::usage = "Quick conversion of a table to a dataset."]

If[ Not@ValueQ[FromDataset::usage],
FromDataset::usage = "FromDataset[dataset,showHeader:False] convert a dataset to a table."]

If[ Not@ValueQ[AddIndex::usage],
AddIndex::usage = "AddIndex[dataset,index:0] adds index to a flat dataset by converting the n^th column specified by 'index' (0 means natural indexing). Note that duplicate values will be overwritten if any column is used as index. "]

If[ Not@ValueQ[DropIndex::usage],
DropIndex::usage = "Remove index into flat dataset. "]

If[ Not@ValueQ[DatasetColumns::usage],
DatasetColumns::usage = "Gives the column names of a flat dataset. "]

If[ Not@ValueQ[DatasetIndices::usage],
DatasetIndices::usage = "Gives the indices of a row indexed dataset. "]

If[ Not@ValueQ[RenameColumn::usage],
RenameColumn::usage = "RenameColumn[replaceRules][dataset] replaces column names with a list of rules. "]


(* ::Subsection::Closed:: *)
(*Time related*)


If[ Not@ValueQ[ReduceDateObject::usage],
ReduceDateObject::usage = "ReduceDateObject[dataset_,dateFormat_:{Month,/,Day,/,Year, ,Hour,:,Minute},position_:1] does quick conversion of DateObject back to string according to specified dateFormat. "]

If[ Not@ValueQ[ConvertDateObject::usage],
ConvertDateObject::usage = "ConvertDateObject[dataset_,dateFormat_:Automatic,timezone_:$TimeZone,position_:1] does quick conversion of DateObject to show in a specified timezone. 
Note: timezone default is set to machine local timezone (not dynamic). If timestamp is already in DateObject format, it will be converted to the specified timezone. "]


If[ Not@ValueQ[ToTemporalData::usage],
ToTemporalData::usage = "Quick conversion to a temporal data object."]

If[ Not@ValueQ[FromTemporalData::usage],
FromTemporalData::usage = "Quick conversion from a temporal data object to extract paths."]

If[ Not@ValueQ[RegularizeTimeSeries::usage],
RegularizeTimeSeries::usage = "RegularizeTimeSeries[data, resampleMethod,timesteps] makes the timestamp spacing uniform by filling in missing timestamps.
Assumes data is regular shaped table. Timestamps must be DateObject. 
Timestep specification can be any of the forms accepted by TimeSeriesResample (default is linear interpolation). "]


(* ::Subsection::Closed:: *)
(*Data import and manipulation*)


If[ Not@ValueQ[take::usage],
take::usage = "Operator form of function Take."]

If[ Not@ValueQ[TestArray::usage],
TestArray::usage = "TestArray[row_,col_] generates a row by col position numbered test array."]

If[ Not@ValueQ[MultiImport::usage],
MultiImport::usage = "Import multiple files at the same time."]

If[ Not@ValueQ[Glimpse::usage],
Glimpse::usage = "quick look at data dimension and first few rows."]

If[ Not@ValueQ[MergeData::usage],
MergeData::usage = "This function merges multiple datasets with a common key (e.g. timestamp). The input needs to be a table of datasets: {dataset1, dataset2, ...}. 
Format of individual datasets must be tables, or flat (not hierarchical) and (only) column indexed Dataset objects. It is not advisable to use DateObject as the timestamp as its exact form may not be the same while appearing to be the same timestamp. "]

Options[MergeData]={KeyPosition->1,Header->False,JoinMethod->"Outer",keyCollisionFunction->Right};


If[ Not@ValueQ[RunningAverage::usage],
RunningAverage::usage = "This function select non-missing and daytime data, obtain averaged values for every x minute interval."]

Options[RunningAverage]={ReportPeriod->Quantity[10,"Minutes"]};

If[ Not@ValueQ[DataSummary::usage],
DataSummary::usage = "Simple summary of data shape."]

If[ Not@ValueQ[GetNASAPowerData::usage],
GetNASAPowerData::usage = "GetNASAPowerData[lat_,lon_,par_:\"ALLSKY_SFC_SW_DWN,T2M,WS10M,PRECTOT\",temporalType_:\"CLIMATOLOGY\",start_:Null,end_:Null] imports NASA POWER project data sets via API for a single location."]


(* ::Subsection::Closed:: *)
(*Quick ReportPeriod statistics*)


If[ Not@ValueQ[PeriodSum::usage],
PeriodSum::usage = "PeriodSum[data] calculates the sums of a binned time window. Data should be dataset or table of the format {{time, value1, value2, ...}, ...}. 
Default options are: {MinDataPts->60,ReportPeriod->\"Day\",Scaling\[Rule]1}. Supported ReportPeriod are: \"Month\", \"Day\", \"Hour\", \"10 Minutes\". "]

Options[PeriodSum]={MinDataPts->60,ReportPeriod->"Day",Scaling->1};

If[ Not@ValueQ[PeriodSpan::usage],
PeriodSpan::usage = "PeriodSpan[data] calculates the span of values within each binned time window. Data should be dataset or table of the format {{time, value1, value2, ...}, ...}. 
Default options are: {MinDataPts->60,ReportPeriod->\"Day\"}. Supported ReportPeriod are: \"Month\", \"Day\", \"Hour\", \"10 Minutes\". "]

Options[PeriodSpan]={MinDataPts->60,ReportPeriod->"Day"};

If[ Not@ValueQ[PeriodAverage::usage],
PeriodAverage::usage = "PeriodAverage[data] calculates the arithmetic average within each binned time window. Data should be dataset or table of the format {{time, value1, value2, ...}, ...}. 
Default options are: {MinDataPts->60,ReportPeriod->\"Day\"}. Supported ReportPeriod are: \"Month\", \"Day\", \"Hour\", \"10 Minutes\"."]

Options[PeriodAverage]={MinDataPts->60,ReportPeriod->"Day"};

If[ Not@ValueQ[PeriodWeightedAvg::usage],
PeriodWeightedAvg::usage = "PeriodWeightedAvg[data,weight] calculates the weighted average within each running time window. Data and weights should have the format {{time, value(s)}, ...}. "]

Options[PeriodWeightedAvg]={MinDataPts->60,ReportPeriod->"Day"};

If[ Not@ValueQ[PeriodStats::usage],
PeriodStats::usage = "PeriodStats[data,start_time,end_time] calculates the stats within each running time window as defined by the option \"function\" (one of following: fnAvg, fnCount, fnCountValid, fnSum, fnWgtAvg).
default options: {TimeStep->Quantity[1,\"Days\"],function->fnAvg,windowAlignment->Left,windowPadding->None,weightPosition->Null,\"TimeZone\"->$TimeZone}. 

Start and end time should be DateObjects. They can also be specified in DateList format of a DateObject, but need to ensure the output of DateList is in local time zone of evaluation (computer system time zone). 
Weighted average calculation is triggered by specifying option weightPosition (the value in option function will be overwritten),
	needs dataset to contain weighting data, 
	weight position should be specified as: column_number_of_the_weight - 1 (minus timestamp column) ,
	weighted average for the weighting data should not be used.
No minimum data point requirement is defined, results will not be Missing as long as there is data. 
Make sure timestamp is contained in the first column and preferably in DateObject format to avoid ambiguous interpretation when converting to TimeSeries object. 
PeriodStats conserves true time but may not output the DateObject in the original time zone unless the correct time zone is specified (to avoid error of not supported by TimeSeries functions, numerical offset will always be used internally for timezone specifications). 
PeriodStats reorders unsorted data into one sorted by timestamp. 
PeriodStats maps function in timesteps, so does not support binning by natural days/months. It is better used for reducing temporal resolution. "]

Options[PeriodStats]={TimeStep->Quantity[1,"Days"],function->fnAvg,windowAlignment->Left,windowPadding->None,weightPosition->Null,"TimeZone"->$TimeZone};

If[ Not@ValueQ[PeriodMaster::usage],
PeriodMaster::usage = "PeriodMaster[data, function_String(default \"fnAvg\")] maps functions to each binned time window. Data should be dataset or table of the format {{time, value1, value2, ...}, ...}. 
Default options are: {MinDataPts->60,ReportPeriod->\"Day\"}. Supported ReportPeriod are: \"Month\", \"Day\", \"Hour\", \"10 Minutes\"
Currently supported functions are: 
- fnAvg: performs averaging to the binned dataset x with data dimension d, without including non-numeric data. 
- fnCount: simply counts the number of data points present in each bin of time window.
- fnCountValid: counts the number of valid numeric datapoints present in each bin of time window. 
- fnSum: gives the sum of valid numeric datapoints present in each bin of time window.
"]

Options[PeriodMaster]={MinDataPts->60,ReportPeriod->"Day"};


(* ::Text:: *)
(*Supporting functions: *)


If[ Not@ValueQ[fnAvg::usage],
fnAvg::usage = "performs averaging to the binned dataset x with data dimension d, without including non-numeric data."]

If[ Not@ValueQ[fnCount::usage],
fnCount::usage = "simply counts the number of data points present in each bin of time window."]

If[ Not@ValueQ[fnCountValid::usage],
fnCountValid::usage = "counts the number of valid numeric datapoints present in each bin of time window."]

If[ Not@ValueQ[fnSum::usage],
fnSum::usage = "gives the sum of valid numeric datapoints present in each bin of time window."]


(* ::Section::Closed:: *)
(*Plotting related*)


If[ Not@ValueQ[TwoAxisPlot::usage],
TwoAxisPlot::usage = "Two axis plotting: TwoAxisPlot[{f,g},{x,x1,x2}]."]

If[ Not@ValueQ[TwoAxisListPlot::usage],
TwoAxisListPlot::usage = "Two axis plotting: TwoAxisListPlot[{f,g}]."]

If[ Not@ValueQ[TwoAxisListLinePlot::usage],
TwoAxisListLinePlot::usage = "Two axis plotting: TwoAxisListLinePlot[{f,g}]."]

If[ Not@ValueQ[HighlightData::usage],
HighlightData::usage = "HighlightData[data_,logic_,positions_:\"default\",color_:Red] labels the selected columns (specified by positions) in a certain color."]

If[ Not@ValueQ[EnhancedShow::usage],
EnhancedShow::usage = "Enhanced plots with some reformatting."]

If[ Not@ValueQ[ExtractPlotData::usage],
ExtractPlotData::usage = "Extract datapoints from a plot."]

If[ Not@ValueQ[AddTrendline::usage],
AddTrendline::usage = "Add a line of best linear fit to a plot."]

Options[AddTrendline]={"ShowEquation"->True,"PlaceEquation"->Scaled[{0.6,0.8}]};

If[ Not@ValueQ[FigureAlbum::usage],
FigureAlbum::usage = "Inspect a set of plots."]


(* ::Section:: *)
(*Solar related*)


(* ::Subsection::Closed:: *)
(*Solar geometry and meteorological*)


If[ Not@ValueQ[DayLength::usage],
DayLength::usage = "DayLength[Julian day,latitude] calculates day length in number of hours."]

If[ Not@ValueQ[RemoveNightTime::usage],
RemoveNightTime::usage = "Remove data points corresponding to night time. At the same time converting timestamps to DateObjects. Default options: {\"DateFormat\"->{\"Day\",\"/\",\"Month\",\"/\",\"Year\",\" \",\"Hour\",\":\",\"Minute\"},Location->$GeoLocation,\"TimeZone\"->Null}. 
Works with tables or datasets with first column as the timestamps. 
Automatically converts timestamp to DateObject if the timestamp is in string format, DateFormat specifies the format to interpret the date string. 
Make sure timestamp of input data is in local timezone of the site but not of the local computer doing the evaluation. \
As all timestamps will be converted to DateList which preserves the form but not the true time, time zone of the input timestamps must be specified explicitly (takes precedence) or by location (note that local time zone for a location may sometimes not be the intended time zone in the input data timestamps due to daylight saving time confusion, e.g. LocalTimeZone for Denver is MDT but timestamps in Denver are also commonly specified in MST). \
If time zone is not explicitly specified, local time zone of the location is assumed. If input data timestamp is in string format, it must be denoted in the intended and specified timezone. If input data timestamp is a DateObject, it can be denoted in any time zone, true time will be preserved. However, it is still advisable to specify target timezone as the local time zone of the intended location (at least not to far from it), \
otherwise may confuse and get Sunrise or Sunset time in the wrong day (in case where input data timestamp is on the other half of the globe as the intended location of which sunrise and sunset time is determined). "]

Options[RemoveNightTime]={"DateFormat"->{"Day","/","Month","/","Year"," ","Hour",":","Minute"},Location->$GeoLocation,"TimeZone"->Null};

If[ Not@ValueQ[ArrayPitch::usage],
ArrayPitch::usage = "ArrayPitch[tilt, width, \[Theta]limit] calculates the required pitch given array tilt, collector width, and desired shading limit angle."]

If[ Not@ValueQ[ShadeLimitAngle::usage],
ShadeLimitAngle::usage = "ShadeLimitAngle[tilt, width, pitch] calculates the shading limit angle."]

If[ Not@ValueQ[GCR::usage],
GCR::usage = "GCR[tilt, \[Theta]limit] estimates the ground coverage ratio for a desired tilt and shading limit angle."]

If[ Not@ValueQ[AoiProjection::usage],
AoiProjection::usage = "AoiProjection[tilt,orientation,sunZenith,sunAzimuth] returns the cosine of the incidence angle between sunlight and module plane."]

If[ Not@ValueQ[AngleOfIncidence::usage],
AngleOfIncidence::usage = "AngleOfIncidence[tilt,orientation,sunZenith,sunAzimuth] returns the incidence angle between sunlight and module plane."]



(* ::Subsection::Closed:: *)
(*PV system related calculations*)


If[ Not@ValueQ[PR::usage],
PR::usage = "PR[power,ratedPower,irradiance] calculates the performance ratio."]

If[ Not@ValueQ[PRcorrT::usage],
PRcorrT::usage = "PRcorrT[power,ratedPower,irradiance,T,Tc] calculates the temperature corrected performance ratio."]

If[ Not@ValueQ[PRcorrW::usage],
PRcorrW::usage = "PRcorrW[power,ratedPower,irradiance,T,Tc,avgT] calculates the temperature corrected performance ratio."]

If[ Not@ValueQ[CalcPR::usage],
CalcPR::usage = "CalcPR[table,powerCol,ratedPower,irrCol] calculates the performance ratio and append as a column for a dataset."]

If[ Not@ValueQ[CalcPRcorrT::usage],
CalcPRcorrT::usage = "CalcPR[table,powerCol,ratedPower,irrCol,T,Tc] calculates the performance ratio and append as a column for a dataset."]

If[ Not@ValueQ[CalcPRcorrW::usage],
CalcPRcorrW::usage = "CalcPR[table,powerCol,ratedPower,irrCol,T,Tc,avgT] calculates the performance ratio and append as a column for a dataset."]

If[ Not@ValueQ[VoltageRatio::usage],
VoltageRatio::usage = "VoltageRatio calculates the ratio of actual voltage to Voc (STC and expected) with simple temperature and irradiance correction."]

If[ Not@ValueQ[VoltageRatio2::usage],
VoltageRatio2::usage = "VoltageRatio2 calculates the ratio of actual voltage to Voc (STC and expected) without irradiance correction."]

If[ Not@ValueQ[CurrentRatio::usage],
CurrentRatio::usage = "CurrentRatio calculates the ratio of actual current to Isc (STC and expected)."]

If[ Not@ValueQ[ConversionFactor::usage],
ConversionFactor::usage = "Dictionary for factors to multiply when doing conversions.
Supported keys: lookup (list all keys), J->kWh, \:4e07\:5343\:74e6->MW, \:4e07\:5343\:74e6->GW, Sum W/min->kWh, Sum W/5min->kWh, Sum W/10min->kWh, Sum W/15min->kWh, Sum W/hour->kWh.
"]

If[ Not@ValueQ[CablingLoss::usage],
CablingLoss::usage = "CablingLoss[current,cableLength,crossSection] calculates the cabling loss in W."]

Options[CablingLoss]={"NumLineCoeff"->1,"LengthCableFactor"->2,"Resistivity"->0.023};

If[ Not@ValueQ[ModuleTemperature::usage],
ModuleTemperature::usage = "ModuleTemperature[airT (in \[Degree]C), irradiance, Tc (default -0.003), \[Eta]stc (default 0.2), U-value (default 29)] estimates the PV module temperature based on simple heat balance model (Faiman 2008). "]

If[ Not@ValueQ[SimpleFaultDetect::usage],
SimpleFaultDetect::usage = "SimpleFaultDetect[listGVIP,listPR,listIratio,listVratio] detects and highlights obvious faults."]


(* ::Subsection::Closed:: *)
(*Analytical monitoring*)


If[ Not@ValueQ[TimeSeriesInspection::usage],
TimeSeriesInspection::usage = "High level time series inspection and plots of system performance KPIs. 
{outputData,plots}=TimeSeriesInspection[data,columns(optional)], columns by default is {Timestamp,G,Vdc,Idc,Pdc,Vac,Iac,Pac}, others can include: {cum_meter_reading, Tmod}. 
Default options are {NominalPower->Null,Tc->Null,InputResolution->1,ReportPeriod->\"Day\"}. 
Note: 
Make sure column names are properly defined and correspond to data. 
Make sure the unit for power in the input data is W, which indicate average power in a time step. Input temporal resolution should be specified in number of minutes (default is 1 minute). Reporting unit will be in KWh, which indicate energy in a certain ReportPeriod (default is 1 day, alternative can be 1 month, 1 hour, 10 minutes). 
Each timestamp indicate the start of a time interval. 
Make sure nominal power and irradiance (insolation) is in the same unit as Pdc and Pac. 
Make sure all irradiance, voltage, current and power are positive in value. 
Insolation is calculated whenever there is valid irradiance reading, valid range of is 0-2000W/m^2. 
Yield is calculated whenever there is valid power reading. 
PR is calculated only when there is valid irradiance reading (values between 20-2000W/m^2) AND power reading. 
PR for DC and AC side is calculated separately, so discrepancy may exist. "]

Options[TimeSeriesInspection]={NominalPower->Null,Tc->Null,InputResolution->1,ReportPeriod->"Day"};

If[ Not@ValueQ[TimeSeriesSummary::usage],
TimeSeriesSummary::usage = "Quick summary table of outputs from TimeSeriesInspection. "]

If[ Not@ValueQ[TimeSeriesInspect::usage],
TimeSeriesInspect::usage = "TimeSeriesInspect[data,start,end,plot options] returns {cropped timeseries, DateListPlots for each column}. "]

If[ Not@ValueQ[TimeSeriesAlbum::usage],
TimeSeriesAlbum::usage = "TimeSeriesAlbum[dataIn,groupBy:{Year,Month,Day},plotOptions] gives an object containing data slices and their plots for each column. 
Argument groupBy specifies how data should be binned and the corresponding key formats for each bin, can take in permitted arguments for function DateString.
Object contains four elements: 
\"bins\" gives keys for time slices; 
\"columns\" gives columns (parameters); 
\"data\" gives a pure function with one argument call (bin) and returns data in that bin;
\"plot\" gives a pure function with two argument calls: DateListPlot[bin,column]; 
\"album\" is interactive exploration of data. "]

If[ Not@ValueQ[CrossSectionInspection::usage],
CrossSectionInspection::usage = "Cross sectional inspection and plots of system performance. Default columns are {\"G\",\"Vdc\",\"Idc\",\"Pdc\",\"Tmod\",\"Vac\",\"Iac\",\"Pac\",\"Tamb\"}. 
Default options are {NominalPower->Null,PlotOptions->{}}. Use PlotOptions to specify additional options for ListPlot.  
crossPlots=CrossSectionInspection[pvData] takes in pv data object prepared by PVDataPrep.
crossPlots=CrossSectionInspection[pvData,\"bin\"] takes in subsetted pv data object prepared by PVDataPrep with key \"bin\".
{keys,crossPlot}=CrossSectionInspection[pvData,\"ZoomIn\",groupBy (default is by days)] gives crossPlot as a function to inspect each bin of data. "]

Options[CrossSectionInspection]={NominalPower->Null, PlotOptions->{}};

If[ Not@ValueQ[PVDataPrep::usage],
PVDataPrep::usage = "PVDataPrep[data,columns,nominal_power,groupBy(optional)] prepares a PV data object to be passed into analytical monitoring functions. Returns an association with keys: {\"PVDataObject\"=True,\"data\",\"columns\",\"data_binned\",\"bins\",\"NominalPower\",\"InputResolution\",\"Tc\",\"ReportPeriod\"}.
Default options are: {\"DateFormat\"->{\"Day\",\"/\",\"Month\",\"/\",\"Year\",\" \",\"Hour\",\":\",\"Minute\"},\"TimeZone\"\[Rule]$TimeZone,\"Pdc_unit\"\[Rule]\"kW\",\"Pac_unit\"\[Rule]\"kW\",\"Tamb_unit\"\[Rule]\"Celcius\",\"Capacity_unit\"\[Rule]\"kW\"}"]

Options[PVDataPrep]={"DateFormat"->{"Day","/","Month","/","Year"," ","Hour",":","Minute"},"TimeZone"->$TimeZone,"Pdc_unit"->"kW","Pac_unit"->"kW","Tamb_unit"->"Celcius","Capacity_unit"->"kW",Tc->Null,ReportPeriod->"Day",InputResolution->Automatic};



(* ::Subsection::Closed:: *)
(*Spectrum related*)


If[ Not@ValueQ[SpecScale::usage],
SpecScale::usage = "SpecScale[scale_][spec_] performs simple scaling of spectrum. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. "]

If[ Not@ValueQ[SpecIntensity::usage],
SpecIntensity::usage = "This function calculates integrated intensity from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. "]

If[ Not@ValueQ[SpecAverage::usage],
SpecAverage::usage = "This function calculates average spectrum from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. "]

If[ Not@ValueQ[APECalc::usage],
APECalc::usage = "This function calculates (the list of) average photon energy (APE) from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. All quantities should be in SI units. "]

If[ Not@ValueQ[Photocurrent::usage],
Photocurrent::usage = "Photocurrent[spectra] calculates (the list of) implied short circuit current from a set of spectra. 
Photocurrent[spec_,{wMin_,wMax_}] calculates it for a given range of wavelength. "]


(* ::Section::Closed:: *)
(*End of initialization*)


Begin["`Private`"];


(* ::Chapter::Closed:: *)
(*Constants*)


q=1.6*10^-19;
k=1.381*10^-23;
c=3*10^8;
h=6.626*10^-34;


(* ::Chapter:: *)
(*General functions*)


(* ::Section:: *)
(*General*)


(* ::Subsection::Closed:: *)
(*Language core*)


SetAttributes[QuietCheck,{HoldAll}];
QuietCheck[expr_,failexpr_,msgs:(_MessageName|{__MessageName}|_String)]:=Quiet[Check[expr,failexpr,msgs],msgs];
QuietCheck[expr_,failexpr_]:=Quiet[Check[expr,failexpr]];


PositionLargest[list_List] /; AllTrue[list, NumericQ] := First[FirstPosition[list, Max[list]]]
 
PositionLargest[list_List, n_Integer] /; AllTrue[list, NumericQ] := Take[Flatten[(Position[list, #1] & ) /@ DeleteDuplicates[TakeLargest[list, n]]], n]
 
PositionLargest[list_List, HoldPattern[UpTo][n_Integer]] /; AllTrue[list, NumericQ] := Take[Flatten[(Position[list, #1] & ) /@ DeleteDuplicates[TakeLargest[list, UpTo[n]]]], UpTo[n]]


Rarest[l_List] := MinimalBy[Tally[l], Last][[All,1]]
 
Rarest[l_List, n_] := SortBy[MinimalBy[Tally[l], Last, n][[All,1]], FirstPosition[l, #1] & ]


LogSpace[a_,b_,n_]:=N[10^Range[Log10@a,Log10@b,(Log10@b-Log10@a)/n]];


(* ::Subsection::Closed:: *)
(*Boolean masking*)


pick[condition_][x_]:=Pick[x,ResourceFunction["BoolEval"][condition],1]
SetAttributes[pick,HoldAll]


Where[condition_,na_:Null][x_]:=Module[{booleanMask=ResourceFunction["BoolEval"][condition]},

If[Dimensions@booleanMask!=Dimensions@x,
	Message[Where::dmism,Dimensions@booleanMask,Dimensions@x];
	Return@$Failed;
,
	Return@ReplacePart[x,Position[booleanMask,1]->na]
];

];
SetAttributes[Where,HoldAll]


extract[x_,pattern_]:=Extract[Replace[Position[x,pattern],{{p_Integer}}:>p]];


(* ::Subsection::Closed:: *)
(*JSON Viewer*)


ruleListQ[{r__Rule}]=True;(*JSON won't have RuleDelayed*)ruleListQ[_]=False;

formatJSON[json_]:=Switch[json,
_?ruleListQ,(*dictionary*)
	Column@Replace[json,HoldPattern[a_->b_]:>OpenerView[{a,formatJSON[b]}],{1}],
_?(ArrayQ[#,_,AtomQ]&),(*AtomQ makes sure that only arrays of basic types are formatted like this,not arrays of dictionaries*)
	TableForm[json],
_List,(*list of non-basic types,including ragged arrays*)
	Column[formatJSON/@json,Frame->All],
_,(*anything else*)
	json]


(* ::Subsection::Closed:: *)
(*Dataset handling*)


(* ::Text:: *)
(*Convert a flat table to Dataset (DataFrame) object assuming first row as titles (column names). *)
(*When table does not contain column names, the title row can be supplied as the second argument. *)
(*Index can be specified, in which case should be given as an integer indicating which column is used as index. *)


ToDataset[table_List,titles_:"Default",index_:None]:=Module[{dataset},
If[titles==="Default",
	dataset=AssociationThread[First@table->#]&/@Rest[table]//Dataset;
,
	dataset=AssociationThread[titles->#]&/@table//Dataset;
];

If[index=!=None&&IntegerQ@index,
	dataset=dataset[All,#[[index]]->Drop[#,{index}]&]//Normal//Association//Dataset;
];

Return[dataset]
];


(* ::Text:: *)
(*Convert back to a table with or without column names. *)


FromDataset[dataset_Dataset,showHeader_:False]:=Module[{dim=Dimensions@dataset,d},

If[dim==={0}, Return@{}; Abort[]];
If[Length@dim==1, d=First@dim;, d=dim[[2]];];

Which[ArrayDepth@dataset==1 && d!=1,
		If[showHeader,Prepend[Normal@Values@dataset,Normal@Keys@First@dataset],Normal@Values@dataset],
	ArrayDepth@dataset==1 && d==1,
		If[showHeader,Flatten[Prepend[Normal@Values@dataset,Normal@Keys@First@dataset],1],Flatten[Normal@Values@dataset,1]],
	ArrayDepth@dataset==2 && d!=1, (* else, array depth is 2 *)
		If[showHeader,Prepend[KeyValueMap[List/*Flatten,Values/@Normal@dataset],Prepend[Normal@Keys@First@dataset,"index"]],KeyValueMap[List/*Flatten,Values/@Normal@dataset]]
]

];


(* ::Text:: *)
(*Add index to a flat dataset. *)


AddIndex[dataset_Dataset/;ArrayDepth@dataset==1,index_:0]:=If[index==0,
MapIndexed[First@#2->#1&,dataset]//Normal//Association//Dataset
,
dataset[All,#[[index]]->Drop[#,{index}]&]//Normal//Association//Dataset
];


(* ::Text:: *)
(*Remove index into flat dataset.*)


DropIndex[dataset_Dataset/;ArrayDepth@dataset==2,index_:0]:=dataset//Values;

DropIndex[dataset_Dataset/;ArrayDepth@dataset==1,index_:0]:=dataset;


(* ::Text:: *)
(*Get column or index names. *)


DatasetColumns[dataset_Dataset/;ArrayDepth@dataset==1]:=dataset//Normal//First//Keys;

DatasetColumns[dataset_Dataset/;ArrayDepth@dataset==2]:=dataset//Values//Normal//First//Keys;


DatasetIndices[dataset_Dataset/;ArrayDepth@dataset==2]:=dataset//Normal//Keys;


(* ::Text:: *)
(*Rename columns. *)


RenameColumn[replaceRules_List][dataset_Dataset/;ArrayDepth@dataset==1]:=KeyMap[Replace[replaceRules]]/@dataset;
RenameColumn[replaceRules_List][dataset_Dataset/;ArrayDepth@dataset==2]:=KeyMap[Replace[replaceRules]]/@DropIndex@dataset;


(* ::Text:: *)
(*Append columns.*)


AppendColumn[array_List, x_List] := Transpose[Append[Transpose[array], x]];
AppendColumn[array_List, x_] := Append[#, x]& /@ array;
AppendColumn[x_] := Function[mat,AppendColumn[mat,x]]

AppendColumn[array_Dataset/;ArrayDepth@array==1, x_List,colName_String:"new_column"] := Dataset[
Table[
	Append[Normal[array][[i]],colName->x[[i]]]
,{i,Length@x}]
]

AppendColumn[array_Dataset/;ArrayDepth@array==2, x_List,colName_String:"new_column"] := Module[{table=FromDataset[array,True],output},

output=AppendColumn[Rest@table,x];
output=ToDataset[output,Append[First@table,colName]]//AddIndex[#,1]&

]


(* ::Section:: *)
(*Time related*)


(* ::Subsection::Closed:: *)
(*DateObject handling*)


ReduceDateObject[dataset_,dateFormat_:{"Month","/","Day","/","Year"," ","Hour",":","Minute",":","Second"},position_:1]:=Block[{reducedOutput,$DateStringFormat=dateFormat},

If[dateFormat==="DateList",
	reducedOutput=Map[MapAt[DateList,#,position]&,dataset];
,
	reducedOutput=Map[MapAt[DateString,#,position]&,dataset];
];

Return[reducedOutput]
];


ConvertDateObject[dataset_,dateFormat_:Automatic,timezone_:$TimeZone,position_:1]:=Module[{output,convertFn},

If[dateFormat===Automatic,
	convertFn=DateObject[#,TimeZone->timezone]&;
	,
	convertFn=If[Head@#=!=DateObject,DateObject[DateList[{#,dateFormat}],TimeZone->timezone],DateObject[#,TimeZone->timezone]]&;
];

output=Map[MapAt[convertFn,#,position]&,dataset];

Return@output;
];


(* ::Subsection::Closed:: *)
(*Convert to and from time series object*)


(* ::Text:: *)
(*Assumes data is regular shaped table, all have DateObject timestamps (if not, automatic conversion to DateObject will be done with system default methods, so better control the conversion beforehand). *)
(*Type of input data can be Dataset, or tables with pure timestamp-value(s) pairs, or tables with title rows. *)
(*Option such as Timezone can be specified (note that this only changed the form appearance but not the true time of the DateObject). *)


ToTemporalData[data_,opt:OptionsPattern[]]:=Which[
Dimensions[data][[2]]==2,
	TimeSeries[If[Head@data===Dataset,Normal@Values@data,DeleteCases[data,{__String}]],opt],
Dimensions[data][[2]]>2,
	With[{d=If[Head@data===Dataset,Normal@Values@data,DeleteCases[data,{__String}]]},
	TemporalData[d[[All,2;;]]\[Transpose],{First/@d},opt]]
];


(* ::Text:: *)
(*Note that time is converted back into DateObject assuming local machine timezone. *)


FromTemporalData[ts_]:=If[
ts["PathCount"]==1,
	ts["DatePath"]
,
	Prepend[ts["ValueList"],First@ts["DateList"]]\[Transpose]
];


(* ::Subsection::Closed:: *)
(*Regularize timestamp spacing*)


(* ::Text:: *)
(*Assumes data is regular shaped table. Timestamps must be DateObject. *)
(*Timestep specification can be any of the forms accepted by TimeSeriesResample. *)


RegularizeTimeSeries[data_/;Head@data[[1,1]]===DateObject,resampleMethod_:Automatic,timesteps_:Automatic]:=Block[{$TimeZone=data[[1,1]]["TimeZone"],ts},

ts=ToTemporalData@data;
Return@FromTemporalData@TimeSeriesResample[ts,timesteps,ResamplingMethod->resampleMethod];

];


(* ::Section:: *)
(*Data import and manipulation*)


(* ::Subsection::Closed:: *)
(*User convenience*)


take[n_]:=Take[#,n]&;


TestArray[row_,col_]:=Array[10#1+#2&,{row,col}];


(* ::Subsection::Closed:: *)
(*Import multiple files*)


MultiImport[files_]:=Module[{output},
output=Import[#]&/@files;
output=Flatten[output,1];

Return[output]
];

MultiImport[files_,format_]:=Module[{output},
output=Import[#,format]&/@files;
output=Flatten[output,1];

Return[output]
];


(* ::Subsection::Closed:: *)
(*Quick glimpse of imported data*)


Glimpse[data_List,rowsToShow_:10]:=Block[{},
	Print@Dimensions@data;
	Print@TableForm@Prepend[Take[data,rowsToShow],Range@Dimensions[data][[2]]];
];


Glimpse[data_Dataset]:=Glimpse[FromDataset[data,True]];


(* ::Subsection::Closed:: *)
(*Merge data*)


(* ::Text:: *)
(*The basic method is the convert these datasets into associations and use Mathematica built-in function JoinAcross to perform data merging. *)
(*The option KeyPosition indicates the column at which this common key resides (can be a number of column name). *)
(*The option JoinMethod specifies the joining method which will be used in function JoinAcross. *)
(*The input "data" needs to be a table of datasets: {dataset1, dataset2, ...}. *)
(*Format of individual datasets must be tables, or flat (not hierarchical) and (only) column indexed Dataset objects. *)
(*It is not advisable to use DateObject as the timestamp as its exact form may not be the same while appearing to be the same timestamp. *)


MergeData[data_,opt:OptionsPattern[]]:=Module[{keyIndex,toAssoc,associationTable,joinedSet},

keyIndex=OptionValue[KeyPosition];

If[MatchQ[data,{__Dataset}], (*if input data is Dataset objects, reduce to associations*)
		associationTable=Normal/@data;
		If[NumericQ[keyIndex],keyIndex=Keys[First@data[[1]]][[keyIndex]]]; (*when there is Header line, assign keyIndex to be the name of the variable*)
		joinedSet=Fold[JoinAcross[#1,#2,keyIndex,OptionValue[JoinMethod],KeyCollisionFunction->OptionValue[keyCollisionFunction]]&,associationTable];
, (* else assume it's a list of tables *)
	If[OptionValue@Header,
		If[NumericQ[keyIndex],keyIndex=data[[1,1]][[keyIndex]]]; 
		toAssoc[table_]:=With[{index=First@table},AssociationThread[index->#]&/@Rest[table]];
		associationTable=toAssoc/@data;
		joinedSet=Fold[JoinAcross[#1,#2,keyIndex,OptionValue[JoinMethod],KeyCollisionFunction->OptionValue[keyCollisionFunction]]&,associationTable];
	, (*else, when there is no Header line*)
		(*assign a unique index (as string) to each column in the format of table#-column#, except the column# of the KeyPosition, which will remain as it is*)
		toAssoc[table_,tableNumber_]:=With[
		{index=If[ArrayDepth@table>1,
			ReplacePart[ToString@tableNumber<>"-"<>ToString[#]&/@Range@Dimensions[table][[2]],keyIndex->ToString@keyIndex]
			, (*else*)
			ToString@keyIndex]},
		AssociationThread[index->#]&/@table];
		(*convert all datasets into associations*)
		associationTable=Table[toAssoc[data[[i]],i],{i,Length@data}];

		(*merge datasets from left to right, i.e. ...JoinAcross[JoinAcross[dataset1,dataset2],dataset3]...*)
		(*when using "Left" or "Right" as joining method, arrange datasets so that the leftmost or rightmost dataset has the master keys*)
		joinedSet=Fold[JoinAcross[#1,#2,ToString@keyIndex,OptionValue[JoinMethod],KeyCollisionFunction->OptionValue[keyCollisionFunction]]&,associationTable];
	];
];

(* returned output may be unsorted, can apply sort function afterwards*)
Return[joinedSet//Dataset]
];


(* ::Text:: *)
(*Fast dataset merging algorithm with inner joining and the first column as the common key. Input must be plain table (list of lists). *)


MergeData[list_?(MatchQ[#,{__List}]&),"Fast"]:=Flatten[{#,Rest/@{##2}}]&@@@(Pick[#,Length@#>Length@list-1]&/@GatherBy[Join@@list,First]);


(* ::Subsection::Closed:: *)
(*Running average*)


(* ::Text:: *)
(*Function starts with the first timestamp in the dataset, and finds the average of all data that falls within the time step (desiredTimeStep) interval from that timestamp, then jump to the next timestamp beyond this interval. Default assumption of the time step in the original dataset is 1 minute. *)


(*Options[RunningAverage]={originalTimeStep->1};
RunningAverage[data_,desiredTimeStep_,opt:OptionsPattern[]]:=Module[{t,timeStamp1,dataBin,dataOut,output},
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


RunningAverage[data_,opt:OptionsPattern[]]:=Module[{dataDimension,ts,timeStep,output},
output={};
dataDimension=Dimensions@data;
timeStep=OptionValue[ReportPeriod];

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
(*Summarize data shape*)


DataSummary[dataset_List]:=Module[{title=FirstCase[dataset,{__String}],min,max,mean,histograms,data,lx},
min[{}]="NaN";
min[x_List]:=Min@x;
max[{}]="NaN";
max[x_List]:=Max@x;
mean[{}]="NaN";
mean[x_List]:=Mean@x;
histograms={};

If[title=!=Missing["NotFound"],
	data=DeleteCases[dataset,{__String}];
	With[{x=Cases[#,_?NumericQ]&/@Transpose@data,l=Length/@Transpose@data},
		lx=Length/@x;
		Print[{Prepend[Range[Length[title]],""],Prepend[title,""],Flatten[{"min",min/@x}],Flatten[{"max",max/@x}],Flatten[{"mean",mean/@x}],Flatten[{"NaN",l-lx}],Flatten[{"NaN-%",(l-lx)/l*100//N}]}//TableForm];
		Do[If[Length@x[[i]]!=0,AppendTo[histograms,Histogram[x[[i]],PlotLabel->title[[i]]]]],{i,Length@x}];
	];
,(*else*)
	With[{x=Cases[#,_?NumericQ]&/@Transpose@dataset,l=Length/@Transpose@dataset},
		lx=Length/@x;
		Print[{Prepend[Range[Dimensions[dataset][[2]]],""],Flatten[{"min",min/@x}],Flatten[{"max",max/@x}],Flatten[{"mean",mean/@x}],Flatten[{"NaN",l-lx}],Flatten[{"NaN-%",(l-lx)/l*100//N}]}//TableForm];
		Do[If[Length@x[[i]]!=0,AppendTo[histograms,Histogram[x[[i]],PlotLabel->ToString@i]]],{i,Length@x}]
	];
];

Return[histograms];
];


DataSummary[dataset_Dataset]:=DataSummary@FromDataset[dataset,True];


(* ::Subsection::Closed:: *)
(*Import NASA data*)


(* ::Text:: *)
(*Get NASA POWER project data sets via API for a single location. *)
(*Default is to get TMY monthly values for solar irradiance (GHI), temperature, wind speed at 2m and precipitation. *)


GetNASAPowerData::notime="time range required but not specified.";

GetNASAPowerData[lat_,lon_,par_:"ALLSKY_SFC_SW_DWN,T2M,WS10M,PRECTOT",temporalType_:"CLIMATOLOGY",start_:Null,end_:Null]:=Module[{accessAPI,fileAddress,output},

accessAPI="https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?request=execute&identifier=SinglePoint&userCommunity=SSE&user=anonymous&outputList=CSV&lat="<>ToString@lat<>"&lon="<>ToString@lon<>"&tempAverage="<>temporalType<>"&parameters="<>par;

If[temporalType!="CLIMATOLOGY",
	If[start===Null||end===Null,
		Message[GetNASAPowerData::notime];
		Abort[];
	,
		accessAPI=accessAPI<>"&startDate="<>start<>"&endDate="<>end;
	];
];

fileAddress=Association[Import[accessAPI,"JSON"]]["outputs"]//Values;
output=Import@First@fileAddress;

Return[Cases[{output},{___,{"-END HEADER-"},x___}->x]];

];


(* ::Subsection:: *)
(*Pass data to Python*)


(* ::Section::Closed:: *)
(*Quick ReportPeriod statistics*)


(* ::Text:: *)
(*Usually from monthly data. *)
(*Data should have the format {{time, var1, var2, ...}, ...}. *)
(*minimum data points are set assuming minute resolution data. *)


PeriodSum[data_List,opt:OptionsPattern[]]:=Module[{minPts,groupedData,periodResults,periodSet,periodSum},
minPts=OptionValue[MinDataPts];

groupedData=Switch[OptionValue[ReportPeriod],
	"Month",
		GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
	"Day",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
	"Hour",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
	"10 Minutes",
		GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]
]; 
(* timestamp represents the start of a time step *)

periodResults={};
(* correct the minimum data points required for each time step if user setting is not meaningful *)
If[OptionValue[ReportPeriod]=="10 Minutes"&&minPts>10,minPts=1;]; 

Do[
	periodSet=groupedData[timestep];

	If[Length[periodSet]<=minPts,
		Print[DateObject[timestep]," missing or has too little data points"]
	,
		(*periodSum=Total[periodSet[[All,2;;-1]]];*)
		periodSum=fnSum[periodSet[[All,2;;-1]],Dimensions[periodSet][[2]]-1];
		AppendTo[periodResults,Flatten[{DateObject[timestep],periodSum*OptionValue@Scaling}]];
	];
,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];

PeriodSum[data_Dataset,opt:OptionsPattern[]]:=ToDataset[PeriodSum[FromDataset@data,opt],DatasetColumns@data];


PeriodSpan[data_List,opt:OptionsPattern[]]:=Module[{minPts,groupedData,periodResults,periodSet,periodSpan},
minPts=OptionValue[MinDataPts];

groupedData=Switch[OptionValue[ReportPeriod],
	"Month",
		GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
	"Day",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
	"Hour",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
	"10 Minutes",
		GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]
];

periodResults={};
If[OptionValue[ReportPeriod]=="10 Minutes",minPts=1;];

Do[
periodSet=groupedData[timestep];

If[Length[periodSet]<=minPts,
	Print[DateObject[timestep]," missing or has too little data points"]
,
	periodSpan=periodSet[[-1,2;;-1]]-periodSet[[1,2;;-1]];
	AppendTo[periodResults,Flatten[{DateObject[timestep],periodSpan}]];];
,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];

PeriodSpan[data_Dataset,opt:OptionsPattern[]]:=ToDataset[PeriodSpan[FromDataset@data,opt],DatasetColumns@data];


PeriodAverage[data_List,opt:OptionsPattern[]]:=Module[{minPts,groupedData,periodResults,periodSet,periodAverage},
minPts=OptionValue[MinDataPts];

groupedData=Switch[OptionValue[ReportPeriod],
	"Month",
		GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
	"Day",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
	"Hour",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
	"10 Minutes",
		GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]
];

periodResults={};
If[OptionValue[ReportPeriod]=="10 Minutes",minPts=1;];

Do[
periodSet=groupedData[timestep];

If[Length[periodSet]<=minPts,
	Print[DateObject[timestep]," missing or has too little data points"]
,
	periodAverage=fnAvg[periodSet[[All,2;;-1]],Dimensions[periodSet][[2]]-1];
	AppendTo[periodResults,Flatten[{DateObject[timestep],periodAverage}]];
];
,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];

PeriodAverage[data_Dataset,opt:OptionsPattern[]]:=ToDataset[PeriodAverage[FromDataset@data,opt],DatasetColumns@data];


PeriodMaster[data_List,function:_String:"fnAvg",opt:OptionsPattern[]]:=Module[{minPts,groupedData,periodResults,periodSet,periodOut},
minPts=OptionValue[MinDataPts];

groupedData=Switch[OptionValue[ReportPeriod],
	"Month",
		GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
	"Day",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
	"Hour",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
	"10 Minutes",
		GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]
];

periodResults={};
If[OptionValue[ReportPeriod]=="Hour",minPts=5;];
If[OptionValue[ReportPeriod]=="10 Minutes",minPts=1;];

Do[
periodSet=groupedData[timestep];

If[Length[periodSet]<=minPts,
	Print[DateObject[timestep]," missing or has too little data points"]
,
	periodOut=ToExpression[function][periodSet[[All,2;;-1]],Dimensions[periodSet][[2]]-1];
	AppendTo[periodResults,Flatten[{DateObject[timestep],periodOut}]];
];
,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];

PeriodMaster[data_Dataset,function:_String:"fnAvg",opt:OptionsPattern[]]:=ToDataset[PeriodMaster[FromDataset@data,function,opt],DatasetColumns@data];


PeriodWeightedAvg[data_List,weight_List,opt:OptionsPattern[]]:=Module[{minPts,groupedData,groupedWgtData,periodResults,periodSet,periodWgtSet,periodAverage},
minPts=OptionValue[MinDataPts];

groupedData=Switch[OptionValue[ReportPeriod],
	"Month",
		GroupBy[data,DateValue[#[[1]],{"Year","Month"}]&],
	"Day",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day"}]&],
	"Hour",
		GroupBy[data,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
	"10 Minutes",
		GroupBy[data,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]
];

groupedWgtData=Switch[OptionValue[ReportPeriod],
	"Month",
		GroupBy[weight,DateValue[#[[1]],{"Year","Month"}]&],
	"Day",
		GroupBy[weight,DateValue[#[[1]],{"Year","Month","Day"}]&],
	"Hour",
		GroupBy[weight,DateValue[#[[1]],{"Year","Month","Day","Hour"}]&],
	"10 Minutes",
		GroupBy[weight,MapAt[Floor[#/10]*10&,DateValue[#[[1]],{"Year","Month","Day","Hour","Minute"}],-1]&]
];

periodResults={};
If[OptionValue[ReportPeriod]=="10 Minutes",minPts=1;];

Do[
periodSet=groupedData[timestep];
periodWgtSet=groupedWgtData[timestep];

If[Length[periodSet]<=minPts,
	Print[DateObject[timestep]," missing or has too little data points"]
,	
	If[Length[periodSet]!=Length[periodWgtSet],
		Print[DateObject[timestep]," does not have matching data and weights"];
		Abort[];
	,
		periodAverage=Total[#*periodWgtSet[[All,2]]]/Total[periodWgtSet[[All,2]]]&/@Table[periodSet[[All,i]],{i,2,Dimensions[periodSet][[2]]}];
		AppendTo[periodResults,Flatten[{DateObject[timestep],periodAverage}]];
	];
];

,
{timestep,Keys[groupedData]}];

Return[SortBy[periodResults,First]]
];


(* ::Text:: *)
(*More generic treatment using built-in functionality of TimeSeries in function PeriodStats: *)
(**)
(*Start and end time should be DateObjects. They can also be specified in DateList format of a DateObject, but need to ensure the output of DateList is in local time zone of evaluation (computer system time zone). *)
(*Weighted average calculation is triggered by specifying option weightPosition (the value in option function will be overwritten),*)
(*	needs dataset to contain weighting data, *)
(*	weight position should be specified as: column_number_of_the_weight - 1 (minus timestamp column) ,*)
(*	weighted average for the weighting data should not be used.*)
(*No minimum data point requirement is defined, results will not be Missing as long as there is data. *)
(*Make sure timestamp is contained in the first column and preferably in DateObject format to avoid ambiguous interpretation when converting to TimeSeries object. *)
(*PeriodStats conserves true time but may not output the DateObject in the original time zone unless the correct time zone is specified (to avoid error of not supported by TimeSeries functions, numerical offset will always be used internally for timezone specifications). *)
(*PeriodStats reorders unsorted data into one sorted by timestamp. *)


PeriodStats[data_,start_:Automatic,end_:Automatic,opt:OptionsPattern[]]:=Block[{$TimeZone=TimeZoneOffset@OptionValue["TimeZone"],fn,tstep,wdAlignment,padding,ts=Null,startTime,endTime,dataDimension,wgtPos,output="No result"},
fn=OptionValue[function];
tstep=OptionValue[TimeStep];
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

If[start===Automatic,startTime=ts["FirstDate"];,startTime=start;];
If[end===Automatic,endTime=ts["LastDate"];,endTime=end;];
	
dataDimension=ts["ValueDimensions"];
If[wgtPos=!=Null,
	If[1<=wgtPos<=dataDimension,
		output=If[dataDimension>1,
			MovingMap[fnWgtAvg[#,dataDimension,#[[All,wgtPos]]]&,ts,{tstep,wdAlignment,{startTime,endTime,tstep}},padding]
		,(*else, when dataset does not contain valid weighting data, simply apply averaging without weighting*)
			Print@"warning: input dataset does not contain weighting data, simple averaging is applied";
			MovingMap[fnAvg[#,dataDimension]&,ts,{tstep,wdAlignment,{startTime,endTime,tstep}},padding]
		];
	,
		Print@"warning: weighting data incorrectly defined, simple averaging is applied";
		output=MovingMap[fnAvg[#,dataDimension]&,ts,{tstep,wdAlignment,{startTime,endTime,tstep}},padding];
	];
,(*else*)
	output=MovingMap[fn[#,dataDimension]&,ts,{tstep,wdAlignment,{startTime,endTime,tstep}},padding];
];

Return[
Which[
	Head@data===TemporalData,output,
	Head@data===Dataset,ToDataset[Flatten/@output["DatePath"],Normal@Keys@First@data],
	Head@data===List,output["DatePath"]]]
];


(* ::Text:: *)
(*Supporting functions to Period Statistics: *)


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
	If[d>1,output=Table[Missing["NoValidData"],d],output=Missing["NoValidData"]]
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



(* ::Section::Closed:: *)
(*Plotting related*)


(* ::Subsection::Closed:: *)
(*Customized plot types*)


TwoAxisPlot[{f_,g_},{x_,x1_,x2_}]:=Module[{fgraph,ggraph,frange,grange,fticks,gticks},{fgraph,ggraph}=MapIndexed[Plot[#,{x,x1,x2},Axes->True,PlotStyle->ColorData[1][#2[[1]]]]&,{f,g}];{frange,grange}=(PlotRange/.AbsoluteOptions[#,PlotRange])[[2]]&/@{fgraph,ggraph};fticks=N@FindDivisions[frange,5];
gticks=Quiet@Transpose@{fticks,ToString[NumberForm[#,2],StandardForm]&/@Rescale[fticks,frange,grange]};
Show[fgraph,ggraph/.Graphics[graph_,s___]:>Graphics[GeometricTransformation[graph,RescalingTransform[{{0,1},grange},{{0,1},frange}]],s],Axes->False,Frame->True,FrameStyle->{ColorData[1]/@{1,2},{Automatic,Automatic}},FrameTicks->{{fticks,gticks},{Automatic,Automatic}}]];



TwoAxisListPlot[{f_,g_}]:=Module[{fgraph,ggraph,frange,grange,fticks,gticks},{fgraph,ggraph}=MapIndexed[ListPlot[#,Axes->True,PlotStyle->ColorData[1][#2[[1]]]]&,{f,g}];{frange,grange}=Last[PlotRange/.AbsoluteOptions[#,PlotRange]]&/@{fgraph,ggraph};
fticks=Last[Ticks/.AbsoluteOptions[fgraph,Ticks]]/._RGBColor|_GrayLevel|_Hue:>ColorData[1][1];
gticks=(MapAt[Function[r,Rescale[r,grange,frange]],#,{1}]&/@Last[Ticks/.AbsoluteOptions[ggraph,Ticks]])/._RGBColor|_GrayLevel|_Hue->ColorData[1][2];
Show[fgraph,ggraph/.Graphics[graph_,s___]:>Graphics[GeometricTransformation[graph,RescalingTransform[{{0,1},grange},{{0,1},frange}]],s],Axes->False,Frame->True,FrameStyle->{ColorData[1]/@{1,2},{Automatic,Automatic}},FrameTicks->{{fticks,gticks},{Automatic,Automatic}}]];

(*variant*)
(*TwoAxisListPlot[{list1_,list2_},opts:OptionsPattern[]]:=Module[{plot1,plot2,ranges},{plot1,plot2}=ListLinePlot/@{list1,list2};
ranges=Last@Charting`get2DPlotRange@#&/@{plot1,plot2};
ListPlot[{list1,Rescale[list2,Last@ranges,First@ranges]},Frame\[Rule]True,FrameTicks\[Rule]{{Automatic,Charting`FindTicks[First@ranges,Last@ranges]},{Automatic,Automatic}},FrameStyle\[Rule]{{Automatic,ColorData[97][2]},{Automatic,Automatic}},FilterRules[{opts},Options[ListPlot]]]]*)

TwoAxisListLinePlot[{f_,g_}]:=Module[{fgraph,ggraph,frange,grange,fticks,gticks},{fgraph,ggraph}=MapIndexed[ListLinePlot[#,Axes->True,PlotStyle->ColorData[1][#2[[1]]]]&,{f,g}];{frange,grange}=Last[PlotRange/.AbsoluteOptions[#,PlotRange]]&/@{fgraph,ggraph};
fticks=Last[Ticks/.AbsoluteOptions[fgraph,Ticks]]/._RGBColor|_GrayLevel|_Hue:>ColorData[1][1];
gticks=(MapAt[Function[r,Rescale[r,grange,frange]],#,{1}]&/@Last[Ticks/.AbsoluteOptions[ggraph,Ticks]])/._RGBColor|_GrayLevel|_Hue->ColorData[1][2];
Show[fgraph,ggraph/.Graphics[graph_,s___]:>Graphics[GeometricTransformation[graph,RescalingTransform[{{0,1},grange},{{0,1},frange}]],s],Axes->False,Frame->True,FrameStyle->{ColorData[1]/@{1,2},{Automatic,Automatic}},FrameTicks->{{fticks,gticks},{Automatic,Automatic}}]];


(* ::Text:: *)
(*(under construction)*)


ListDatePlot[data_,options_:Options[ListPlot]]:=Module[{absTime},

If[ArrayDepth@data==3,
	absTime=Map[MapAt[AbsoluteTime,#,1]&,#,{2}]&;
,
	absTime=Map[MapAt[AbsoluteTime,#,1]&,#]&;
];

];


(* ::Subsection::Closed:: *)
(*Auxiliary*)


(* ::Text:: *)
(*Labels the selected columns (specified by positions) in a certain color. *)
(*Logic should be specified as a function or pure function that applies to the entire row. *)
(*Positions should be specified in a format compatible with position specification in MapAt. By default, will highlight column 2 to end. *)
(*Color can be specified in the form of pure function. *)


HighlightData[data_,logic_,positions_:"default",color_:Red]:=Module[{highlightPos,output},

Which[ArrayDepth@data==2,
If[positions==="default",highlightPos=Rest[List/@Range@Last@Dimensions@data];,highlightPos=positions;];

output=If[Check[logic@#,False],With[{color2=If[Head@color===Function,color@#,color]},MapAt[Style[#,color2]&,#,highlightPos]],#]&/@data;
,
ArrayDepth@data==1,
output=If[Check[logic@#,False],Style[#,color],#]&/@data;
];

Return@output;
];


(* ::Subsection::Closed:: *)
(*Speedy post-processing of plots*)


EnhancedShow[plot_,opt:OptionsPattern[]]:=Show[plot,
Frame->True,
Axes->False,
LabelStyle->{FontFamily->"Helvetica",FontSize->22,FontWeight->Bold,FontColor->Black},
ImageSize->600,ImageMargins->15,opt];


ExtractPlotData[plot_]:=Module[{points},
points=Cases[plot,x_Point:>First@x,Infinity];
If[Length[points]==0,
points=Cases[plot,x_Line:>First@x,Infinity];];
If[Length[points]==0,points=$Failed;,points=points//First];

Return[points]
];


AddTrendline[plot_,opt:OptionsPattern[]]:=Module[{data,fit},
data=Cases[plot,x_Point:>First@x,Infinity]//First;
fit=Fit[data,{1,x},x];

Show[plot,Plot[fit,{x,Min[data[[All,1]]],Max[data[[All,1]]]},PlotStyle->Red],Epilog->If[OptionValue["ShowEquation"],Text[Style[fit,"Helvetica",Bold,15],OptionValue["PlaceEquation"],{-1,0}]]]
];


FigureAlbum[figures_List]:=With[{lables=Table[First@Values@AbsoluteOptions[figures[[j]],PlotLabel]/.None->j,{j,Length@figures}]},
	Manipulate[AssociationThread[lables->figures][i],{i,lables}]
];


(* ::Chapter:: *)
(*Solar geometry and meteorological*)


(* ::Subsection::Closed:: *)
(*Daytime duration*)


(* ::Text:: *)
(*Input is Julian day number from start of the year, and latitude is in degrees (value from 89 to -89, positive being northern hemisphere). *)


DayLength[j_,lat_]:=Module[{\[Theta],\[Phi],dayLength},
\[Theta]=0.2163108+2*ArcTan[0.9671396*Tan[0.00860*(j-186)]];
\[Phi]=ArcSin[0.39795*Cos[\[Theta]]];
dayLength=24-24/Pi*ArcCos[(Sin[0.8333*Pi/180]+Sin[lat*Pi/180]*Sin[\[Phi]])/(Cos[lat*Pi/180]*Cos[\[Phi]])];
(*there is no real solution when it's polar day or polar night*)
Return[Re@dayLength];
];


(* ::Subsection::Closed:: *)
(*Night time data removal*)


(* ::Text:: *)
(*Works with tables or datasets with first column as the timestamps. *)
(*Automatically converts timestamp to DateObject if the timestamp is in string format, DateFormat specifies the format. *)
(*Make sure timestamp of input data is in local timezone of the site but not of the local computer doing the evaluation. *)
(*As all timestamps will be converted to DateList which preserves the form but not the true time, time zone of the input timestamps must be specified explicitly or by location (note that local time zone for a location may sometimes not be the intended time zone in the input data timestamps due to daylight saving time confusion, e.g. LocalTimeZone for Denver is MDT but timestamps in Denver are also commonly specified in MST). If time zone is not explicitly specified, local time zone of the location is assumed. If input data timestamp is in string format, it must be denoted in the intended and specified timezone. If input data timestamp is a DateObject, it can be denoted in any time zone, true time will be preserved. However, it is still advisable to specify target timezone as the local time zone of the intended location (at least not to far from it), otherwise may confuse and get Sunrise or Sunset time in the wrong day (in case where input data timestamp is on the other half of the globe as the intended location of which sunrise and sunset time is determined). *)


RemoveNightTime[dataIn_,opt:OptionsPattern[]]:=Module[{location=OptionValue[Location],timezone=OptionValue["TimeZone"],dateFormat,data,convertTime,groupedData,sunrise,sunset,dailySet,output},
If[timezone===Null,timezone=LocalTimeZone@location;]; (* this is the working time zone of all the timestamps in the input and output dataset and within this function *)
dateFormat=OptionValue["DateFormat"];

data=If[Head@dataIn===Dataset,Normal@Values@dataIn,dataIn];

(*convert timestamps to DateLists, and group the dataset by days*)
convertTime=MapAt[
	If[Head@#===String,
		Check[DateList@{#,dateFormat},Check[DateList@#,{0,0,0}]] (* if dateFormat doesn't work, try directly applying DateList, if still doesn't work, discard that timestamp; no need timezone info as only form is preserved *)
		,
		DateList[#,TimeZone->timezone]]&
	,1];
groupedData=GroupBy[convertTime/@data,First[#][[1;;3]]&];

output={};

Do[
sunrise=Sunrise[location,DateObject[day,TimeZone->timezone]]//DateObject[#,"Instant",TimeZone->timezone]&; 
(*function Sunrise and Sunset gives DateObject with "Minute" granularity after version 12, need to convert to "Instant" in order to compare with timestamps in the dataset*)
sunset=Sunset[location,DateObject[day,TimeZone->timezone]]//DateObject[#,"Instant",TimeZone->timezone]&;
dailySet=MapAt[DateObject[#,"Instant",TimeZone->timezone]&,1]/@groupedData[day];
AppendTo[output,Select[dailySet,sunrise<First@#<sunset&]]
,
{day,DeleteCases[Keys@groupedData,{0,0,0}]}];

Return@
If[Head@dataIn===Dataset,
	ToDataset[Flatten[output,1],Normal@Keys@First@dataIn]
,
	Flatten[output,1]
]

];


(* ::Subsection::Closed:: *)
(*Array configuration calculations*)


(* ::Text:: *)
(*Inter-row pitch & profile angle (shading limit angle) & GCR*)


(* ::Text:: *)
(*tilt and \[Theta]limit should be in degrees. *)


ArrayPitch[tilt_,width_,\[Theta]limit_]:=width*Cos[tilt \[Degree]]+width*Sin[tilt \[Degree]]/Tan[\[Theta]limit \[Degree]];


ShadeLimitAngle[tilt_,width_,pitch_]:=N[ArcTan[width*Sin[tilt \[Degree]]/(pitch-width*Cos[tilt \[Degree]])]/Degree];


(* ::Text:: *)
(*Calculate resulting GCR for a certain tilt and desired shading limit angle. *)
(*GCR is also equal to collector width / pitch. *)


GCR[tilt_,\[Theta]limit_]:=1/(Cos[tilt \[Degree]]+Sin[tilt \[Degree]]/Tan[\[Theta]limit \[Degree]]);


(* ::Subsection::Closed:: *)
(*Angle of incidence*)


(* ::Text:: *)
(*projection (dot product) = cos(surface_tilt) * cos(solar_zenith) +  sin(surface_tilt) * sin(solar_zenith) * cos(solar_azimuth - surface_azimuth). *)
(*Note that sunZenith is used instead of sun height. *)
(*Array orientation and sun azimuth can be measured with reference to North or South, as long as it is consistent. *)
(*AOI > 90 can be encountered, meaning sunlight comes from back of the module. *)


AoiProjection[tilt_,orientation_,sunZenith_,sunAzimuth_]:=Module[{planeNormal,sunLight,projection},
planeNormal={Sin[tilt \[Degree]]*Sin[orientation \[Degree]],Sin[tilt \[Degree]]*Cos[orientation \[Degree]],Cos[tilt \[Degree]]}; (* unit vector for plane normal *)
sunLight={Sin[sunZenith \[Degree]]*Sin[sunAzimuth \[Degree]],Sin[sunZenith \[Degree]]*Cos[sunAzimuth \[Degree]],Cos[sunZenith \[Degree]]}; (* unit vector for sun light *)

projection=planeNormal.sunLight; (* dot product of two unit vectors *)

Return@projection;
];


AngleOfIncidence[tilt_,orientation_,sunZenith_,sunAzimuth_]:=ArcCos@AoiProjection[tilt,orientation,sunZenith,sunAzimuth]/Degree//N;


(* ::Chapter:: *)
(*PV system related calculations*)


(* ::Section:: *)
(*Misc KPIs & General*)


(* ::Subsection::Closed:: *)
(*Performance ratio*)


(* ::Text:: *)
(*For temperature corrected or weather corrected PR (Ref NREL report: weather corrected performance ratio, Timothy Dierauf et al. ), temperature coefficient (sTC) and average temperature (weighted with in-plane irradiance) throughout the ReportPeriod (usually a year), avgT, are needed. *)
(*sTC in the unit of /K (usually negative).  *)
(*avgT and sT should be same unit, in \[Degree]C. *)
(**)
(*Weather corrected PR is most meaningful when monitoring is done for a limited period of time but it is desired to infer the annual PR. *)


PR[power_,ratedPower_,irradiance_?(NonPositive@# || !NumericQ@#&)]:="NA";
PR[power_,ratedPower_,irradiance_?Positive]:=power/(ratedPower*irradiance/1000)//N;

PRcorrT[power_,ratedPower_,irradiance_?Positive,sT_,sTC_]:=power/(ratedPower*irradiance/1000*(1-sTC*(25-sT)))//N;
PRcorrT[power_,ratedPower_,irradiance_?NonPositive,sT_,sTC_]:="NA";

(*make sure power, irradiance, sT are lists of same length, same timestep*)
PRcorrW[power_,ratedPower_,irradiance_?Positive,sT_,sTC_,avgT_]:=Total[power]/Total[ratedPower*irradiance/1000*(1-sTC*(avgT-sT))]//N;
PRcorrW[power_,ratedPower_,irradiance_?NonPositive,sT_,sTC_,avgT_]:="NA";



(* ::Text:: *)
(*Calculate PR and append as a column for a dataset. *)


CalcPR[table_Dataset/;ArrayDepth@table==1,powerCol_,ratedPower_?NumericQ,irrCol_]:=table[All,<|#,"PR"->PR[#[[powerCol]],ratedPower,#[[irrCol]]]|>&];
CalcPR[table_List/;ArrayDepth@table==2,powerCol_Integer/;powerCol>0,ratedPower_?NumericQ,irrCol_Integer/;irrCol>0]:=Append[#,PR[#[[powerCol]],ratedPower,#[[irrCol]]]]&/@table;


CalcPRcorrT[table_Dataset/;ArrayDepth@table==1,powerCol_,ratedPower_?NumericQ,irrCol_,sTCol_,sTC_]:=
table[All,<|#,"PR_Tcorr"->PRcorrT[#[[powerCol]],ratedPower,#[[irrCol]],#[[sTCol]],sTC]|>&];
CalcPRcorrT[table_List/;ArrayDepth@table==2,powerCol_Integer/;powerCol>0,ratedPower_?NumericQ,irrCol_Integer/;irrCol>0,sTCol_Integer/;sTCol>0,sTC_]:=
Append[#,PRcorrT[#[[powerCol]],ratedPower,#[[irrCol]],#[[sTCol]],sTC]]&/@table;


CalcPRcorrW[table_Dataset/;ArrayDepth@table==1,powerCol_,ratedPower_?NumericQ,irrCol_,sTCol_,sTC_,avgT_]:=
table[All,<|#,"PR_Wcorr"->PRcorrW[#[[powerCol]],ratedPower,#[[irrCol]],#[[sTCol]],sTC,avgT]|>&];
CalcPRcorrW[table_List/;ArrayDepth@table==2,powerCol_Integer/;powerCol>0,ratedPower_?NumericQ,irrCol_Integer/;irrCol>0,sTCol_Integer/;sTCol>0,sTC_,avgT_]:=
Append[#,PRcorrW[#[[powerCol]],ratedPower,#[[irrCol]],#[[sTCol]],sTC,avgT]]&/@table;


(* ::Subsection::Closed:: *)
(*Current & voltage*)


(* ::Text:: *)
(*VoltageRatio returns a simple ratio to Voc at STC, and a ratio to expected Voc under a certain operating condition. *)
(*Dependence of Voc on irradiance is based on the following equation (assuming a simple one-diode model): *)
(*Voc = (n Ns kT) / (q ln(Isc/I0)) *)
(*= (n Ns kT) / (q ln(Isc,stc / I0 * G/1000)) *)
(*=  Voc,T + (n Ns kT) / (q ln(G/1000))*)
(*Ns is number of cells (not modules) in series in a string, which is important to specify in order to correctly estimate irradiance dependence. *)
(*n is the ideality factor in the diode model of the solar cell. *)
(*Temperature coefficient is for Voc. *)


VoltageRatio[V_,Voc_,Gpoa_?Positive,Tmod_,Ns_,tempCoeff_:-0.003,n_:1]:={V/Voc,V/(Voc+Voc*tempCoeff*(Tmod-298.15)+k*Tmod*Ns*n/q*Log[Gpoa/1000])};
VoltageRatio[V_,Voc_,Gpoa_?NonPositive,Tmod_,Ns_,tempCoeff_:-0.003,n_:1]:={0,0};


VoltageRatio2[V_,Voc_,Gpoa_?Positive,Tmod_,tempCoeff_:-0.003]:={V/Voc,V/(Voc+Voc*tempCoeff*(Tmod-298.15))}; (* simplified without irradiance correction *)
VoltageRatio2[V_,Voc_,Gpoa_?NonPositive,Tmod_,tempCoeff_:-0.003]:={0,0};


(* ::Text:: *)
(*CurrentRatio returns a simple ratio to Isc, and a ratio to expected Isc under a certain operating condition. *)


CurrentRatio[current_,Isc_,Gpoa_?Positive,Tmod_,tempCoeff_:0.0005]:={current/Isc,current/(Gpoa/1000*(Isc+Isc*tempCoeff*(Tmod-298.15)))};
CurrentRatio[current_,Isc_,Gpoa_?NonPositive,Tmod_,tempCoeff_:0.0005]:={0,0};


CurrentRatio[current_,Isc_,Gpoa_?Positive]:={current/Isc,current/(Gpoa/1000*Isc)}; (* simplified without temperature correction *)
CurrentRatio[current_,Isc_,Gpoa_?NonPositive]:={0,0};


(* ::Subsection::Closed:: *)
(*Quick conversion*)


(* factor to multiply for converting units *)
ConversionFactor["J->kWh"]=1/3600000;
ConversionFactor["\:4e07\:5343\:74e6->MW"]=10;
ConversionFactor["\:4e07\:5343\:74e6->GW"]=0.01;

(* factor to multiply from sum of average power readings to energy *)
ConversionFactor["Sum W/min->kWh"]=1/60000;
ConversionFactor["Sum W/5min->kWh"]=1/12000;
ConversionFactor["Sum W/10min->kWh"]=1/6000;
ConversionFactor["Sum W/15min->kWh"]=1/4000;
ConversionFactor["Sum W/hour->kWh"]=1/1000;


ConversionFactor["lookup"]=Column@{"J->kWh","\:4e07\:5343\:74e6->MW","\:4e07\:5343\:74e6->GW","Sum W/min->kWh","Sum W/5min->kWh","Sum W/10min->kWh","Sum W/15min->kWh","Sum W/hour->kWh"};
ConversionFactor@_:=Block[{},Print@"no such conversion! Default value of 1 will be used.";1];


(* ::Section::Closed:: *)
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


CablingLoss[current_,cableLength_,crossSection_,opt:OptionsPattern[]]:=Module[{loss,sR,b,phi,a},
a=OptionValue["NumLineCoeff"];
b=OptionValue["LengthCableFactor"];
phi=OptionValue["Resistivity"];

If[current>0,
sR=b*phi*cableLength/crossSection;
loss=a*sR*current^2;
,
loss=0;
];

Return[loss]
];


(* ::Section::Closed:: *)
(*Thermal models*)


(* ::Text:: *)
(*Temperature coefficient tCoeff is for Pmpp, should be around negative and in relative percentage of STC, around -0.003 ~ -0.004. *)
(*module efficiency is around 0.15~0.25 for Si technologies. *)
(*Air temperature should be specified in \[Degree]C. *)


ModuleTemperature[airT_,irrLv_,tCoeff_:-0.003,\[Eta]0_:0.2,uL_:29]:=Module[{\[Eta]c,Tmod,t,\[Tau],\[Alpha],TmodEst},
\[Tau]=0.95; (* transmittance of glazing *)
\[Alpha]=0.8; (* fraction of solar spectrum absorbed *)

\[Eta]c[T_]:=\[Eta]0+tCoeff*(T-airT)*\[Eta]0;
TmodEst=airT+irrLv*((\[Tau]*\[Alpha])/uL)*(1-\[Eta]c[airT+10]/(\[Tau]*\[Alpha]));
Tmod=Quiet@Check[t/.(FindRoot[t==airT+irrLv*((\[Tau]*\[Alpha])/uL)*(1-\[Eta]c[t]/(\[Tau]*\[Alpha])),{t,TmodEst}]),TmodEst]; 
(* if cannot find the root, simply return Tmod_Estimate = Tamb+k*G *)
Return[Tmod]
];


(* ::Section::Closed:: *)
(*Simple fault detection*)


(* ::Text:: *)
(*Each input lists should be in the format of {timestamp, values}. *)
(*Faults are only considered for irradiance levels greater than 100 W/m2. *)
(*Currently two types of faults:*)
(*low current (likely open circuit, MPPT error, inverter protection on);*)
(*disconnected (no current AND voltage);*)


SimpleFaultDetect[listGVIP_,listPR_,listIratio_,listVratio_]:=Module[{combined,faultData},
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


(* ::Section::Closed:: *)
(*Time series inspection*)


(* ::Text:: *)
(*Note: *)
(*Make sure column names are properly defined and correspond to data. *)
(*Make sure the unit for power in the input data is W, which indicate average power in a time step. Input temporal resolution should be specified in number of minutes (default is 1 minute). Reporting unit will be in KWh, which indicate energy in a certain ReportPeriod (default is 1 day, alternative can be 1 month, 1 hour, 10 minutes). *)
(*Each timestamp indicate the start of a time interval. *)
(*Make sure nominal power and irradiance (insolation) is in the same unit as Pdc and Pac. *)
(*Make sure all irradiance, voltage, current and power are positive in value. *)
(**)
(*Insolation is calculated whenever there is valid irradiance reading, valid range of is 0-2000W/m^2. *)
(*Yield is calculated whenever there is valid power reading. *)
(*PR is calculated only when there is valid irradiance reading (values between 20-2000W/m^2) AND power reading. *)
(*PR for DC and AC side is calculated separately, so discrepancy may exist. *)
(**)
(*Full choice of column names is: ColumnNames->{"Timestamp","G","Vdc","Idc","Pdc","Vac","Iac","Pac","cum_meter_reading", "Tmod"}. (Tmod still not implemented yet)*)


TimeSeriesInspection[data_List?(ArrayDepth@#==2&),columns:{___String}:{"Timestamp","G","Vdc","Idc","Pdc","Vac","Iac","Pac"},opt:OptionsPattern[]]:=Module[{nominalP,resolution$input,resolution$output,irradianceThreshold,colIndex,powerDC=Null,integrateYieldDC=Null,specificYieldDC=Null,powerAC=Null,integrateYieldAC=Null,specificYieldAC=Null,insolation=Null,PRdc=Null,PRac=Null,PRdcTcorr=Null,PRacTcorr=Null,cumEnergy=Null,\[Eta]inverter=Null,plotOptions,outputData=<||>,plots=<||>},
nominalP=OptionValue[NominalPower];
resolution$input=OptionValue[InputResolution];
resolution$output=OptionValue[ReportPeriod];
irradianceThreshold={20,1600};
plotOptions={Mesh->Full,ImageSize->Large,GridLines->Automatic,LabelStyle->{FontFamily->"Helvetica",FontSize->14,FontWeight->Bold,FontColor->Black}};

If[Length[columns]!=Dimensions[data][[2]],
	Print["error: columns are not properly defined"];
	Abort[];
];

colIndex=AssociationThread[columns->Range[Length[columns]]];

(* ----------------- DC yield and PR --------------------------- *)
If[MemberQ[columns,"Vdc"]&&MemberQ[columns,"Idc"],

If[MemberQ[columns,"Pdc"],

	(*test if Vdc Idc and Pdc are consistent*)
	If[Abs@Mean@With[{x=Select[data[[All,{colIndex["Vdc"],colIndex["Idc"],colIndex["Pdc"]}]],Last@#>0&]},
	(Times@@@x[[All,{1,2}]]-x[[All,{3}]])/x[[All,{3}]]]>0.05,
		(*return warning if two values deviate more than 5%-rel*)
		Print["warning: discrepancy between DC voltage current product and power"]; 
	];

	(* calculate yield with power or V and I*)
	powerDC={First@#,If[NumericQ@#[[4]]&&NonNegative@#[[4]],#[[4]],If[NumericQ@#[[2]]&&NumericQ@#[[3]]&&#[[2]]>0&&#[[3]]>0,#[[2]]*#[[3]],0]]}&/@data[[All,{colIndex["Timestamp"],colIndex["Vdc"],colIndex["Idc"],colIndex["Pdc"]}]];
	integrateYieldDC={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[powerDC,MinDataPts->1,ReportPeriod->resolution$output];

	, (*else, calculate yield with voltage and current*)

	powerDC={First@#,If[#[[2]]>0&&#[[3]]>0,#[[2]]*#[[3]],0]}&/@Cases[data[[All,{colIndex["Timestamp"],colIndex["Vdc"],colIndex["Idc"]}]],{_,_?NumericQ,_?NumericQ}];
	integrateYieldDC={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[powerDC,MinDataPts->1,ReportPeriod->resolution$output];

];

If[NumberQ[nominalP],
	(* calculate specific yield *)
	specificYieldDC={First@#,#[[2]]/nominalP*1000}&/@integrateYieldDC;

	(* calculate insolation and PR *)
	If[MemberQ[columns,"G"],
		insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
		
		(* calculate with data points either with valid power readings or valid voltage-current readings *)
		PRdc={First@#,#[[3]]/nominalP/#[[2]]*1000}&/@PeriodSum[
		{#[[1]],#[[2]],If[NumericQ@#[[5]]&&NonNegative@#[[5]],#[[5]],#[[3]]*#[[4]]]}&/@Select[data[[All,{colIndex["Timestamp"],colIndex["G"],colIndex["Vdc"],colIndex["Idc"],colIndex["Pdc"]}]],irradianceThreshold[[1]]<#[[2]]<irradianceThreshold[[2]]&&(NumericQ@#[[3]]&&NumericQ@#[[4]]&&#[[3]]>=0&&#[[4]]>=0||NumericQ@#[[5]]&&NonNegative@#[[5]])&]
		,MinDataPts->1,ReportPeriod->resolution$output];
	];
,
	If[MemberQ[columns,"G"],
		insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
	];
];

, (*else, only Pdc is present*)

If[MemberQ[columns,"Pdc"],
(*non valid readings like "NA" or negative values are removed*)
	powerDC=Select[data[[All,{colIndex["Timestamp"],colIndex["Pdc"]}]],NumberQ[#[[2]]]&&NonNegative[#[[2]]]&];
	integrateYieldDC={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[powerDC,MinDataPts->1,ReportPeriod->resolution$output];

	If[NumberQ[nominalP],
	(* calculate specific yield *)
	specificYieldDC={First@#,#[[2]]/nominalP*1000}&/@integrateYieldDC;

	(* calculate insolation and PR *)
	If[MemberQ[columns,"G"],
		insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
		PRdc={First@#,#[[3]]/nominalP/#[[2]]*1000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"],colIndex["Pdc"]}]],irradianceThreshold[[1]]<#[[2]]<irradianceThreshold[[2]]&&NumericQ@#[[3]]&&NonNegative@#[[3]]&],MinDataPts->1,ReportPeriod->resolution$output];
	];
	,
	If[MemberQ[columns,"G"],
		insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
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

If[MemberQ[columns,"Pac"],
	(*test if Vac Iac and Pac are consistent*)
	If[Abs@Mean[(Times@@@data[[All,{colIndex["Vac"],colIndex["Iac"]}]]-data[[All,{colIndex["Pac"]}]])/data[[All,{colIndex["Pac"]}]]]>0.05,Print["discrepancy between AC voltage current and power"];];

	(* calculate yield using P or V and I*)
	powerAC={First@#,If[NumericQ@#[[4]]&&NonNegative@#[[4]],#[[4]],If[NumericQ@#[[2]]&&NumericQ@#[[3]]&&#[[2]]>0&&#[[3]]>0,#[[2]]*#[[3]],0]]}&/@data[[All,{colIndex["Timestamp"],colIndex["Vac"],colIndex["Iac"],colIndex["Pac"]}]];
	integrateYieldAC={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[powerAC,MinDataPts->1,ReportPeriod->resolution$output];

, (* else, calculate yield with V and I *)
	powerAC={First@#,If[#[[2]]>0&&#[[3]]>0,#[[2]]*#[[3]],0]}&/@Cases[data[[All,{colIndex["Timestamp"],colIndex["Vac"],colIndex["Iac"]}]],{_,_?NumericQ,_?NumericQ}];
	integrateYieldAC={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[powerAC,MinDataPts->1,ReportPeriod->resolution$output];

];

If[NumberQ[nominalP],
	(* calculate specific yield *)
	specificYieldAC={First@#,#[[2]]/nominalP*1000}&/@integrateYieldAC;

	(* calculate insolation and PR *)
	If[MemberQ[columns,"G"],
	If[insolation===Null,
		insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
	];
	PRac={First@#,#[[3]]/nominalP/#[[2]]*1000}&/@PeriodSum[
		{#[[1]],#[[2]],If[NumericQ@#[[5]]&&NonNegative@#[[5]],#[[5]],#[[3]]*#[[4]]]}&/@Select[data[[All,{colIndex["Timestamp"],colIndex["G"],colIndex["Vac"],colIndex["Iac"],colIndex["Pac"]}]],irradianceThreshold[[1]]<#[[2]]<irradianceThreshold[[2]]&&(NumericQ@#[[3]]&&NumericQ@#[[4]]&&#[[3]]>=0&&#[[4]]>=0||NumericQ@#[[5]]&&NonNegative@#[[5]])&]
		,MinDataPts->1,ReportPeriod->resolution$output];
	];
,
	If[MemberQ[columns,"G"]&&insolation===Null,
	insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
	];
];

, (*else*)

If[MemberQ[columns,"Pac"],
	(*non valid readings like "NA" or negative values are removed*)
	powerAC=Select[data[[All,{colIndex["Timestamp"],colIndex["Pac"]}]],NumberQ[#[[2]]]&&NonNegative[#[[2]]]&];
	integrateYieldAC={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[powerAC,MinDataPts->1,ReportPeriod->resolution$output];

If[NumberQ[nominalP],
	(* calculate specific yield *)
	specificYieldAC={First@#,#[[2]]/nominalP*1000}&/@integrateYieldAC;

	(* calculate insolation and PR *)
	If[MemberQ[columns,"G"],
	If[insolation===Null,
		insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
	];
	PRac={First@#,#[[3]]/nominalP/#[[2]]*1000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"],colIndex["Pac"]}]],irradianceThreshold[[1]]<#[[2]]<irradianceThreshold[[2]]&&NumericQ@#[[3]]&&NonNegative@#[[3]]&],MinDataPts->1,ReportPeriod->resolution$output];
	];
,
	If[MemberQ[columns,"G"]&&insolation===Null,
		insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
	];
];
];

];

AppendTo[outputData,"AC integrated yield"->integrateYieldAC];
AppendTo[plots,"AC integrated yield"->If[integrateYieldAC=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@integrateYieldAC,PlotLabel->"AC integrated yield",FrameLabel->{None,"yield (kWh)"},plotOptions]]];
AppendTo[outputData,"AC specific yield"->specificYieldAC];
AppendTo[plots,"AC specific yield"->If[specificYieldAC=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@specificYieldAC,PlotLabel->"AC specific yield",FrameLabel->{None,"yield (kWh)"},plotOptions]]];
AppendTo[outputData,"PRac"->PRac];
AppendTo[plots,"PRac"->If[PRac=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@PRac,PlotLabel->"PRac",FrameLabel->{None,"PR"},plotOptions]]];

(* for case with only irradiance data *)
If[MemberQ[columns,"G"],
	insolation={First@#,#[[2]]*60*resolution$input/3600000}&/@PeriodSum[Select[data[[All,{colIndex["Timestamp"],colIndex["G"]}]],0<#[[2]]<irradianceThreshold[[2]]&],MinDataPts->1,ReportPeriod->resolution$output];
];

AppendTo[outputData,"insolation"->insolation];
AppendTo[plots,"insolation"->If[insolation=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@insolation,PlotLabel->"insolation",FrameLabel->{None,"insolation (kWh)"},plotOptions]]];

(* ---------- report cumulative energy meter readings, if applicable -------------- *)
If[MemberQ[columns,"cum_meter_reading"],
cumEnergy=PeriodSpan[data[[All,{colIndex["Timestamp"],colIndex["cum_meter_reading"]}]],MinDataPts->2,ReportPeriod->resolution$output];
AppendTo[outputData,"meter reading"->cumEnergy];
AppendTo[plots,"meter reading"->If[cumEnergy=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@cumEnergy,PlotLabel->"meter reading",FrameLabel->{None,"yield (kWh)"},plotOptions]]];
];

(* --------------- calculate inverter efficiency if applicable ------------------- *)
If[powerDC=!=Null && powerAC=!=Null,
\[Eta]inverter=Flatten[{#,Rest/@{##2}}]&@@@(Pick[#,Length@#>1]&/@GatherBy[Join[powerDC,powerAC],First]);
\[Eta]inverter={First@#,#[[3]]/#[[2]]}&/@PeriodSum[Cases[\[Eta]inverter,{_,_?Positive,_}],MinDataPts->1,ReportPeriod->resolution$output];
AppendTo[outputData,"\[Eta]inverter"->\[Eta]inverter];
AppendTo[plots,"\[Eta]inverter"->If[\[Eta]inverter=!=Null,DateListPlot[Tooltip[#,{DateObject@First@#,Last@#}]&/@\[Eta]inverter,PlotLabel->"\[Eta]inverter",FrameLabel->{None,"\[Eta]"},plotOptions]]];
];

Return[{outputData,plots}];
];


TimeSeriesInspection[data_Dataset,columns:{___String}:{"Timestamp","G","Vdc","Idc","Pdc","Vac","Iac","Pac"},opt:OptionsPattern[]]:=TimeSeriesInspection[FromDataset@data,columns,opt];


TimeSeriesInspection[data_Association/;data["PVDataObject"]]:=TimeSeriesInspection[
data["data"],data["columns"],NominalPower->data["NominalPower"],Tc->data["Tc"],ReportPeriod->data["ReportPeriod"],InputResolution->data["InputResolution"]
]


(* ::Text:: *)
(*TimeSeriesSummary produces a table summarizing the total and mean of each time period. *)
(*Input is the OutputData from TimeSeriesInspection. *)


TimeSeriesSummary[tsOutput_Association]:=With[{merged=MergeData[tsOutput//Values,"Fast"]},
Grid[Append[Append[Prepend[ReduceDateObject[merged],Prepend[Keys@tsOutput,""]],Prepend[Rest@Total@merged,"total"]],Prepend[Rest@Mean@merged,"mean"]],Frame->All]];


(* ::Text:: *)
(*TimeSeriesInspect crops out and plots the part of data within specified time window. *)
(*Type of input data can be Dataset, Temporal data, tables with pure timestamp-value(s) pairs, or tables with title rows. *)
(*Time window can be specified as {y,m,d,h,m,s}, or DateObject. *)
(*Options can be specified to DateListPlot. *)


TimeSeriesInspect[data_,start_,end_,opt:OptionsPattern[]]:=Module[{ts,titles,croppedTS,plots},
(* define time series *)
If[Head@data===TemporalData,
	ts=data;
,(*else*)
	ts=ToTemporalData@data;
];

titles=Which[
	Head@data===Dataset,
		Rest@Normal@Keys@First@data,
	Head@data===List,
		With[{t=FirstCase[data,{__String}]},If[MissingQ@t,Rest[Range@Dimensions[data][[2]]-1],Rest@t]]
	];
croppedTS=TimeSeriesWindow[ts,{start,end}];
plots=MapThread[DateListPlot[#1,PlotLabel->#2,opt]&,{croppedTS["DatePaths"],titles}];

Return@{croppedTS,plots};
];


(* ::Text:: *)
(*TimeSeriesAlbum gives an object containing data slices and their plots for each column.*)


TimeSeriesAlbum[dataIn_,groupBy:_:{"Year","-","Month","-","Day"},opt:OptionsPattern[]]:=Module[{self=<||>,data,columnNames,groupData,index,timestampPos,cols},

Which[
Head@dataIn===Dataset,
	columnNames=Normal@Keys@First@dataIn;
	data=Normal@Values@dataIn;
,
Head@dataIn===List&&ArrayDepth@dataIn==2,
	(* assumes first pure string row is header, if not, assume timestamp position at first column *)
	columnNames=With[{t=FirstCase[data,{__String}]},If[MissingQ@t,Prepend[StringJoin["column",#]&/@ToString/@Rest[Range@Dimensions[data][[2]]-1],"timestamp"],t]];
	data=dataIn;
,
True,
	Print@"input data error";
	Abort[];
	];

index=AssociationThread[columnNames->Range@Length@columnNames];
timestampPos=index@FirstCase[columnNames,Alternatives@@(Flatten[{Identity@#,Capitalize/@#}]&[{"timestamp", "time" , "date","data_date"}])]/._Missing->1;
groupData=GroupBy[data,DateString[#[[timestampPos]],groupBy]&];

(*AppendTo[self,"grouped_data"\[Rule]groupData];*)
AppendTo[self,"bins"->Keys@groupData];
AppendTo[self,"columns"->columnNames];
AppendTo[self,"data":>(groupData[#1]&)];
AppendTo[self,"plot":>(DateListPlot[groupData[#1][[All,{timestampPos,index@#2}]],FrameLabel->{None,Style[#2,Bold]},opt,ImageSize->Medium,GridLines->Automatic]&)];
(*AppendTo[self,"album"\[RuleDelayed]Manipulate[self["plot"][bins,column],{bins,self["bins"]},{column,Drop[self["columns"],{timestampPos}]}]];*)
AppendTo[self,"album":>
	With[{func=self["plot"],bin=self["bins"],col=Drop[self["columns"],{timestampPos}]},
		Manipulate[
			With[{plot=func[bins,column]},
				Column@{
					Button["save plot",
						Export[ToString@bins<>".png",
							plot
						]
					],
					plot
				}
			],
			{{bins,First@bin,"bins"},bin},{{column,First@col,"column"},col},
			Row@{Button["previous bin",bins=bin[[With[{p=Position[bin,bins][[1,1]]},Max[p-1,1]]]]],Button["next bin",bins=bin[[Min[Position[bin,bins][[1,1]]+1,Length@bin]]]]}
		]
	]
];
(*AppendTo[self,"album"\[RuleDelayed]
	With[{func=self["plot"],bin=self["bins"],tpos=timestampPos},
		Column[{
		TogglerBar[Dynamic[cols],Drop[self["columns"],{tpos}]],
		Manipulate[Row@Table[func[bins,c],{c,cols}],
		{{bins,First@bin,"bins"},bin}
		]
		}]
	]
];*)
AppendTo[self,"album_extendable":>
	With[{func=self["plot"],bin=self["bins"],col=Drop[self["columns"],{timestampPos}]},
		makeAlbum[func,col,bin]
	]
];

Return@self;
];


(*Off[Table::iterb];*)
makeAlbum[func_,col_,bin_]:=
Column[{
	TogglerBar[Dynamic[cols],col,Appearance->"Row"],
	Button["clear selection",cols={}],
	Manipulate[
		With[{plot=Table[func[bins,c],{c,If[Head@cols=!=List||cols==={},{First@col},cols]}]},
			Column@{
				Button["save plot",
					Export[ToString@bins<>".png",
						GraphicsGrid[Partition[plot,UpTo[4]],ImageSize->Full]
					]
				],
				Row@plot
			}
		],
		{{bins,First@bin,"bins"},bin},
		Row@{Button["previous bin",bins=bin[[With[{p=Position[bin,bins][[1,1]]},Max[p-1,1]]]]],Button["next bin",bins=bin[[Min[Position[bin,bins][[1,1]]+1,Length@bin]]]]}
	]
	}]




(* ::Section::Closed:: *)
(*Cross-sectional inspection*)


(* ::Text:: *)
(*Note: *)
(*make sure column names are properly defined and correspond to data. *)
(*make sure nominal power is in the same unit as Pdc and Pac. *)
(**)
(*output plots: *)
(*- Pdc-G*)
(*- Yf_dc-Yr*)
(*- Pac-G*)
(*- Pac-Pdc*)
(*- eff_inverter-G*)
(*- Yf_ac-Yr*)
(*- Vac-Yf_ac*)
(*- Vdc-Tmod*)
(*- PRdc-Tmod*)
(*- PRac-Tmod*)
(*- delta_Tmod _Tamb-Yr*)
(*- Vdc-G*)
(*- Idc-G*)


CrossSectionInspection[data_List?(ArrayDepth@#==2&),columns:{___String}:{"G","Vdc","Idc","Pdc","Tmod","Vac","Iac","Pac","Tamb"},opt:OptionsPattern[]]:=Module[{colIndex,nominalP,PRdc=Null,PRac=Null,\[Eta]inverter,dcPlots=<||>,acPlots=<||>,tempData,tempData2,defaultOptions,userOptions},
nominalP=OptionValue[NominalPower];

defaultOptions={ImageSize->Large,Frame->{{True,True},{True,True}},FrameStyle->Directive[Black,Thickness[0.003]],FrameTicks->{{Automatic,None},{Automatic,None}},GridLines->Automatic,LabelStyle->{FontFamily->"Helvetica",FontSize->18,FontWeight->Bold,FontColor->Black},ImageMargins->15};
userOptions=OptionValue@PlotOptions;

If[Length[columns]!=Dimensions[data][[2]],
	Print["error: columns are not properly defined"];
	Abort[];
];

colIndex=AssociationThread[columns->Range[Length[columns]]];

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~ All plots involve irradiance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
If[MemberQ[columns,"G"],

(* ------------------------- plots involving DC power --------------------------------*)
If[MemberQ[columns,"Pdc"],
	(*DC power-irradiance plot*)
	AppendTo[dcPlots,"Pdc-G"->
		ListPlot[data[[All,{colIndex["G"],colIndex["Pdc"]}]],PlotLabel->"Pdc-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC power"},userOptions,defaultOptions]
	];

	If[NumberQ[nominalP],
		(*Yf_dc-Yr plot*)
		AppendTo[dcPlots,"Yf_dc-Yr"->
			ListPlot[{#[[1]]/1000,#[[2]]/nominalP}&/@data[[All,{colIndex["G"],colIndex["Pdc"]}]],PlotLabel->"Yf_dc-Yr",FrameLabel->{"reference yield","DC final yield"},Epilog->Line[{{0,0},{1.5,1.5}}],userOptions,defaultOptions]
		];

		(*calculate PR_dc*)
		PRdc=PR[data[[All,colIndex["Pdc"]]],nominalP,data[[All,colIndex["G"]]]];
	];

,

	(*alternative DC power-irradiance and Yf_dc-Yr plot and PR_dc calculation using Vdc and Idc*)
	If[MemberQ[columns,"Vdc"]&&MemberQ[columns,"Idc"],
		tempData={#[[1]],#[[2]]*#[[3]]}&/@(data[[All,{colIndex["G"],colIndex["Vdc"],colIndex["Idc"]}]]//Cases[{_?Positive,_?Positive,_?Positive}]);
		AppendTo[dcPlots,"Pdc-G"->
			ListPlot[tempData,PlotLabel->"Pdc-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC power"},userOptions,defaultOptions]
		];

		If[NumberQ[nominalP],
			AppendTo[dcPlots,"Yf_dc-Yr"->
				ListPlot[{#[[1]]/1000,#[[2]]/nominalP}&/@tempData,PlotLabel->"Yf_dc-Yr",FrameLabel->{"reference yield","DC final yield"},Epilog->Line[{{0,0},{1.5,1.5}}],userOptions,defaultOptions]
			];
		PRdc=PR[tempData[[All,2]],nominalP,tempData[[All,1]]];
		];
	];
];

(* ------------------------- plots involving AC power --------------------------------*)
If[MemberQ[columns,"Pac"],
	(*AC power-irradiance plot*)
	AppendTo[acPlots,"Pac-G"->
		ListPlot[data[[All,{colIndex["G"],colIndex["Pac"]}]],PlotLabel->"Pac-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","AC power"},userOptions,defaultOptions]
	];

	If[MemberQ[columns,"Pdc"],
		(*Pac-Pdc plot*)
		AppendTo[acPlots,"Pac-Pdc"->
			ListPlot[data[[All,{colIndex["Pdc"],colIndex["Pac"]}]],PlotLabel->"Pac-Pdc",FrameLabel->{"DC power","AC power"},userOptions,defaultOptions]
		];

		(*inverter efficiency-irradiance plot*)
		AppendTo[acPlots,"eff_inverter-G"->
			ListPlot[{#[[1]],#[[2]]/#[[3]]}&/@Cases[data[[All,{colIndex["G"],colIndex["Pac"],colIndex["Pdc"]}]],{__?Positive}],
			PlotLabel->"eff_inverter-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","inverter efficiency"},Epilog->{Thick,Magenta,Line[{{-100,1},{1500,1}}]},userOptions,defaultOptions]
		];
	];

	If[NumberQ[nominalP],
		(*Yf_ac-Yr plot*)
		AppendTo[acPlots,"Yf_ac-Yr"->
			ListPlot[{#[[1]]/1000,#[[2]]/nominalP}&/@data[[All,{colIndex["G"],colIndex["Pac"]}]],
			PlotLabel->"Yf_ac-Yr",FrameLabel->{"reference yield","AC final yield"},Epilog->Line[{{0,0},{1.5,1.5}}],userOptions,defaultOptions]
		];

		(*calculate PR_ac*)
		PRac=PR[data[[All,colIndex["Pac"]]],nominalP,data[[All,colIndex["G"]]]];

		(*Vac-Yf_ac plot*)
		If[MemberQ[columns,"Vac"],
			AppendTo[acPlots,"Vac-Yf_ac"->
				ListPlot[{#[[2]]/nominalP,#[[1]]}&/@data[[All,{colIndex["Vac"],colIndex["Pac"]}]],PlotLabel->"Vac-Yf_ac",FrameLabel->{"AC final yield","AC voltage (V)"},userOptions,defaultOptions]
			];
		];
	];

,

	(*alternative plots using Vac and Iac when Pac is not available. For three phase, Vac and Iac should be the average. *)
	If[MemberQ[columns,"Vac"]&&MemberQ[columns,"Iac"],
		(*alternative AC power-irradiance plot, tempData={G,Pac}*)
		tempData={#[[1]],#[[2]]*#[[3]]}&/@(data[[All,{colIndex["G"],colIndex["Vac"],colIndex["Iac"]}]]//Cases[{_?Positive,_?Positive,_?Positive}]);
		AppendTo[acPlots,"Pac-G"->
			ListPlot[tempData,PlotLabel->"Pac-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC power"},userOptions,defaultOptions]
		];
		If[MemberQ[columns,"Pdc"],
			(*alternative Pac-Pdc plot*)
			AppendTo[acPlots,"Pac-Pdc"->
				ListPlot[{data[[All,colIndex["Pdc"]]],tempData[[All,2]]}\[Transpose],PlotLabel->"Pac-Pdc",FrameLabel->{"DC power","AC power"},userOptions,defaultOptions]
			];
			(*alternative inverter efficiency-irradiance plot*)
			AppendTo[acPlots,"eff_inverter-G"->
				ListPlot[{#[[3]]*#[[2]]/#[[4]],#[[1]]}&/@Cases[data[[All,{colIndex["G"],colIndex["Vac"],colIndex["Iac"],colIndex["Pdc"]}]],{__?Positive}],
				PlotLabel->"eff_inverter-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","inverter efficiency"},Epilog->{Thick,Magenta,Line[{{-100,1},{1500,1}}]},userOptions,defaultOptions]
			];
		,
			If[MemberQ[columns,"Vdc"]&&MemberQ[columns,"Idc"],
				tempData2=data[[All,colIndex["Vdc"]]]*data[[All,colIndex["Idc"]]];
				(*alternative Pac-Pdc plot*)
				AppendTo[acPlots,"Pac-Pdc"->
					ListPlot[{tempData2,tempData[[All,2]]}\[Transpose],PlotLabel->"Pac-Pdc",FrameLabel->{"DC power","AC power"},userOptions,defaultOptions]
				];
				(*alternative inverter efficiency-irradiance plot*)
				AppendTo[acPlots,"eff_inverter-G"->
					ListPlot[{#[[2]]/#[[3]],#[[1]]}&/@Cases[Join[tempData,List@tempData2,2],{__?Positive}],
					PlotLabel->"eff_inverter-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","inverter efficiency"},Epilog->{Thick,Magenta,Line[{{-100,1},{1500,1}}]},userOptions,defaultOptions]
				];
			];
		];

		If[NumberQ[nominalP],
			(*alternative Yf_ac-Yr plot*)
			AppendTo[dcPlots,"Yf_ac-Yr"->
				ListPlot[{#[[1]]/1000,#[[2]]/nominalP}&/@tempData,PlotLabel->"Yf_ac-Yr",FrameLabel->{"reference yield","AC final yield"},Epilog->Line[{{0,0},{1.5,1.5}}],userOptions,defaultOptions]
			];
			(*alternative PRac calculation*)
			PRac=PR[tempData[[All,2]],nominalP,tempData[[All,1]]];
			(*alternative Vac-Yf_ac plot*)
			AppendTo[acPlots,"Vac-Yf_ac"->
				ListPlot[{#[[2]]*#[[3]]/nominalP,#[[1]]}&/@data[[All,{colIndex["Vac"],colIndex["Iac"]}]],PlotLabel->"Vac-Yf_ac",FrameLabel->{"AC voltage (V)","AC final yield"},userOptions,defaultOptions]
			];
		];
	];
];

(* ------------------------- plots involving module temperature --------------------------------*)
If[MemberQ[columns,"Tmod"],
	(*Vdc-Tmod plot*)
	If[MemberQ[columns,"Vdc"],
		AppendTo[dcPlots,"Vdc-Tmod"->
			ListPlot[data[[All,{colIndex["Tmod"],colIndex["Vdc"]}]],PlotLabel->"Vdc-Tmod",FrameLabel->{"module temperature","DC voltage"},userOptions,defaultOptions]
		];
	];

	(*PR-Tmod plot*)
	If[PRdc=!=Null,
		AppendTo[dcPlots,"PRdc-Tmod"->
			ListPlot[{data[[All,colIndex["Tmod"]]],PRdc}\[Transpose],PlotLabel->"PRdc-Tmod",FrameLabel->{"module temperature","PR_dc"},userOptions,defaultOptions]
		];
	];
	If[PRac=!=Null,
		AppendTo[acPlots,"PRac-Tmod"->
			ListPlot[{data[[All,colIndex["Tmod"]]],PRac}\[Transpose],PlotLabel->"PRac-Tmod",FrameLabel->{"module temperature","PR_ac"},userOptions,defaultOptions]
		];
	];

	(*(Tmod-Tamb)-Yr plot*)
	If[MemberQ[columns,"Tamb"],
		AppendTo[dcPlots,"delta_Tmod_Tamb-Yr"->
			ListPlot[{#[[2]]-#[[3]],#[[1]]/1000}&/@data[[All,{colIndex["G"],colIndex["Tmod"],colIndex["Tamb"]}]],PlotLabel->"delta_Tmod_Tamb-Yr",FrameLabel->{"reference yield","Tmod-Tamb"},userOptions,defaultOptions]
		];
	];
];

(* ------------------------- others --------------------------------*)
If[MemberQ[columns,"Vdc"],
	(*Vdc-irradiance plot*)
	AppendTo[dcPlots,"Vdc-G"->
		ListPlot[data[[All,{colIndex["G"],colIndex["Vdc"]}]],PlotLabel->"Vdc-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC voltage (V)"},userOptions,defaultOptions]
	];
];

If[MemberQ[columns,"Idc"],
	(*Idc-irradiance plot*)
	AppendTo[dcPlots,"Idc-G"->
		ListPlot[data[[All,{colIndex["G"],colIndex["Idc"]}]],PlotLabel->"Idc-G",FrameLabel->{"irradiance (W/\!\(\*SuperscriptBox[\(m\), \(2\)]\))","DC current (A)"},userOptions,defaultOptions]
	];
];

,

Print["warning: no irradiance data present"];
If[MemberQ[columns,"Tmod"]&&MemberQ[columns,"Vdc"],
	(*Vdc-Tmod plot*)
	AppendTo[dcPlots,"Vdc-Tmod"->
		ListPlot[data[[All,{colIndex["Vdc"],colIndex["Tmod"]}]],PlotLabel->"Vdc-Tmod",FrameLabel->{"module temperature","DC voltage (V)"},userOptions,defaultOptions]
	];
];

If[MemberQ[columns,"Pac"]&&MemberQ[columns,"Vac"]&&NumberQ[nominalP],
	AppendTo[acPlots,"Vac-Yf_ac"->
		ListPlot[{#[[2]]/nominalP,#[[1]]}&/@data[[All,{colIndex["Vac"],colIndex["Pac"]}]],PlotLabel->"Vac-Yf_ac",FrameLabel->{"AC yield","AC voltage"},userOptions,defaultOptions]
	];
];

If[MemberQ[columns,"Pac"]&&MemberQ[columns,"Pdc"],
	AppendTo[acPlots,"Pac-Pdc"->
		ListPlot[data[[All,{colIndex["Pdc"],colIndex["Pac"]}]],PlotLabel->"Pac-Pdc",FrameLabel->{"DC power","AC power"},userOptions,defaultOptions]
	];
];

];

Return[acPlots~Join~dcPlots]
];


CrossSectionInspection[data_Dataset,columns:{___String}:{"G","Vdc","Idc","Pdc","Tmod","Vac","Iac","Pac","Tamb"},opt:OptionsPattern[]]:=CrossSectionInspection[FromDataset@data,columns,opt];


CrossSectionInspection[data_Association/;data["PVDataObject"],opt:OptionsPattern[]]:=CrossSectionInspection[data["data"],data["columns"],NominalPower->data["NominalPower"],opt];


CrossSectionInspection[data_Association/;data["PVDataObject"],bin_,opt:OptionsPattern[]]:=CrossSectionInspection[data["data_binned"]@bin,data["columns"],NominalPower->data["NominalPower"],opt];


CrossSectionInspection[data_Association/;data["PVDataObject"],"ZoomIn",groupBy_:{"Year","-","Month","-","Day"},opt:OptionsPattern[]]:=With[{groupData=GroupBy[data["data"],DateString[#//extract[data["columns"],"Timestamp"],groupBy]&]},
{
Keys@groupData,
CrossSectionInspection[groupData[#],data["columns"],NominalPower->data["NominalPower"],opt]&
}
];


(* ::Section::Closed:: *)
(*Helper functions*)


(* ::Text:: *)
(*Prepare a PVData object to standardize input PV data, as input to TimeSeriesInspection and CrossSectionInspection. *)
(*Function converts timestamps to DateObjects, auto detect time resolution, and pass on all specified options. *)


PVDataPrep[data_List?(ArrayDepth@#==2&),Shortest[columns:_:{"Timestamp","G","Vdc","Idc","Pdc","Tmod","Vac","Iac","Pac","Tamb"}],nominalP:_?NumericQ:Null,groupBy:_:"Off",opt:OptionsPattern[]]:=Module[{self=<|"PVDataObject"->True|>,dataOut,colIndex,inputResolution,groupData=<||>},

If[Length[columns]!=Dimensions[data][[2]],
	Print["error: columns are not properly defined"];
	Abort[];
];

colIndex=AssociationThread[columns->Range[Length[columns]]];
dataOut=data;

(* standardize timestamp as DateObjects *)
If[MemberQ[columns,"Timestamp"],
	If[!ContainsOnly[Head/@RandomSample[dataOut[[All,colIndex["Timestamp"]]],UpTo@300],{DateObject}],
		dataOut=ConvertDateObject[dataOut,OptionValue@"DateFormat",OptionValue@"TimeZone",colIndex["Timestamp"]];
	];
];

(* binned data *)
If[groupBy=!="Off",
	groupData=GroupBy[dataOut,groupBy]
];

(* standardize units *)
If[MatchQ[OptionValue@"Pdc_unit","kW"|"kw"]&&MemberQ[columns,"Pdc"],dataOut[[All,colIndex["Pdc"]]]*=1000;];
If[MatchQ[OptionValue@"Pac_unit","kW"|"kw"]&&MemberQ[columns,"Pac"],dataOut[[All,colIndex["Pac"]]]*=1000;];
If[MatchQ[OptionValue@"Tamb_unit","K"|"Kelvin"|"kelvin"]&&MemberQ[columns,"Tamb"],dataOut[[All,colIndex["Tamb"]]]-=273.15;];

(* auto detect input resolution *)
inputResolution=OptionValue@InputResolution;
If[inputResolution===Automatic,inputResolution=QuantityMagnitude[First@Commonest@Differences@Take[dataOut[[All,colIndex["Timestamp"]]],UpTo@200],"Minutes"];];

(* construct object *)
AppendTo[self,"data"->dataOut];
AppendTo[self,"columns"->columns];
AppendTo[self,"data_binned"->groupData];
AppendTo[self,"bins"->Keys@groupData];
AppendTo[self,"NominalPower"->If[MatchQ[OptionValue@"Capacity_unit","kW"|"kw"]&&nominalP=!=Null,nominalP*1000,nominalP]];
AppendTo[self,"InputResolution"->inputResolution];
AppendTo[self,"Tc"->OptionValue@Tc];
AppendTo[self,"ReportPeriod"->OptionValue@ReportPeriod];

Return@self;
];


PVDataPrep[data_Dataset,Shortest[columns:_:{"Timestamp","G","Vdc","Idc","Pdc","Tmod","Vac","Iac","Pac","Tamb"}],nominalP:_?NumericQ:Null,opt:OptionsPattern[]]:=PVDataPrep[FromDataset@data,columns,nominalP,opt];




(* ::Chapter::Closed:: *)
(*Spectrum related calculations*)


(* ::Text:: *)
(*Simple scaling of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. *)


SpecScale[scale_][spec_]:={First@spec}~Join~(scale*Rest@spec);


(* ::Text:: *)
(*This function calculates integrated intensity from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. Additional care is taken to deal with the highly oscillatory nature of spectrum shape. *)


SpecIntensity[spec_]:=Module[{spectrum,wavelength,integrate,intensity},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,First[wavelength],Last[wavelength]},Method->"Trapezoidal",MaxRecursion->20]&;
intensity=integrate/@spectrum;

Return[intensity]
];


(* ::Text:: *)
(*This function calculates average spectrum from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. *)


SpecAverage[spec_]:=Module[{spectrum,wavelength,integrate,intensity,intensitySum,avgSpec},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,First[wavelength],Last[wavelength]},Method->"Trapezoidal",MaxRecursion->20]&;
intensity=integrate/@spectrum;
intensitySum=Total[intensity];
avgSpec=Thread[Times[spectrum,intensity],1]/intensitySum//Total;

Return[avgSpec]
];


(* ::Text:: *)
(*This function calculates (the list of) average photon energy (APE) from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. All quantities should be in SI units. *)


APECalc[spec_]:=Module[{spectrum,wavelength,integrate,intensity,flux,APE},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,First[wavelength],Last[wavelength]},Method->"Trapezoidal",MaxRecursion->20]&;
intensity=integrate/@spectrum;
flux=#*wavelength/(h*c)&/@spectrum;
flux=integrate/@flux;
APE=intensity/flux/q;

Return[APE]
];


(* ::Text:: *)
(*This function calculates (the list of) implied short circuit current from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. All quantities should be in SI units. *)


Photocurrent[spec_]:=Module[{spectrum,wavelength,integrate,intensity,flux,APE,Jsc},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,First[wavelength],Last[wavelength]},Method->"Trapezoidal",MaxRecursion->20]&;
flux=#*wavelength/(h*c)&/@spectrum;
flux=integrate/@flux;
Jsc=flux*q;

Return[Jsc]
];


(* ::Text:: *)
(*This function calculates (the list of) implied short circuit current in a range of wavelengths from a set of spectra. Format for input spectra set should be {{wavelength}, {spec1}, {spec2}, ...}. All quantities should be in SI units. *)


Photocurrent[spec_,{wMin_,wMax_}]:=Module[{spectrum,wavelength,integrate,intensity,flux,APE,Jsc},
wavelength=First[spec];
spectrum=Rest[spec];

integrate=NIntegrate[Interpolation[{wavelength,#}\[Transpose],InterpolationOrder->1][x],{x,Max[First[wavelength],wMin],Min[Last[wavelength],wMax]},Method->"Trapezoidal",MaxRecursion->20]&;
flux=#*wavelength/(h*c)&/@spectrum;
flux=integrate/@flux;
Jsc=flux*q;

Return[Jsc]
];


(* ::Chapter::Closed:: *)
(*End*)


End[]; 

EndPackage[]; 
