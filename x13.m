(* ::Package:: *)

(*
TODO:
1. x13:
1.1 2014-01-22 add test for missing values and an error message*)

ClearAll[x13,runx13]
quote[s_]:="\""<>s<>"\"";
deleteBraces[s_String]:=StringReplace[s,{"{"->"","}"->""}];
Options[x13]={ARIMA->True,Mode->"auto",Frequency->"Month",
Output->"e2",Outliers->{}};
Options[runx13]=Options[x13];

x13::mode="Option Mode can only take following values: auto, mult, add, pseudoadd, logadd";
x13::freq="ARIMA-X13 supports only frequencies Month and Quarter";
x13::threeyear="ARIMA-X13 runs only on 3 or more years of observations";
x13::thirtyseconds="File has not been generated";


x13[x_,OptionsPattern[]]:=Module[
{},

Which[
ARIMA \[Element] Booleans && ARIMA==True, SetOptions[x13,ARIMA]=8,
ARIMA \[Element] Booleans && ARIMA==False, SetOptions[x13,ARIMA]=0,
True,Null];

If[
	MemberQ[{"auto","mult", "add", "pseudoadd", "logadd"},OptionValue[x13,Mode]],
	Null,
	Message[x13::mode];Abort[]
];

If[
	MemberQ[{"Month","Quarter"},OptionValue[x13,Frequency]],
	Null,
	Message[x13::freq];Abort[]
];

If[
	Length@x<(OptionValue[x13,Frequency]/.{"Month"->12,"Quarter"->4})*3,
	Message[x13::threeyear];Abort[],
	Null
];

(*Add error and condtition for the Output option, all values should be in the table in the manual
If[
	Length@x<(OptionValue[x13,Frequency]/.{"Month"->12,"Quarter"->4})*3,
	Message[x13::threeyear];Abort[],
	Null
];
*)

(*Add error and condtition for oulier option. 
It should be a regular expression that checks whether they are propperly syntax.
If[
	Length@x<(OptionValue[x13,Frequency]/.{"Month"->12,"Quarter"->4})*3,
	Message[x13::threeyear];Abort[],
	Null
];
*)

(*After checking for issues run the adjustment procedute*)
runx13[x,Outliers->OptionValue[Outliers]]

]





runx13[x_,OptionsPattern[]]:=Module[
{outputText,
dateList=Sort@x,dataList,
startDate,endDate,
spanDate,
x13file, inputfile

},
(*TODO:
1. Switch between negative and positive values
2. Add frequency options: month and quarter
3. Add Multivariate support*)

dataList=dateList[[;;,2]];
startDate=DateString[First@First@dateList,{"Year",".","Month"}];
endDate=DateString[First@Last@dateList,{"Year",".","Month"}];
spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Month"}];

outputText="series{
  title=\""<>"Title"<>"\"
  start="<>startDate<>"
  data=("<>Riffle[ToString[#]<>" "&/@dataList,"\n\t",13]<>")
}
spectrum{
  savelog=peaks\t
}
transform{
  function=auto
  savelog=autotransform  
}

regression {
	variables = ("<>Quiet@Check[deleteBraces@ToString[OptionValue[Outliers]],""]<>")

}


automdl{}

outlier{
  method=addone
  critical=3.7
}
x11{
  print=none
  save=("<>ToString[OptionValue[x13,Output]]<>")}

";

Export[FileNameJoin[{$TemporaryDirectory,"mmax13.spc"}],outputText,"Text"];

inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];

Run["x13as",quote@inputfile];

(*We pause for 30 sec as it takes about this times to generate files*)
TimeConstrained[
	While[Not@FileExistsQ[FileNameJoin[
	{$TemporaryDirectory,If[Length@OptionValue[x13,Output]>0,"mmax13."<>First@OptionValue[x13,Output],"mmax13."<>OptionValue[x13,Output]]
	}]]],
30,
Message[x13::thirtyseconds];Abort[]
];

(*Pause[30];*)

readX31output[OptionValue[x13,Output]]

]



readX31output[output_String]:=Module[
{imp},
imp=Import[FileNameJoin[{$TemporaryDirectory,"mmax13."<>output}],{"Data"}];
DeleteFile[FileNameJoin[{$TemporaryDirectory,"mmax13."<>output}]];
imp[[3;;]]/.{x_,y_}:>{{Round[x/100,1],x-Round[x,100],1},y}
]

readX31output[output_List]:=Module[
{},
readX31output/@output
]
