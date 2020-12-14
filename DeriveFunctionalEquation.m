(* ::Package:: *)

(* ::Chapter:: *)
(*Derive Flow Equation*)


(* ::Input::Initialization:: *)
BeginPackage["DeriveFunctionalEquation`"]


(* ::Input::Initialization:: *)
Needs["getDSE`",FileNameJoin[{$UserBaseDirectory,"Applications","QMeS-Derivation-main","package","getDSE.m"}]]
Needs["FunctionalDerivatives`",FileNameJoin[{$UserBaseDirectory,"Applications","QMeS-Derivation-main","package","FunctionalDerivatives.m"}]]
Needs["SuperindexDiagrams`",FileNameJoin[{$UserBaseDirectory,"Applications","QMeS-Derivation-main","package","SuperindexDiagrams.m"}]]
Needs["FullDiagrams`",FileNameJoin[{$UserBaseDirectory,"Applications","QMeS-Derivation-main","package","FullDiagrams.m"}]]


(* ::Input::Initialization:: *)
DeriveFunctionalEquation::usage = "DeriveFunctionalEquation[setup, derivativelist, options]

If you already have a master equation the setup is:
setup = <|\"MasterEquation\" -> masterEquation, \"FieldSpace\" -> fields, \"Truncation\" -> truncation|>

If you first want to derive the DSE for a theory:
SetupDSE = <|\"MasterEquation\" -> <|\"getDSE\" -> \"True\", \"classicalAction\" -> classicalAction|>, 
\"FieldSpace\" -> fields, \"Truncation\" -> truncation|>

Master equations can be the FRG equation:
FRGEq = {\"Prefactor\" -> {1/2}, <|\"type\" -> \"Regulatordot\", \"indices\" -> {a, b}|>, 
<|\"type\" -> \"Propagator\", \"indices\" -> {a, b}|>}

or the mSTI:
LHSmSTIEq = {\"Prefactor\" -> {1}, <|\"type\" -> \"nPoint\", \"indices\" -> {i}, \"nPoint\" -> 1, \"spec\" -> \"none\"|>, 
<|\"type\" -> \"nPoint\", \"indices\" -> {Q[i]}, \"nPoint\" -> 1, \"spec\" -> \"BRST\"|>}

mSTIEq = {\"Prefactor\" -> {1}, <|\"type\" -> \"Regulator\", \"indices\" -> {a, b}|>, 
<|\"type\" -> \"Propagator\", \"indices\" -> {b, c}|>, 
<|\"type\" -> \"nPoint\", \"indices\" -> {c, Q[a]}, \"nPoint\" -> 2, \"spec\" -> \"BRST\"|>}

One specifies the theory by giving a list of fields (and BRST sources) with their respective indices and statistics. If no bosonic or fermionic fields are present in a theory one must insert an empty list. 
fields = <|\"bosonic\" -> {A[p, {mu, i}], B[p,{i}]},

\"fermionic\" -> {{cbar[p, {i}], c[p, {i}]}, {af[p,{i}], f[p,{i}]}},

\"BRSTsources\" -> {{Q[A], \"fermionic\"}, {Q[B], \"fermionic\"}, {Q[cbar], \"bosonic\"}, {Q[c], \"bosonic\"}, {Q[af], \"bosonic\"}, {Q[f], \"bosonic\"}}|>

The classical action and truncation are simply a list of possible vertices starting with the possible propagators. 
classicalAction = {{A, A}, {c, cbar}, {A, A, A}, {A, A, A, A}, {A, c, cbar}}

truncation = {{A, A}, {c, cbar}, {A, A, A}, {A, A, A, A}, {A, c, cbar}, {A, A, c, cbar}}


For the list of derivatives one generally has three options:
DerivativeList1 = {A, A}
DerivativeList2 = {A[a], A[b]}
DerivativeList3 = {A[-p, {mu, m}], A[p, {nu, n}]}

The options for the function getFunctionalDerivatives are:
\"OutputLevel\" ->  \"getDSE\"
\"OutputLevel\" ->  \"FunctionalDerivatives\"
\"OutputLevel\" ->  \"SuperindexDiagrams\"
\"OutputLevel\" ->  \"FullDiagrams\"

DerivativeList1 works with the first two output levels, DerivativeList2 with the first three and DerivativeList3 works for all output levels.

\"getDSE\": Computes the DSE of the theory
\"FunctionalDerivatives\": Takes functional derivatives of the master equation
\"SuperindexDiagrams\": Takes the trace in field space and sorts the objects
\"FullDiagrams\": replaces the superfield indices with physical indices and writes the objects as funtions
";


(* ::Input::Initialization:: *)
Begin["Private`"]


(* ::Input::Initialization:: *)
$DebugLevel =0;


(* ::Input::Initialization:: *)
myEcho[msg_,lvl_] := If[$DebugLevel >=lvl, Echo[msg];, Nothing;]


(* ::Text:: *)
(*Run the notebooks:*)
(*	- Setup*)
(*	- FunctionalDerivatives*)
(*	- ....*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
Clear[DeriveFunctionalEquation]
DeriveFunctionalEquation[setupAssoc_,derivativeList_,OptionsPattern[]] := Module[{masterEq,derivativeListnew, dse = False, classicalAction, fields, truncation, outputLevel,loopIndex, funcDerDiagrams,replacementList,superindexReplacementList, superindexDiags,allVars,fullDiags},

If[AssociationQ[setupAssoc[["MasterEquation"]]]==True,

classicalAction = setupAssoc[["MasterEquation","classicalAction"]];
masterEq = getDSE[classicalAction,Last@derivativeList];
derivativeListnew = derivativeList[[1;;-2]];
dse = True;
,
derivativeListnew = derivativeList;
masterEq = setupAssoc[["MasterEquation"]];

];

fields = setupAssoc[["FieldSpace"]];
truncation = setupAssoc[["Truncation"]];
myEcho[{masterEq,fields,truncation},1];
outputLevel = OptionValue["OutputLevel"];
loopIndex = OptionValue["LoopIndex"];


Switch[outputLevel,

"getDSE",
If[dse == True,
Return[masterEq]
,
Print["Master equation is aleady given. Please choose a different output (i.e. \[OpenCurlyDoubleQuote]FunctionalDerivatives\[CloseCurlyDoubleQuote], \[OpenCurlyDoubleQuote]SuperindexDiagrams\[CloseCurlyDoubleQuote] or \[OpenCurlyDoubleQuote]FullDiagrams\[CloseCurlyDoubleQuote])."];
Return[masterEq]
]
,


"FunctionalDerivatives",
{funcDerDiagrams,replacementList} = MultipleFuncDer[masterEq,derivativeListnew];
	funcDerDiagrams = funcDerDiagrams/.replacementList;

	Return[funcDerDiagrams(*,replacementList*)]
	,

"SuperindexDiagrams",
	{funcDerDiagrams,replacementList} = MultipleFuncDer[masterEq,derivativeListnew];
	

If[AssociationQ[setupAssoc[["MasterEquation"]]]==True,superindexDiags = TraceOverFields[funcDerDiagrams,derivativeList,replacementList, fields,truncation,classicalAction];
,
superindexDiags = TraceOverFields[funcDerDiagrams,derivativeList,replacementList, fields,truncation];
];


Return[superindexDiags]
	,

"FullDiagrams",
	{funcDerDiagrams,replacementList} = MultipleFuncDer[masterEq,derivativeListnew];

	If[AssociationQ[setupAssoc[["MasterEquation"]]]==True,superindexDiags = TraceOverFields[funcDerDiagrams,derivativeList,replacementList, fields,truncation,classicalAction];
	,
	superindexDiags = TraceOverFields[funcDerDiagrams,derivativeList,replacementList, fields,truncation];
	];
	{allVars,fullDiags }= InsertFeynRulesAllDiags[superindexDiags, derivativeList,fields,loopIndex]/.Private`q->Global`q;


If[OptionValue["DummyVarList"]==False,

Return[fullDiags];
	,
	
Return[{allVars,fullDiags }];
];
];



]
Options[DeriveFunctionalEquation] = {"OutputLevel"->"FunctionalDerivatives","LoopIndex"-> q, "DummyVarList"-> False};


(* ::Input:: *)
(*DeriveFunctionalEquation[Setup,DerivativeList,"OutputLevel"-> "FullDiagrams"]*)


(* ::Input::Initialization:: *)
End[]
EndPackage[]
