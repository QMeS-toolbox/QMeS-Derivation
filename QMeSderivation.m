(* ::Package:: *)

(* ::Chapter:: *)
(*Derive Functional Equation*)


(* ::Input::Initialization:: *)
BeginPackage["QMeSderivation`"]


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
LHSmSTIEq = {\"Prefactor\" -> {1}, <|\"type\" -> \"nPoint\", \"indices\" -> {Q[i]}, \"nPoint\" -> 1, \"spec\" -> \"BRST\"|>, <|\"type\" -> \"nPoint\", \"indices\" -> {i}, \"nPoint\" -> 1, \"spec\" -> \"none\"|>,}

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
getDSE::usage = "getDSE[classicalAction, firstDerivative]

Derive the DSE for a given classical action. A simple example is
getDSE[{{phi,phi},{phi,phi,phi,phi}}, phi[a]]
";


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Section:: *)
(*Load*)


(* ::Input:: *)
(*qmesDerivationDirectory=SelectFirst[*)
(*Join[*)
(*{FileNameJoin[{$UserBaseDirectory,"Applications","QMeS-Derivation"}],FileNameJoin[{$BaseDirectory,"Applications","QMeS-Derivation"}],FileNameJoin[{$InstallationDirectory,"AddOns","Applications","QMeS-Derivation"}],FileNameJoin[{$InstallationDirectory,"AddOns","Packages","QMeS-Derivation"}],FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","QMeS-Derivation"}]*)
(*},*)
(*Select[$Path,StringContainsQ[#,"QMeS-Derivation"]&]*)
(*],DirectoryQ[#]&];*)


(* ::Section:: *)
(*Debug*)


(* ::Input::Initialization:: *)
$DebugLevel =0;


(* ::Input::Initialization:: *)
myEcho[msg_,lvl_] := If[$DebugLevel >=lvl, Echo[msg];, Nothing;]


(* ::Section:: *)
(*Get DSE*)


(* ::Subsection:: *)
(*Main*)


(* ::Input::Initialization:: *)
Clear[getDSE]
getDSE[classicalAction_,firstDerivative_] := Module[{fullClassicalAction,derAction,rhsExpDSE,rhsDSE},

fullClassicalAction = generateClassicalAction[classicalAction];

derAction = takeFieldDerivative[fullClassicalAction,firstDerivative];
rhsExpDSE = replaceExpectationValue[derAction]; 
rhsDSE = takeFinalFieldDerivatives[rhsExpDSE]/."dummyEntry"->Nothing;

Return[rhsDSE]
]


(* ::Subsection:: *)
(*Generate Classical Action*)


(* ::Input::Initialization:: *)
(*generate the classical action in operator and list form form with superfield indices*)
Clear[generateClassicalAction]
generateClassicalAction[classicalAction_] := Module[{},
generateClassicalAction[{},classicalAction]
]
generateClassicalAction[actionList_,classicalAction_] := Module[{newTerm,newactionList,newclassicalAction},

newTerm = transformToClassicalObject[classicalAction[[1]]];
newactionList = Join[actionList,{newTerm}];
newclassicalAction = classicalAction[[2;;-1]];

generateClassicalAction[newactionList,newclassicalAction]
]
generateClassicalAction[actionList_,{}] := Module[{},
Return[actionList]
]


(* ::Input::Initialization:: *)
(*transform list of vertices into objects*)
Clear[transformToClassicalObject]
transformToClassicalObject[classicalActionEntry_] := Module[{objectList,objectsIndices,fullObjectList, prefac},
objectList =  (<|"type"-> "classicalField", "indices"-> {classicalActionEntry[[#]][Unique[Global`a83]]}|>)&/@Range[Length@classicalActionEntry];

objectsIndices = Evaluate[Reverse@objectList[[All,"indices"]]][[All,1]];

prefac = getPrefactor[classicalActionEntry];

fullObjectList = Join[{"Prefactor"-> {prefac},<|"type"->"nPoint","indices"->objectsIndices,"nPoint"-> Length@objectList,"spec"->"classical"|>},objectList];

Return[fullObjectList]
]


(* ::Input::Initialization:: *)
(*get correct 1/n! prefactors*)
Clear[getPrefactor]
getPrefactor[classicalActionEntry_] := Module[{modFieldList,prefacList,prefac},

modFieldList = Split[classicalActionEntry];
prefacList = 1/((Length[#]&/@modFieldList)!);
prefac = Times@@prefacList;
Return[prefac]
]


(* ::Input::Initialization:: *)
(*get correct 1/n! prefactors*)
Clear[getSign]
getSign[classicalActionEntry_] := Module[{sign},

sign = generatePrefacCombinations[classicalActionEntry];

Return[sign]
]


(* ::Input::Initialization:: *)
(*generate the classical action in operator and list form form with superfield indices*)
Clear[generatePrefacCombinations]
generatePrefacCombinations[classicalActionEntry_] := Module[{},
	generatePrefacCombinations[classicalActionEntry,{}]
]

generatePrefacCombinations[classicalActionEntry_,prefac_] := Module[{newCombinations,newactionList,combinations},
	newactionList= classicalActionEntry[[1;;-2]];
	newCombinations = Tuples[{newactionList,classicalActionEntry[[{-1}]]}];
	combinations  = Join[prefac,newCombinations];

	generatePrefacCombinations[newactionList,combinations]
]

generatePrefacCombinations[{},actionList_] := Module[{},
	Return[actionList]
]


(* ::Subsection::Closed:: *)
(*Take First Field Derivative*)


(* ::Input::Initialization:: *)
Clear[takeFieldDerivative]
takeFieldDerivative[fullClassicalAction_,firstDerivative_]:=Module[{},
takeFieldDerivative[{},fullClassicalAction,firstDerivative]
]
takeFieldDerivative[derAction_,restClassicalAction_,firstDerivative_] := Module[{newDerAction,newrestClassicalAction},

newrestClassicalAction = restClassicalAction[[2;;-1]];

newDerAction = Join[derAction,takeOneFieldDerivative[restClassicalAction[[1]],firstDerivative]];
takeFieldDerivative[newDerAction,newrestClassicalAction,firstDerivative]
]
takeFieldDerivative[derAction_,{},firstDerivative_] := Module[{},
Return[derAction]
]


(* ::Input::Initialization:: *)
Clear[takeOneFieldDerivative]
takeOneFieldDerivative[oneActionTerm_,firstDerivative_]:=Module[{derTerm, fieldObject,derField,derAction,deltaIndices,dummyaaa,startingPrefactor,prefactors,newfullPrefactor},

If[(Length@firstDerivative)==0,
derField = firstDerivative;
,
derField = Head@firstDerivative;
];

startingPrefactor = oneActionTerm[[1]];

derAction = (fieldObject =oneActionTerm[[2+#]];

If[ToString[(Head@fieldObject[["indices",1]])]==ToString[derField],
If[(Length@firstDerivative)>0,
deltaIndices = {(List@@(fieldObject[["indices",1]]))[[1]]-> (List@@firstDerivative)};
,

deltaIndices = {((fieldObject[["indices",1]]))-> (firstDerivative)};
];

If[#==1,
prefactors = {};
,
prefactors = Flatten[Outer[Join,oneActionTerm[[2+1;;2+#-1,"indices"]],{{firstDerivative}},1],1];
];

newfullPrefactor = startingPrefactor;
newfullPrefactor[[2]] =Join[startingPrefactor[[2]],prefactors];

Join[{newfullPrefactor},oneActionTerm[[2;;2+#-1]],oneActionTerm[[2+#+1;;-1]]]/.deltaIndices/.derField[{dummyaaa___}]-> derField[dummyaaa]
,
Nothing
]
)&/@Range[Length@oneActionTerm[[3;;-1]]];(*bc first entry is pref and second is S*)

Return[derAction]

]



(* ::Subsection::Closed:: *)
(*Replace With Expectation Value*)


(* ::Input::Initialization:: *)
Clear[replaceExpectationValue]
replaceExpectationValue[derAction_]:=Module[{},
replaceExpectationValue[{},derAction]
]

replaceExpectationValue[expDerAction_, derAction_] := Module[{currentTerm,restDerAction,replacedTerms,expDerActionNew},
currentTerm = derAction[[1]];
restDerAction = Drop[derAction,1];

replacedTerms = replaceOneExpectationValue[currentTerm];

expDerActionNew = Join[expDerAction,replacedTerms];

replaceExpectationValue[expDerActionNew,restDerAction]
]
replaceExpectationValue[expDerAction_, {}] := Module[{},
Return[expDerAction]
]


(* ::Input::Initialization:: *)
Clear[replaceOneExpectationValue]
replaceOneExpectationValue[ActionTerm_]:=Module[{newActionTerms},

newActionTerms = scanForFirstClassicalFieldAndReplace[ActionTerm];

replaceOneExpectationValue[{},newActionTerms]
]

replaceOneExpectationValue[replacedActionTerms_, ActionTerm_] := Module[{currentActionTerm ,restActionTerm,newActionTerms,replacedActionTermsNew,restActionTermNew},
currentActionTerm = ActionTerm[[1]];
restActionTerm = Drop[ActionTerm,1];

newActionTerms = scanForFirstClassicalFieldAndReplace[currentActionTerm];


If[newActionTerms == currentActionTerm,

replacedActionTermsNew = Join[replacedActionTerms,{newActionTerms}];
restActionTermNew = restActionTerm;
,

restActionTermNew = Join[newActionTerms,restActionTerm];
replacedActionTermsNew = replacedActionTerms;
];


replaceOneExpectationValue[replacedActionTermsNew,restActionTermNew]
]
replaceOneExpectationValue[expDerAction_, {}] := Module[{},
Return[expDerAction]
]


(* ::Input::Initialization:: *)
Clear[scanForFirstClassicalFieldAndReplace]
scanForFirstClassicalFieldAndReplace[ActionTerm_]:=Module[{iter, positionFirstClField,newActionTerms,dummyIndex, newExpFieldObject, newPropObject, newDerObject},

positionFirstClField = 0;
For[iter = 2, iter <= Length@ActionTerm, iter++,

If[StringMatchQ[ActionTerm[[iter,"type"]],"classicalField"]==True,
positionFirstClField = iter;
iter = Length@ActionTerm;
]
];

If[positionFirstClField==0,

Return[ActionTerm]
,
newExpFieldObject = ActionTerm[[positionFirstClField]];
newExpFieldObject[["type"]] = "Field";

dummyIndex = Unique[Global`a83];

newPropObject = ActionTerm[[positionFirstClField]];
newPropObject[["type"]] = "Propagator";
newPropObject[["indices"]] = Join[ActionTerm[[positionFirstClField,"indices"]],{dummyIndex}];
newDerObject =ActionTerm[[positionFirstClField]];
newDerObject[["type"]] = "DerField";
newDerObject[["indices"]] = {dummyIndex};

newActionTerms = {Join[ActionTerm[[1;;positionFirstClField-1]],{newExpFieldObject},ActionTerm[[positionFirstClField+1;;-1]]],Join[ActionTerm[[1;;positionFirstClField-1]],{newPropObject,newDerObject},ActionTerm[[positionFirstClField+1;;-1]]]};
Return[newActionTerms]
]
]




(* ::Subsection::Closed:: *)
(*Take Final Derivatives*)


(* ::Input::Initialization:: *)
Clear[takeFinalFieldDerivatives]
takeFinalFieldDerivatives[ActionTerms_]:=Module[{},
takeFinalFieldDerivatives[{},ActionTerms]
]

takeFinalFieldDerivatives[DerActionTerms_, ActionTerms_] := Module[{currentActionTerm,DerActionTermsNew,ActionTermsNew,newDerTerm,ActionTermsNewAll},

currentActionTerm = ActionTerms[[1]];
ActionTermsNew = Drop[ActionTerms,1];



newDerTerm = takeOneFinalFieldDerivative[currentActionTerm];


Switch[newDerTerm ,
 {},DerActionTermsNew = Join[DerActionTerms,newDerTerm];
				ActionTermsNewAll = ActionTermsNew;
				
,
 {currentActionTerm},DerActionTermsNew = Join[DerActionTerms,newDerTerm];
					ActionTermsNewAll = ActionTermsNew;
					
,
_,   
ActionTermsNewAll = Join[newDerTerm,ActionTermsNew];
	DerActionTermsNew = DerActionTerms;
	
];



takeFinalFieldDerivatives[DerActionTermsNew,ActionTermsNewAll]
]
takeFinalFieldDerivatives[DerActionTerms_, {}] := Module[{},
Return[DerActionTerms]
]


(* ::Input::Initialization:: *)
Clear[takeOneFinalFieldDerivative]
takeOneFinalFieldDerivative[ActionTerm_]:=Module[{newActionTerms},

newActionTerms = scanForLastDerAndReplace[ActionTerm];

takeOneFinalFieldDerivative[{},newActionTerms]
]

takeOneFinalFieldDerivative[replacedActionTerms_, ActionTerm_] := Module[{currentActionTerm ,restActionTerm,newActionTerms,replacedActionTermsNew},
currentActionTerm = ActionTerm[[1]];
restActionTerm = Drop[ActionTerm,1];

newActionTerms = scanForLastDerAndReplace[currentActionTerm];



If[newActionTerms == {currentActionTerm},
replacedActionTermsNew = Join[replacedActionTerms,newActionTerms];

,

restActionTerm = Join[newActionTerms,restActionTerm];
replacedActionTermsNew = replacedActionTerms;

];

takeOneFinalFieldDerivative[replacedActionTermsNew,restActionTerm]
]
takeOneFinalFieldDerivative[expDerAction_, {}] := Module[{},
Return[expDerAction]
]


(* ::Input::Initialization:: *)
Clear[scanForLastDerAndReplace]
scanForLastDerAndReplace[ActionTerm_]:=Module[{iter, positionLastDer,newActionTerms,dummyIndex, newExpFieldObject, newPropObject, newDerObject},

positionLastDer = 0;


For[iter = Length@ActionTerm, iter >= 2, iter--,


If[StringMatchQ[ActionTerm[[iter,"type"]],"DerField"]==True,

positionLastDer = iter;
iter = 2;

]
];



Switch[positionLastDer,
0,
	Return[{ActionTerm}],

Length@ActionTerm,
	Return[{Nothing}],

_,
	newActionTerms = takeRightDer[ActionTerm,positionLastDer];

Return[newActionTerms]
]
]


(* ::Input::Initialization:: *)
(* I have no idea if these prefactors are correct or consistent with the rest*)
Clear[takeRightDer]
takeRightDer[ActionTerm_,positionLastDer_]:=Module[{positionFields,newOneActionTerm,newActionTerms,iter,replacementrules,dummyIndexProp1,dummyIndexProp2},

positionFields = (Switch[ActionTerm[[#,"type"]],
"Field",{#,"Field"},
"Propagator",{#,"Propagator"},
   "nPoint",If[ActionTerm[[#,"spec"]]=="none",{#,"nPoint"}]
])&/@Range[Length@ActionTerm,positionLastDer+1,-1];

	
newActionTerms = (
iter = positionFields[[#,1]];

Switch[positionFields[[#,2]],
"Field",
	
	newOneActionTerm = Join[ActionTerm[[1;;positionLastDer-1]],ActionTerm[[positionLastDer+1;;iter-1]],ActionTerm[[iter+1;;-1]]];
	newOneActionTerm[[1,2]] = Join[ActionTerm[[1,2]],Tuples[{ActionTerm[[positionLastDer,"indices"]],ActionTerm[[positionLastDer+1;;iter-1,"indices"]]}]];
	replacementrules = {ActionTerm[[positionLastDer,"indices",1]]-> ActionTerm[[iter,"indices",1]]};


	newOneActionTerm =newOneActionTerm/.replacementrules;

,
"Propagator",

	dummyIndexProp1 = Unique[Global`a83];
	dummyIndexProp2 = Unique[Global`a83];

	newOneActionTerm = Join[ActionTerm[[1;;positionLastDer-1]],ActionTerm[[positionLastDer+1;;iter-1]],{<|"type"->"Propagator","indices"->{ActionTerm[[iter,"indices",1]],dummyIndexProp1}|>,<|"type"->"nPoint","indices"->{dummyIndexProp1,ActionTerm[[positionLastDer,"indices",1]],dummyIndexProp2},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{dummyIndexProp2,ActionTerm[[iter,"indices",2]]}|>},ActionTerm[[iter+1;;-1]]];

	newOneActionTerm[[1,2]] = Join[ActionTerm[[1,2]],{-1}, {{dummyIndexProp2,dummyIndexProp2}},Tuples[{ActionTerm[[positionLastDer,"indices"]],Join[ActionTerm[[positionLastDer+1;;iter-1,"indices"]],{ActionTerm[[iter,"indices",1]]}]}]];
	
,
"nPoint",

	newOneActionTerm = ActionTerm;
	newOneActionTerm[[iter,"nPoint"]] = ActionTerm[[iter,"nPoint"]]+1;
	newOneActionTerm[[iter,"indices"]] = Join[ActionTerm[[iter,"indices",1]],ActionTerm[[positionLastDer,"indices",1]],ActionTerm[[iter,"indices",2;;-1]]];


	newOneActionTerm[[1,2]] = Join[ActionTerm[[1,2]], {{ActionTerm[[positionLastDer,"indices",1]],ActionTerm[[iter,"indices",1]]}}];
	
	newOneActionTerm = Drop[newOneActionTerm,positionLastDer];
	
	

];
newOneActionTerm
)&/@Range[Length@positionFields];


Return[newActionTerms]
]


(* ::Section:: *)
(*Functional Derivatives*)


(* ::Subsection:: *)
(*Multiple Functional Derivatives*)


(* ::Input::Initialization:: *)
Clear[DerRules] (* works for regs, regdots, props, nPoints, fields *)
DerRules[operator_,dummyFieldIndex_] := Module[{operatorType,firstDummyProp, secondDummyProp,dummynPoint,operatorDer,firstDummyIndices,secondDummyIndices, operatorPref,indexReplacement = {}},

myEcho["DerRules",1];
myEcho[{"operator: ",operator},2];

operatorType = operator[["type"]];
myEcho[{"operator type: ", operatorType},3];

firstDummyIndices = Unique[Global`a83];
secondDummyIndices = Unique[Global`a83];

Switch[operatorType,

"Regulator", 
operatorDer = {{"dummyEntry"}}; 
operatorPref = {0}; 
myEcho["Regulator Der",2]; 
myEcho[{operatorPref,operatorDer},3];,

"Regulatordot", 
operatorDer = {{"dummyEntry"}}; 
operatorPref = {0};
myEcho["Regulatordot Der",2]; 
myEcho[{operatorPref,operatorDer},3];,


"Propagator", 
firstDummyProp = operator;
firstDummyProp[["indices",2]]=firstDummyIndices;
secondDummyProp = operator;
secondDummyProp[["indices",1]]=secondDummyIndices;
dummynPoint = <|"type"-> "nPoint", "indices"-> Table[Null,3], "nPoint"-> 3, "spec"-> "none"|>;
dummynPoint[["indices"]][[1]]=firstDummyIndices;
dummynPoint[["indices"]][[2]]=dummyFieldIndex;
dummynPoint[["indices"]][[3]]=secondDummyIndices;

operatorDer = {firstDummyProp,dummynPoint,secondDummyProp};
operatorPref = {-1,{operator[["indices",1]],dummyFieldIndex},{secondDummyIndices,secondDummyIndices}};
myEcho["Propagator Der",2];
myEcho[{operatorPref,operatorDer},3]; ,

"nPoint", 


Switch[operator[["spec"]],
	"none",
	
		dummynPoint = operator;
		dummynPoint[["nPoint"]] = operator[["nPoint"]]+1;
		dummynPoint[["indices"]] = Table[Null,operator[["nPoint"]]+1];
		dummynPoint[["indices",1]] = operator[["indices",1]];
		dummynPoint[["indices",2]] = dummyFieldIndex;

		(dummynPoint[["indices",#]] =  operator[["indices",#-1]])&/@Table[i,{i,3,dummynPoint[["nPoint"]]}];

		operatorDer = {dummynPoint};
		operatorPref = {{dummynPoint[["indices",1]],dummyFieldIndex}};
		myEcho["nPoint Der",2];
		myEcho[{operatorPref,operatorDer},3];,

	"BRST",
		dummynPoint = operator;
		dummynPoint[["nPoint"]] = operator[["nPoint"]]+1;
		dummynPoint[["indices"]] = Table[Null,operator[["nPoint"]]+1];
		dummynPoint[["indices",1]] = operator[["indices",1]];
		dummynPoint[["indices",2]] = dummyFieldIndex;

		(dummynPoint[["indices",#]] =  operator[["indices",#-1]])&/@Table[i,{i,3,dummynPoint[["nPoint"]]}];

		operatorDer = {dummynPoint};
		operatorPref = {{dummynPoint[["indices",1]],dummyFieldIndex}};
		myEcho["nPoint Der",2];
		myEcho[{operatorPref,operatorDer},3];,

	"classical",
	
	operatorDer = {{"dummyEntry"}}; 
		operatorPref = {0}; 

		];
,

"Field",

If[Length@dummyFieldIndex>0,


If[ToString[Head[operator[["indices",1]]]]==ToString[Head[dummyFieldIndex]],
operatorDer = {};
operatorPref = {1}; 
indexReplacement = {operator[["indices",1]]-> dummyFieldIndex};

,
operatorDer = {{"dummyEntry"}}; 
	     operatorPref = {0}; 

];
,
If[ToString[Head[operator[["indices",1]]]]==ToString[dummyFieldIndex],
operatorDer = {};
operatorPref = {1}; 
indexReplacement = {operator[["indices",1]]-> dummyFieldIndex};

,
operatorDer = {{"dummyEntry"}}; 
	     operatorPref = {0}; 

];
];
,
_, 
operatorDer = {{"dummyEntry"}}; 
operatorPref = {0}; 
myEcho["no Der defined for this object",2];
];

Return[{operatorPref,operatorDer,indexReplacement}]
]


(* ::Input::Initialization:: *)
Clear[MatchDerIndices]
MatchDerIndices[field_,dummyindex_] := Module[{replacementList},
myEcho["MatchDerIndices",2];
replacementList = {dummyindex->field};
Return[replacementList]
]



(* ::Input::Initialization:: *)
Clear[MultiplyFactorsFuncDer]
MultiplyFactorsFuncDer[prefList_] := Module[{product = 1, metricfactors = {}},
myEcho[{"prefactor: ",prefList},3];
(Switch[Head[prefList[[#]]],

Integer, product = product* prefList[[#]];,
Rational, product = product* prefList[[#]];,
List, metricfactors = Join[metricfactors,{prefList[[#]]}];
])&/@Table[i,{i,1,Length@prefList}];
myEcho[{"multiplied prefactor: ",Join[{product},metricfactors]},3];
Return[Join[{product},metricfactors]]
]


(* ::Input::Initialization:: *)
Clear[FuncDer]
FuncDer[RHS_,field_] := Module[{dummyFieldIndex,AllDerivatives, FirstIndices, SecondIndices, ThirdIndices, dummyEntry, elements, elementsPref, PreviousPref, ThreePoint, SecondProp, nPoint, nP, ProductRuleNumber, RHSdummy = RHS,operatorPref,operatorDer,replacementList,indexReplacement2},


myEcho["FuncDer",1];

myEcho[{"Equation: ", RHS},2];


dummyFieldIndex = (*Unique[Global`iext]*)field;

AllDerivatives = Table[Null,Length@RHSdummy-1];     (*All Derivatives has the length of all Props/nPoints in the RHS expression*)


(*PreviousPref: This gives the (-1)^(field,field) factors in the prefactor from the permutation of fields in the derivative product rule*)

PreviousPref = Join[{{}},{{1}},Table[Null,Length@RHSdummy-2]];

(*change this according to the starting point which was figured out below*)

(If[RHSdummy[[#-1]][["type"]] =="Regulator"||RHSdummy[[#-1]][["type"]] =="Regulatordot"||(RHSdummy[[#-1]][["type"]] =="nPoint"&& RHSdummy[[#-1]][["spec"]] =="classical"),

PreviousPref[[#]] = PreviousPref[[#-1]];

,

PreviousPref[[#]] = Join[PreviousPref[[#-1]], Tuples[{RHSdummy[[#-1]][["indices"]],{dummyFieldIndex}}]];

])&/@Table[i,{i,3,Length@RHS}];             



(*Check for each master equation entry if prop or nPoint and find derivative of the object*)

(ProductRuleNumber = #;
myEcho[{"Product Rule No.: ", #},3];
{operatorPref,operatorDer,indexReplacement2} = DerRules[RHSdummy[[#]],dummyFieldIndex];

RHSdummy[[1,2]] = Join[RHS[[1,2]],operatorPref];

RHSdummy[[#]] = operatorDer;

RHSdummy = Join[{RHSdummy[[1]]},Flatten@RHSdummy[[2;;Length@RHSdummy]]];

If[Length@indexReplacement2>0,
RHSdummy = RHSdummy/.indexReplacement2;
];





If[MemberQ[{RHSdummy},{{"dummyEntry"}}],
Nothing,

RHSdummy[[1,2]] = Sort[Join[RHSdummy[[1,2]],PreviousPref[[#]]]];
];


AllDerivatives[[ProductRuleNumber-1]] = RHSdummy;

RHSdummy = RHS;
)&/@Table[i,{i,2,Length@RHSdummy}];


myEcho [{"All Derivatives: ",AllDerivatives},4];


replacementList = MatchDerIndices[field,dummyFieldIndex];

AllDerivatives = AllDerivatives/.{stuff___,"dummyEntry",furtherstuff___}->Nothing;

(AllDerivatives[[#,1,2]] = MultiplyFactorsFuncDer[AllDerivatives[[#,1,2]]])&/@Table[i,{i,1,Length@AllDerivatives}];



Return[{AllDerivatives, replacementList}]
]


(* ::Subsection:: *)
(*Main*)


(* ::Input::Initialization:: *)
Clear[MultipleFuncDer]
(*MultipleFuncDer[RHSlist_,fieldlist_] := Module[{truefieldlist, indexOptions},
indexOptions = fieldlist[["IndexStructure"]];
truefieldlist =fieldlist[["FieldDerivatives"]];

MultipleFuncDer[RHSlist,truefieldlist, {},indexOptions]
]*)
MultipleFuncDer[RHSlist_,fieldlist_] := MultipleFuncDer[RHSlist,fieldlist,{}]
MultipleFuncDer[RHSlist_,fieldlist_,replacementList_] := Module[{dummysuperindexlist,firstfield, newfieldlist, newRHSlist,newreplacementList},

myEcho["MultipleFuncDer",1];

firstfield = Last@fieldlist; (*replaced first with last and 1 with -1  to get correct order of derivatives*)
newfieldlist = Drop[fieldlist,-1];

(*check if its still the master eq or already a list of list of func ders*)
If[MemberQ[{RHSlist[[1]]}, "Prefactor"-> {__}],

myEcho["First FuncDer",2];
{newRHSlist,newreplacementList} = FuncDer[RHSlist,firstfield];

,

myEcho["Further FuncDer",2];

newRHSlist =Table[Null,{i,1,Length@RHSlist}];
newreplacementList= Table[Null,{i,1,Length@RHSlist}];

({newRHSlist[[#]],newreplacementList[[#]]} = FuncDer[RHSlist[[#]],firstfield])&/@Table[i,{i,1,Length@RHSlist}];

newRHSlist = Flatten[newRHSlist,1];
];

newreplacementList = Flatten@Join[replacementList, newreplacementList];

MultipleFuncDer[newRHSlist,newfieldlist,newreplacementList]
]
MultipleFuncDer[RHSlist_,{},newreplacementList_] := Module[{finalRHSlist},

finalRHSlist = (If[StringContainsQ[ToString[RHSlist[[#]]],"Field"],
Nothing,
RHSlist[[#]]
])&/@Range[Length@RHSlist];
Return[{finalRHSlist,newreplacementList}]
]


(* ::Section:: *)
(*Superindex Diagrams*)


(* ::Subsection:: *)
(*Get Fields and Misc*)


(* ::Input::Initialization:: *)
IndexProjection[iii_,jjj_] := Module[{},Return[iii->jjj]]


(* ::Input::Initialization:: *)
Clear[GetFieldsList]
GetFieldsList[fields_] := Module[{keys,bosons, fermions, BRSTs, BRSTbosons, BRSTfermions,fullList},
myEcho["GetFieldsList",1];
keys = Keys[fields];

If[MemberQ[keys,"bosonic"] == True,
bosons = Map[Head,fields[["bosonic"]]];
,
bosons = {};
];
If[MemberQ[keys,"fermionic"] == True,
fermions = Map[Head,fields[["fermionic"]],{2}];
,
fermions = {};
];
If[MemberQ[keys,"BRSTsources"]== True,

(Switch[fields[["BRSTsources",#,2]],
"bosonic", 
	bosons = Join[bosons,{fields[["BRSTsources",#,1]]}];,
"fermionic",
	fermions = Join[fermions,{fields[["BRSTsources",#,1]]}];
]
)&/@Range[Length@fields[["BRSTsources"]]];
,
Nothing
];
fullList = {bosons,fermions};
Return[fullList]
]


(* ::Subsection:: *)
(*Possible Configs*)


(* ::Input::Initialization:: *)
(*define the possible propagators for fields*) (*Adjust such that one can insert a truncation (done) and vertex ordering...*)
Clear[getPossibleProps]
getPossibleProps[fields_,truncation___]:= Module[{keys,bosonicfields,testbosonicfield, fermionicfields,PossibleProps = {},TruncatedPossibleProps = {}, PossibleBosonProps, PossibleFermionProps},

myEcho["GetPossibleProps",1];

keys = Keys[fields];

If[MemberQ[keys,"bosonic"] == True,
bosonicfields = Map[Head,fields[["bosonic"]]];
,
bosonicfields = {};
];
If[MemberQ[keys,"fermionic"] == True,
fermionicfields = Map[Head,fields[["fermionic"]],{2}];
,
fermionicfields = {};
];

PossibleBosonProps = Permutations[Join[bosonicfields,bosonicfields],{2}];
myEcho[{"Possible Boson Props",PossibleBosonProps},3];
PossibleFermionProps = Join[Tuples[{fermionicfields[[All,1]],fermionicfields[[All,2]]}],Map[Reverse,Tuples[{fermionicfields[[All,1]],fermionicfields[[All,2]]}],1]];
myEcho[{"Possible Fermion Props",PossibleFermionProps},3];


PossibleProps = Join[PossibleBosonProps,PossibleFermionProps];
        
(* if a Truncation is specified, the other props are removed *)
(* !!!!!!!!!!!!!!!!!!!!! does this still work?????!!!!!!!*)
If[Length@truncation>0,
myEcho["Truncate possible props",3];
(If[MemberQ[truncation,{OrderlessPatternSequence[PossibleProps[[#,1]],PossibleProps[[#,2]]]}],
TruncatedPossibleProps = Join[TruncatedPossibleProps,{PossibleProps[[#]]}];
,
Nothing
])&/@Range[Length@PossibleProps];
PossibleProps = TruncatedPossibleProps;
myEcho[{"poss props",PossibleProps},3];
,
Nothing
];
Return[PossibleProps];
]                                                                                       


(* ::Input::Initialization:: *)
(* get an assoc with the positions of objects *)
Clear[getAllObjectPositionsSuperindex]
getAllObjectPositionsSuperindex[RHSDiagram_]:= Module[{props ,regs, vertices, classical, brst,positionAssoc},
props = regs = vertices = classical= brst = {};

(Switch[RHSDiagram[[#,"type"]],
"Propagator",props =Join[props,{#}];,
"Regulator",regs=Join[regs,{#}];,
"Regulatordot",regs=Join[regs,{#}];,

"nPoint",
Switch[RHSDiagram[[#,"spec"]],
"none",vertices= Join[vertices,{#}];,
"classical",classical=Join[classical,{#}];,
"BRST", brst=Join[brst,{#}];
];
])&/@Range[2,Length@RHSDiagram,1];

positionAssoc = <|"Propagators" -> props, "Regulators" -> regs, "classicalVertices"-> classical,"BRSTVertices"-> brst, "Vertices"-> vertices|>;

Return[positionAssoc]
]                                                                                       


(* ::Input::Initialization:: *)
 (*get possible index configs for props and regs*) 
Clear[getObjectIndexMapping]
getObjectIndexMapping[diagram_,objectPositionAssoc_,Truncation_,indexConfigs_,objectType_,allfields_]:= Module[{objectSuperindices,possibleSets,allConfigs,appliedObjectSuperindices,fieldHeads,allConfigsnew,allIndexConfigs},

myEcho["getObjectIndexMapping",1];

(* get all object superindices *)
objectSuperindices = ((diagram[[#,"indices"]])&/@objectPositionAssoc[[objectType]]);


(* map superindices onto possible configs *)
If[Length@objectPositionAssoc[["Propagators"]]>0,

If[Length@objectSuperindices>0,

(* hard sum over all prop and reg configs and remove impossible ones *)
If[(objectType == "Regulators" ||objectType == "Propagators"),

allConfigs = getAllPossibleSets[objectSuperindices,Truncation];

(* apply previous index configs and remove impossible configs & check if one object index is already field and modify all Configs accordingly*)

If[allConfigs=={},

allIndexConfigs= {"Null"};
,

If[Length@indexConfigs>0,

appliedObjectSuperindices = objectSuperindices/.indexConfigs;

allIndexConfigs = Flatten[(checkHeads[appliedObjectSuperindices[[#]],objectSuperindices,allConfigs,indexConfigs[[#]],allfields])&/@Range[Length@indexConfigs],1];

,

allIndexConfigs = checkHeads[objectSuperindices,objectSuperindices,allConfigs,indexConfigs,allfields];

];

If[allIndexConfigs == {},
allIndexConfigs= {"Null"};
];

(*,

allIndexConfigs = indexConfigs;*)

];


,

(* after fixing the prop and reg indices, the vertex indices are automatically set and only need to be compared to the truncation *)

allConfigs = getPossibleVertexList[Truncation];

If[allConfigs=={},

allIndexConfigs= {"Null"};

,

appliedObjectSuperindices = objectSuperindices/.indexConfigs;



(* go through different vertex configs and then through each vertex *)



allIndexConfigs = ((compareAllVertexHeads[appliedObjectSuperindices[[#]],allConfigs,indexConfigs[[#]],allfields])&/@Range[Length@indexConfigs])/.{"Null"}->Nothing;


If[(*allConfigs*)allIndexConfigs=={},

allIndexConfigs= {"Null"};

];

];
];
];

,

(* If no props, ie DSE or LHSmSTI, then hard sum over vertices *)
allConfigs = getAllPossibleSets[objectSuperindices,Truncation];

If[allConfigs=={},

allIndexConfigs= {"Null"};
,

If[Length@indexConfigs>0,

appliedObjectSuperindices = objectSuperindices/.indexConfigs;

allIndexConfigs = Flatten[(checkHeads[appliedObjectSuperindices[[#]],objectSuperindices,allConfigs,indexConfigs[[#]],allfields])&/@Range[Length@indexConfigs],1];

,

allIndexConfigs = checkHeads[objectSuperindices,objectSuperindices,allConfigs,indexConfigs,allfields];

];

If[allIndexConfigs == {},
allIndexConfigs= {"Null"};
];

(*,

allIndexConfigs = indexConfigs;*)

];

];
Return[allIndexConfigs];
]


(* ::Input::Initialization:: *)
(* get all possible sets matching the length of the objects *)
Clear[getAllPossibleSets]
getAllPossibleSets[objectSuperindices_,Truncation_]:= Module[{iter,possibleSets = Table[Null,Length@objectSuperindices],allConfigs},

  For[iter = 1, iter<=Length@objectSuperindices, iter++,
possibleSets[[iter]] =  Select[Truncation,(Length[#]== Length@objectSuperindices[[iter]])&];
possibleSets[[iter]] = Union@Flatten[Permutations[possibleSets[[iter,#]]]&/@Range[Length@possibleSets[[iter]]],1];
];

allConfigs = Tuples[possibleSets];

allConfigs = Flatten[allConfigs[[#]],1]&/@Range[Length@allConfigs];

Return[allConfigs];
]                                                      


(* ::Input::Initialization:: *)
(* get all possible sets matching the length of the objects *)
Clear[getPossibleVertexList]
getPossibleVertexList[Truncation_]:= Module[{AllStringVertices},

AllStringVertices = (Map[ToString,Truncation[[#]]])&/@Range[Length@Truncation];

Return[AllStringVertices];
]                                                      


(* ::Input::Initialization:: *)
(* check if one object index is already fixed to a field and modify all Configs accordingly *)
Clear[checkHeads]
checkHeads[appliedObjectSuperindices_, objectSuperindices_,allConfigs_,indexConfigs_,allfields_]:= Module[{fieldHeads,allConfigsnew,allIndexConfigs},


fieldHeads = 
Switch[Head[Flatten[appliedObjectSuperindices][[#]]],

Symbol, (If[MemberQ[allfields,Flatten[appliedObjectSuperindices][[#]]],
{#,Flatten[appliedObjectSuperindices][[#]]},
Nothing]),
_, If[ToString[Head[Flatten[appliedObjectSuperindices][[#]]]] == "Q",

If[MemberQ[allfields,Flatten[appliedObjectSuperindices][[#,1]]],

{#,Flatten[appliedObjectSuperindices][[#]]}
,Nothing
]
,
{#,Head[Flatten[appliedObjectSuperindices][[#]]] }
]
]&/@Range[Length@Flatten[appliedObjectSuperindices]];

(* remove impossible configs *)


If[Length@fieldHeads>0,
	
	  allConfigsnew = (If[ToString[allConfigs[[#,fieldHeads[[All,1]]]]] == ToString[fieldHeads[[All,2]]],

allConfigs[[#]]

,

Nothing
]
)&/@Range[Length@allConfigs];


	If[Length@allConfigsnew>0,
allIndexConfigs = MapThread[IndexProjection,{Table[Flatten[objectSuperindices,2],Length@allConfigsnew],allConfigsnew},2];

If[Length@indexConfigs>0,
allIndexConfigs = Tuples[{allIndexConfigs,Table[indexConfigs,Length@allIndexConfigs]}];
allIndexConfigs = (Union[Flatten[allIndexConfigs[[#]],1]])&/@Range[Length@allIndexConfigs];

];
	,

allIndexConfigs = {};
];
,
allIndexConfigs = MapThread[IndexProjection,{Table[Flatten[objectSuperindices,2],Length@allConfigs],allConfigs},2];

If[Length@indexConfigs>0,
allIndexConfigs = Tuples[{allIndexConfigs,Table[indexConfigs,Length@allIndexConfigs]}];
allIndexConfigs = (Union[Flatten[allIndexConfigs[[#]],1]])&/@Range[Length@allIndexConfigs];

];
];

Return[allIndexConfigs];
]


(* ::Input::Initialization:: *)
(* compare each vertex in an indexConfigs with the truncation *)
Clear[compareAllVertexHeads]
compareAllVertexHeads[appliedObjectSuperindices_, allConfigs_,indexConfigs_,allfields_]:= Module[{fieldHeads,indexConfignew= indexConfigs,iter},



 (

indexConfignew = compareVertexHeads[appliedObjectSuperindices[[#]], allConfigs,indexConfignew,allfields]

)&/@Range[Length@appliedObjectSuperindices];


Return[indexConfignew];
]


(* ::Input::Initialization:: *)
(* compare the heads of vertices, as the fields are already set after setting the props and regs *)
Clear[compareVertexHeads]
compareVertexHeads[appliedObjectSuperindices_, allConfigs_,indexConfigs_,allfields_]:= Module[{fieldHeads,indexConfignew,iter},

If[indexConfigs == {"Null"},

Return[indexConfigs];
,


fieldHeads = 
Switch[Head[Flatten[appliedObjectSuperindices][[#]]],

Symbol, (If[MemberQ[allfields,Flatten[appliedObjectSuperindices][[#]]],
{#,Flatten[appliedObjectSuperindices][[#]]},
Nothing]),
_, If[ToString[Head[Flatten[appliedObjectSuperindices][[#]]]] == "Q",

If[MemberQ[allfields,Flatten[appliedObjectSuperindices][[#,1]]],

{#,Flatten[appliedObjectSuperindices][[#]]}
,Nothing
]
,
{#,Head[Flatten[appliedObjectSuperindices][[#]]] }
]
]&/@Range[Length@Flatten[appliedObjectSuperindices]];
fieldHeads = fieldHeads[[All,2]];

(* compare every single vertex to truncation and remove impossible configs *)

If[Length@fieldHeads>0,
	
	  If[ MemberQ[allConfigs,{Apply[OrderlessPatternSequence,(Map[ToString,fieldHeads])]}],

indexConfignew = indexConfigs;
,

indexConfignew = {"Null"};
];
];

Return[indexConfignew];
];
]


(* ::Input::Initialization:: *)
Clear[getBRSTTruncation]
getBRSTTruncation[truncation_] := Module[{iter,brstTruncation={}},

  For[iter = 1, iter<=Length@truncation, iter++,
If[MemberQ[StringMatchQ[Map[ToString,truncation[[iter]]],"Q*"],True],
brstTruncation =  Append[brstTruncation,truncation[[iter]]]
]
];

Return[brstTruncation]
]


(* ::Subsection::Closed:: *)
(*Sort Objects*)


(* ::Input::Initialization:: *)
(* split field object list into lists such that each field only appears one per list *)
Clear[SplitIndexList];
SplitIndexList[listRight_]:=SplitIndexList[{},listRight]
SplitIndexList[{},listRight_]:=SplitIndexList[{{First[listRight]}},Drop[listRight,1]]
SplitIndexList[listsLeft_,listRight_]:=Module[{newLeftPart},
newLeftPart=If[
MemberQ[Last[listsLeft][[All,1]],First[listRight][[1]]],
listsLeft~Join~{{First[listRight]}},Drop[listsLeft,-1]~Join~{Append[Last[listsLeft],First[listRight]]}
];
SplitIndexList[newLeftPart,Drop[listRight,1]]
]
SplitIndexList[listsLeft_,{}]:=listsLeft


(* ::Input::Initialization:: *)
Clear[OrderByFieldTypes]
OrderByFieldTypes[testList_,sign_,fields_] := Module[{testfield,positionTestfield,dummysign = sign,jointfermionicList = {}, jointantifermionicList = {},signold, signnew,newTestList},

myEcho["OrderByFieldTypes",1];


(* sorts for antifermions then fermions with correct sign *)
(testfield = testList[[#]];

If[MemberQ[Map[Head,fields[["fermionic",All,1]]],testfield[[1]]],

	jointantifermionicList = Join[jointantifermionicList,{testfield}];
	signnew = dummysign*(-1)^(Length@jointfermionicList);
,

	jointfermionicList = Join[jointfermionicList,{testfield}];
	signnew = dummysign;
];
dummysign = signnew;
)&/@Range[Length@testList];

(* needs to be tested but should in principle work =) !!!!!!!!!!!! *)

signold = Signature[jointfermionicList]*Signature[jointantifermionicList];
signnew = dummysign*signold*Signature[Sort[jointfermionicList]]*Signature[Sort[jointantifermionicList]];
myEcho[{"sign and new ordering: ", signnew, testList, Join[{jointantifermionicList},{jointfermionicList}]},3];

(*!!!!!!!!!!!!!!!!!!*)

Return[{signnew, Join[{jointantifermionicList},{jointfermionicList}]}]
]


(* ::Input::Initialization:: *)
Clear[JoinSplitLists];  (*canonical ordering of list is af, f, BRSTf*) 
JoinSplitLists[jointList_,list_,sign_,fields_]:= Module[{positionJointList, newJointList, signnew,correctpos,reducedList, member},

myEcho[{"list: ", list},3];

If[MemberQ[(Flatten[jointList,1])[[All,1]],First[list][[1]]]==True,
(* if the same field is already in the first joint list, just append at the corresponding position *)
myEcho["field already in joint list",3];


positionJointList = Last[Position[jointList, First[list][[1]]]];  
newJointList = Insert[jointList, First[list],{positionJointList[[1]],positionJointList[[2]]+1}];

signnew = sign*(-1)^(Length@(Flatten[newJointList,1][[All,1]])-Last[Position[Flatten[newJointList,1][[All,1]],First[list][[1]]]][[1]]);

,

(* if not, then insert it into an empty list at the corresponding position *)
myEcho["field not yet in joint list",3];


{signnew,newJointList} = OrderByFieldTypes[{First[list]},sign,fields];

myEcho["check how the lists need to be joined",3];
myEcho[{"joint List: ",jointList},3];
myEcho[{"new joint List: ", newJointList},3];

If[Length@jointList==1,

(*check if the single list is a fermion or antifermion*)
If[MemberQ[Map[Head,fields[["fermionic",All,1]]],Map[Head,jointList[[1,1,1]]]],
myEcho["single antifermion in list",3];
newJointList = MapThread[Join,{Join[jointList,{{}}],newJointList}];
];
If[MemberQ[Map[Head,fields[["fermionic",All,2]]],Map[Head,jointList[[1,1,1]]]]||MemberQ[fields[["BRSTsource",All,1]],Map[Head,jointList[[1,1,1]]]],
myEcho["single fermion in list",3];
newJointList = MapThread[Join,{Join[{{}},jointList],newJointList}];
]
,
newJointList = MapThread[Join,{jointList,newJointList}];
];

(* insert the field in correct alphabetical order *)

correctpos = Position[{Sort[newJointList[[1,All,1]]],Sort[newJointList[[2,All,1]]]},First[list][[1]]][[1]];

signnew = signnew*(-1)^(Length@Flatten[newJointList,1][[All,1]]-First[Position[Flatten[Map[Sort,newJointList],1][[All,1]],First[list][[1]]]][[1]]);


If[Length@jointList==1,

myEcho["list of length 1",3];
(*check if the single list is a fermion or antifermion*)
If[MemberQ[Map[Head,fields[["fermionic",All,1]]],Map[Head,jointList[[1,1,1]]]],
myEcho["single antifermion in list",3];
newJointList = Insert[Join[jointList,{{}}],First[list],correctpos];
];
If[MemberQ[Map[Head,fields[["fermionic",All,2]]],Map[Head,jointList[[1,1,1]]]]||MemberQ[fields[["BRSTsource",All,1]],Map[Head,jointList[[1,1,1]]]],
myEcho["single fermion in list",3];
newJointList = Insert[Join[{{}},jointList],First[list],correctpos];
]
,
myEcho["list of more than one field",3];
newJointList = Insert[jointList,First[list],correctpos];
];

];
reducedList = Drop[list,1];
JoinSplitLists[newJointList, reducedList, signnew,fields]
]
JoinSplitLists[list_,{}, sign_,fields_] := {sign,list}


(* ::Input::Initialization:: *)
(* merge a list of fermionic lists where each field only appears once into one list with sorted fields *)
Clear[MergeIndexLists]; (*superlist = list of lists*)
MergeIndexLists[superList_,sign_,fields_]:= Module[{signnew, newList},
(* sort first sublist in field type ordering (+alphabetical order) *)
{signnew, newList} = OrderByFieldTypes[First[superList],sign,fields];
MergeIndexLists[newList,Drop[superList,1],signnew,fields]
];
MergeIndexLists[list_,superList_,sign_,fields_] := Module[{newFirstList, newRestSuperList,signnew},
(* join the sublists *)

{signnew,newFirstList} = JoinSplitLists[list,First[superList],sign,fields];

newRestSuperList = Drop[superList,1];
MergeIndexLists[newFirstList,newRestSuperList,signnew,fields]
]
MergeIndexLists[list_,{},sign_,fields_]:={sign,Flatten[list,1]}


(* ::Input::Initialization:: *)
(*orders all vertices in the nPoints in the default order and adjusts the signs of the diagrams*)
Clear[DefaultVertexOrdering]; 
DefaultVertexOrdering[objectIndices_,onecombinedIndexConfigs_,fields_,truncation_,classicalAction___] :=Module[{allfields,fieldHeads,superindices,bosonicList = {}, fermionicList = {}, antifermionicList = {},allfermionList = {}, positionTestfield, testfield, signFermionsStart,signFermionsIntermediate,signofSort,splitAllFermionList,mergedFermionsList, signFermionsEnd, newObjectIndices},

allfields =Union[Flatten[Join[truncation,classicalAction]]];


myEcho["DefaultVertexOrdering",1];


fieldHeads = (
Switch[Head[Flatten[objectIndices][[#]]],
Symbol, (If[MemberQ[allfields,Flatten[objectIndices][[#]]],
{Flatten[objectIndices/.onecombinedIndexConfigs][[#]],List@@objectIndices[[#,All]]},
{Flatten[objectIndices/.onecombinedIndexConfigs][[#]],{objectIndices[[#]]}}]),

_, If[ToString[Head[Flatten[objectIndices][[#]]]] == "Q",

{Flatten[objectIndices/.onecombinedIndexConfigs][[#]],List@@objectIndices[[#]]}
,
{Head[Flatten[objectIndices][[#]]],List@@objectIndices[[#]]}
]])&/@Range[Length@Flatten[objectIndices/.onecombinedIndexConfigs]];



superindices = fieldHeads;
(*sort for bosons and xfermions*)

(testfield = superindices[[#]];
If[MemberQ[Map[Head,fields[["bosonic"]]],testfield[[1]]],
bosonicList = Sort@Join[bosonicList,{testfield}];
];
If[MemberQ[Map[Head,fields[["fermionic",All,1]]],testfield[[1]]],
antifermionicList = Sort@Join[antifermionicList,{testfield}];
allfermionList = Join[allfermionList,{testfield}];
];
If[MemberQ[Map[Head,fields[["fermionic",All,2]]],testfield[[1]]],
fermionicList = Sort@Join[fermionicList,{testfield}];
allfermionList = Join[allfermionList,{testfield}];
];

If[MemberQ[fields[["BRSTsources",All,1]],testfield[[1]]],
positionTestfield = First@Flatten@Position[fields[["BRSTsources",All,1]],testfield[[1]]];
Switch[fields[["BRSTsources",positionTestfield,2]],
"bosonic",
	bosonicList = Sort@Join[bosonicList,{testfield}];,
"fermionic", 
	fermionicList = Sort@Join[fermionicList,{testfield}];
	allfermionList = Join[allfermionList,{testfield}];,
"antifermionic",
	antifermionicList = Sort@Join[antifermionicList,{testfield}];
	allfermionList = Join[allfermionList,{testfield}];
];
];

)&/@Range[Length@superindices];



If[Length@allfermionList == 0,
myEcho["no fermions",3];
signofSort = 1;

newObjectIndices = bosonicList;
,
If[Signature[allfermionList[[All,1]]]!= 0,
(* if each fermionic field only appears once, the ordering and sign are straight forward *)


(* !!!!!!!!!!!!!!!!!!!!!!!!!!the sign might still be wrong!!!!!!!!!!!!!!!!!!!!!!!! *)

myEcho["each fermion max once",3];
signFermionsStart = Signature[allfermionList[[All,1]]];


newObjectIndices = Flatten[{bosonicList,antifermionicList,fermionicList},1];

If[Length@antifermionicList >0,
If[Length@fermionicList>0,

signofSort = signFermionsStart * Signature[Join[antifermionicList[[All,1]],fermionicList[[All,1]]]];

,
signofSort = signFermionsStart * Signature[antifermionicList[[All,1]]];
];
,
signofSort = signFermionsStart * Signature[fermionicList[[All,1]]];
];
,
(* for multiple fermionic fields of the same type in one list we need to split it up to get the correct sign and then merge it again *)


myEcho["multiple similar fermions",3];

splitAllFermionList = SplitIndexList[allfermionList];

myEcho[{"split fermion list: ",splitAllFermionList},3];
(*signFermionsStart = Apply[Times,Map[Signature,splitAllFermionList[[All,All,1]]]];
myEcho[{"sign ferm start: ",signFermionsStart},3];*)

{signFermionsEnd,mergedFermionsList} = MergeIndexLists[splitAllFermionList,(*signFermionsStart*)1,fields];

signofSort = signFermionsEnd;
myEcho[{"sign fermions end: ",signofSort},3];
newObjectIndices = Join[bosonicList, mergedFermionsList];
];
];
Return[{signofSort,newObjectIndices}]
] 


(* ::Input::Initialization:: *)
Clear[DefaultPropOrdering]; 
DefaultPropOrdering[propIndices_,onecombinedIndexConfigs_,fields_,truncation_,classicalAction___] :=Module[{allfields,fieldHeads,superindices,testfield,positionTestfield,bosonicList= {}, fermionicList= {}, antifermionicList = {}, newsign, newPropIndices,signFermionsStart},

myEcho["DefaultPropOrdering",3];
allfields =Union[Flatten[Join[truncation,classicalAction]]];

fieldHeads = (
Switch[Head[Flatten[propIndices][[#]]],
Symbol, (If[MemberQ[allfields,Flatten[propIndices][[#]]],

{Flatten[propIndices/.onecombinedIndexConfigs][[#]],List@@propIndices[[#,All]]},

{Flatten[propIndices/.onecombinedIndexConfigs][[#]],{propIndices[[#]]}}]),

_, {Flatten[propIndices/.onecombinedIndexConfigs][[#]],List@@propIndices[[#,All]]}
])&/@Range[Length@Flatten[propIndices/.onecombinedIndexConfigs]];



superindices = fieldHeads;

(*sort for bosons and xfermions*)

(testfield = superindices[[#]];

If[MemberQ[Map[Head,fields[["bosonic"]]],testfield[[1]]],
myEcho["boson",3];
bosonicList = Sort@Join[bosonicList,{testfield}];
];
If[MemberQ[Map[Head,fields[["fermionic",All,1]]],testfield[[1]]],
myEcho["antifermion",3];
 antifermionicList = Join[antifermionicList,{testfield}];
];
If[MemberQ[Map[Head,fields[["fermionic",All,2]]],testfield[[1]]],
myEcho["fermion",3];
fermionicList = Join[fermionicList,{testfield}];
];
)&/@{1,2};



If[Length@fermionicList == 0,   (* get sign for bosonic or fermionic prop *)
myEcho["bosonic prop",5];
newsign = 1;
newPropIndices = bosonicList;
,
myEcho["fermionic prop",5];

signFermionsStart = Signature[superindices];

newPropIndices =Flatten[Transpose[{fermionicList,antifermionicList}],1];

newsign = signFermionsStart * Signature[newPropIndices];

];

Return[{newsign,newPropIndices}]
] 


(* ::Input::Initialization:: *)
Clear[SortObject] (* sorts one general object *)

SortObject[objecttype_,objectIndices_,onecombinedIndexConfigs_, fields_,truncation_,classicalAction___] := Module[{sign,sortedIndices = Table[Null,Length@objectIndices], newObjectIndices = objectIndices},

myEcho[{"old ordering: ", objectIndices},4];

Switch[objecttype,
"Propagators",{sign,sortedIndices} = DefaultPropOrdering[objectIndices,onecombinedIndexConfigs, fields,truncation,classicalAction];,
"Regulators", {sign,sortedIndices} = DefaultVertexOrdering[objectIndices,onecombinedIndexConfigs, fields,truncation,classicalAction];,
"Vertices",{sign,sortedIndices} = DefaultVertexOrdering[objectIndices,onecombinedIndexConfigs, fields,truncation,classicalAction];,
"classicalVertices",{sign,sortedIndices} = DefaultVertexOrdering[objectIndices,onecombinedIndexConfigs, fields,truncation,classicalAction];,
"BRSTVertices",{sign,sortedIndices} = DefaultVertexOrdering[objectIndices,onecombinedIndexConfigs, fields,truncation,classicalAction];
];


myEcho[{"vertex/prop ordering yields: ", sign, objectIndices,sortedIndices},3];

Return[{sign, sortedIndices}];
] 


(* ::Input::Initialization:: *)
Clear[SortOneDiag] (*Sorts indices such that they match the sorted vertices/props....*)
SortOneDiag[RHSDiagram_,oneIndexConfig_,fields_,objectPositionAssoc_,truncation_,classicalAction___] := Module[{iter,newsign = 1, objectIndices,newObjectIndices ,dummysign, totalsign,sortedDiagram = RHSDiagram},

myEcho["SortOneDiag",1];

(If[Length@(objectPositionAssoc[[#]])>0,
objectIndices = ((RHSDiagram[[#,"indices"]])&/@objectPositionAssoc[[#]]);

newObjectIndices = objectIndices;
If[Length@(objectPositionAssoc[[#]])>1,


For[iter = 1, iter<=Length@(objectPositionAssoc[[#]]),iter++,

{dummysign, newObjectIndices[[iter]]} = SortObject[#,objectIndices[[iter]],oneIndexConfig, fields, truncation, classicalAction];

newsign = newsign*dummysign;

];

,

{dummysign, newObjectIndices} = SortObject[#,objectIndices[[1]],oneIndexConfig, fields, truncation, classicalAction];

newsign = newsign*dummysign;

];

sortedDiagram[[objectPositionAssoc[[#]],"indices"]] = newObjectIndices;
];
)&/@(objectPositionAssoc//Keys);
sortedDiagram[[1,2]] = Join[RHSDiagram[[1,2]],{newsign}]//Sort;

Return[sortedDiagram];
] 


(* ::Input::Initialization:: *)
Clear[SortDiags] (*Sorts vertices and props of one diagram type for multiple index configs....*)
SortDiags[RHSDiagram_,IndexConfigs_,fields_,objectPositionAssoc_,truncation_,classicalAction___] := Module[{sortedDiag, appliedDiag},

If[Length@IndexConfigs >1,
sortedDiag = Table[Null,Length@IndexConfigs];
appliedDiag = Table[Null,Length@IndexConfigs];
(
sortedDiag[[#]] = SortOneDiag[RHSDiagram,IndexConfigs[[#]],fields,objectPositionAssoc,truncation,classicalAction];
appliedDiag[[#]] = sortedDiag[[#]];

appliedDiag[[#,1,2]]= GetSign[appliedDiag[[#,1]]/.IndexConfigs[[#]],fields];
)&/@Range[Length@IndexConfigs];
,
sortedDiag = SortOneDiag[RHSDiagram,IndexConfigs,fields,objectPositionAssoc,truncation,classicalAction];
appliedDiag = {sortedDiag};

appliedDiag[[1,1,2]]= GetSign[appliedDiag[[1,1]]/.IndexConfigs[[1]],fields];
];

Return[appliedDiag];
] (* we want to get the swapped superindices as well as the possible configs as a list *)


(* ::Subsection::Closed:: *)
(*Signs*)


(* ::Input::Initialization:: *)
(* multiply all the numbers in the prefactor of a single diagram *)
Clear[GetSign] 
GetSign[appliedDiagPref_,fields_] := Module[{newappliedDiagPref = appliedDiagPref[[2]],fermionicBRST,testfield1, testfield2, positionTestfield1, positionTestfield2, totalDiagPref},

myEcho["GetSign",1];
If[MemberQ[Keys[fields],"BRSTsources"],
fermionicBRST = Flatten@(fields[["BRSTsources",#,1]]&/@Position[fields[["BRSTsources"]][[All,2]],"fermionic"]);
,
fermionicBRST = {};
];

(
Switch[Head[newappliedDiagPref[[#]]],
Integer,
		myEcho["Integer",4];
		 Nothing,
Rational,
		myEcho["Rational",4];
		 Nothing,
List,
		myEcho["Metric factor",4];
		testfield1 =  newappliedDiagPref[[#,1]];
		testfield2 =  newappliedDiagPref[[#,2]];
		
		
		If[(MemberQ[Join[fermionicBRST,Map[Head,Flatten[fields[["fermionic"]],1]]],testfield1]&&MemberQ[Join[fermionicBRST,Map[Head,Flatten[fields[["fermionic"]],1]]],testfield2]),
			myEcho["both fermions",4];

			newappliedDiagPref[[#]] = -1;
			,

			myEcho["one or no fermion",4];
			newappliedDiagPref[[#]] = 1;
		];
];
)&/@Range[Length@newappliedDiagPref];

totalDiagPref = Apply[Times,newappliedDiagPref];

Return[{totalDiagPref}];
]


(* ::Subsection:: *)
(*Trace over Fields*)


(* ::Input::Initialization:: *)
Clear[TraceOverFields]
TraceOverFields[RHS_,derivativelist_,replacementList_, fields_,truncation_,classicalAction___] := Module[{internalreplacementList,objectPositionAssoc,brstTruncation,truncationnew,tracedDiags},

myEcho["GetTraceOverFields",1];

(* replace dummy indices with external fields *)
internalreplacementList = MapThread[IndexProjection,{derivativelist,Map[Head,derivativelist]}];
objectPositionAssoc = getAllObjectPositionsSuperindex[RHS[[1]]];
If[Length@objectPositionAssoc[["BRSTVertices"]]>0,
brstTruncation = getBRSTTruncation[truncation];
truncationnew =Select[truncation,!MemberQ[brstTruncation,#]&];
, 
brstTruncation = {};
truncationnew = truncation;
];

(* take field trace diag-wise *)
tracedDiags = 
(traceSingleDiagram[RHS[[#]],derivativelist,internalreplacementList,fields, truncationnew,brstTruncation,classicalAction])&/@Range[Length@RHS];

Return[Flatten[tracedDiags/.{"dummyEntry"}->Nothing,1]]
]


(* ::Input::Initialization:: *)
Clear[traceSingleDiagram];
traceSingleDiagram[RHSDiagram_,derivativelist_,replacementList_,fields_,truncation_,brstTruncation_,classicalAction___] := Module[{allfields,objectPositionAssoc, truncationObject,IndexConfigs = {},sortedDiag},


myEcho["GetSingleDiagram",1];

(* get positions of all object types *)
objectPositionAssoc = getAllObjectPositionsSuperindex[RHSDiagram];

allfields = Union@Flatten[Join[truncation,brstTruncation,classicalAction]];

(* check if there are "objects" in diag and get index mapping configs of "object" indices onto possible "object configs" *)
(If[Length@(objectPositionAssoc[[#]])>0 ,

If[Length@IndexConfigs==0,
If[# == "classicalVertices",
					truncationObject = classicalAction;

					,

				truncationObject = truncation];
If[ # =="BRSTVertices",

truncationObject = brstTruncation;

					,

				truncationObject = truncation];
				
	IndexConfigs =getObjectIndexMapping[RHSDiagram,objectPositionAssoc,truncationObject,IndexConfigs,#,allfields];
		
,
If[StringMatchQ[ToString[IndexConfigs[[1]]],"Null"],
Nothing
,

		If[# == "classicalVertices",
		truncationObject = classicalAction;

		,

		truncationObject = truncation];
If[ # =="BRSTVertices",

truncationObject = brstTruncation;

					,

				truncationObject = truncation];


		
		IndexConfigs =getObjectIndexMapping[RHSDiagram,objectPositionAssoc,truncationObject,IndexConfigs,#,allfields];
		
];
];
];)&/@(objectPositionAssoc//Keys);




(*sort the objects in default ordering*)

If[StringMatchQ[ToString[IndexConfigs[[1]]], {"Null"}], 
myEcho["no possible configs for this diagram",2];

Return[{"dummyEntry"}]
];
IndexConfigs = MapThread[Join,{IndexConfigs,Table[replacementList,Length@IndexConfigs]}];
myEcho["sort possible configs for this diagram",2];

sortedDiag = SortDiags[RHSDiagram,IndexConfigs,fields,objectPositionAssoc,truncation,classicalAction];

Return[sortedDiag]


];


(* ::Input::Initialization:: *)
Clear[SwitchSuperindexReplacementList]
SwitchSuperindexReplacementList[replacementList_] := Module[{superindexReplacementList = Table[Null,Length@replacementList]},

(
superindexReplacementList[[#]] = IndexProjection[replacementList[[#,1]],List@@replacementList[[#,2]]];
)&/@Range[Length@replacementList];

Return[superindexReplacementList]
]


(* ::Section::Closed:: *)
(*Full Diagrams*)


(* ::Subsection::Closed:: *)
(*Aux Functions*)


(* ::Input::Initialization:: *)
Clear[FindFirstVertex]
FindFirstVertex[diag_, firstsuperindex_] := Module[{posFirstVertex,pattern,casesFirstVertex},
myEcho["FindFirstVertex",3];

pattern = <|"type"->"nPoint","indices"->{___,{___,firstsuperindex},___},__|>;

casesFirstVertex = Cases[diag,pattern];
If[Length@casesFirstVertex>0,
posFirstVertex = Position[diag,Cases[diag,pattern][[1]]][[1,1]];
Echo[posFirstVertex];
myEcho[{"Position of first vertex",posFirstVertex},3];
Return[posFirstVertex]
];
]


(* ::Input::Initialization:: *)
 (* gives the correct number of indices for the prop fields *)
Clear[SuperindexRules]; 
SuperindexRules[{{field1_,a_},{field2_,b_}},fields_] := Module[{positionField1,superindices1,superindices2,indicesNew},

myEcho["Superindex Rules",3];


positionField1 = Position[Flatten@Join[Map[Head,fields[["bosonic",All]]],Map[Head,Flatten@fields[["fermionic",All]]]],field1][[1,1]];
superindices1 = Map[Unique,List@@(Flatten[Join[fields[["bosonic",All]],fields[["fermionic",All]]],1][[positionField1]])];superindices2 = Map[Unique,List@@(Flatten[Join[fields[["bosonic",All]],fields[["fermionic",All]]],1][[positionField1]])];

indicesNew = (*{{field1,superindices1},{field2,superindices2}}*){superindices1,superindices2};

Return[indicesNew]
]


(* ::Input::Initialization:: *)
Clear[BRSTSuperindexRules]; (*needs to be adjusted for fermions/ghosts \[Rule] lorentz indices*)
BRSTSuperindexRules[{BRSTfield_,a_},fields_] := Module[{positionField,superindices,indicesNew},


positionField = Position[Flatten@Join[Map[Head,fields[["bosonic",All]]],Map[Head,Flatten@fields[["fermionic",All]]]],BRSTfield[[1]]][[1,1]];
superindices = Map[Unique,List@@(Flatten[Join[fields[["bosonic",All]],fields[["fermionic",All]]],1][[positionField]])];

indicesNew = {superindices};

Return[indicesNew]
]


(* ::Input::Initialization:: *)
Clear[FindPositionNewMomentum]
FindPositionNewMomentum[allmomenta_,externalMomenta_,signRules_,loopMom_] := Module[{iter,posNewMom,memberList},
myEcho["Find Position New",5];

For[iter = 1, iter <=Length@allmomenta, iter++,
	myEcho[{"check index if new momentum: ", allmomenta[[iter]]},5];
	If[MemberQ[Join[Map[List,externalMomenta],{{loopMom}}],{allmomenta[[iter]]}/.signRules],
		posNewMom = {};
		myEcho["no new mom",5];
		,
	
		If[Length@((Flatten[List@@@{allmomenta[[iter]]}]/.signRules)/.n_?NumericQ->Nothing)==1,
			myEcho["only one mom in this entry",5];
			posNewMom = iter;
			myEcho[{"newmom", allmomenta[[iter]], posNewMom},5];
			iter = Length@allmomenta+1;
			,
			myEcho["more than one mom in this entry",5];
			memberList = MemberQ[Join[Map[List,externalMomenta],{{loopMom}}],{allmomenta[[iter,#]]}/.signRules]&/@Table[i,{i,1,Length@allmomenta[[iter]]}];
			
			If[AllTrue[memberList,TrueQ],
				myEcho["all mom are external",5];
				posNewMom = {};
				,
				myEcho["find out position of false",5];
				posNewMom = {iter,Position[(MemberQ[Join[Map[List,externalMomenta],{{loopMom}}],{allmomenta[[iter,#]]}/.signRules]&/@Table[i,{i,1,Length@allmomenta[[iter]]}]),False][[1,1]]};
				iter = Length@allmomenta+1;
			];
		];
		myEcho[{"posNewMom",posNewMom},5];
	];
];

Return[posNewMom]
]


(* ::Input::Initialization:: *)
Clear[FixMomentum]
FixMomentum[object_,posNewMom_,allmomenta_,fullDeltaList_,signRules_] := Module[{allmomentanew,newVar, prefVar,momentumCons, newfullDeltaList = fullDeltaList},
myEcho["Fix Momentum",5];


If[Length@Flatten[{posNewMom}]==1,

myEcho["only one new mom",5];

allmomentanew = Drop[allmomenta,{posNewMom}];

newVar = Evaluate[object[["indices",posNewMom,2,1]]/.fullDeltaList]/.signRules;
prefVar =Evaluate[object[["indices",posNewMom,2,1]]/.fullDeltaList]/newVar;




momentumCons = Flatten@Solve[(prefVar*newVar + Total[allmomentanew]/.fullDeltaList)==0,newVar];


newfullDeltaList = Join[Evaluate[fullDeltaList/.momentumCons],momentumCons];


,

If[Length@Flatten[{posNewMom}]==2,

myEcho["more than one new mom",5];

allmomentanew = Delete[allmomenta,{posNewMom}];

newVar = (Evaluate[object[["indices",2,All,2,1]]/.fullDeltaList]/.signRules)[[posNewMom[[1]],posNewMom[[2]]]];

prefVar =((Evaluate[object[["indices",2,All,2,1]]/.fullDeltaList])[[posNewMom[[1]],posNewMom[[2]]]]/newVar);

momentumCons = Flatten@Solve[(prefVar*newVar + Total[allmomentanew]/.fullDeltaList)==0,newVar];

newfullDeltaList = Join[Evaluate[fullDeltaList/.momentumCons],momentumCons];

,
Nothing
];
];
Return[newfullDeltaList]
]


(* ::Input::Initialization:: *)
(* somehow it also fixes external momenta...*)

Clear[MomentumConservationVert]
MomentumConservationVert[diag_,replacementList_,positionAllVertices_,loopMom_] := Module[{newReplList = replacementList,externalMomenta,signRules,positionReg,positionFirstVert, irun,rightLength, leftLength,direction = 0, newVar, prefVar,posNewMom,RunningIndex,MemberList,FullDeltaList = {},MomentumCons = {}, directionTable, allMomenta},

newReplList[[All,2]] = ((List@@(replacementList[[#,2]]))&/@Table[i,{i,1,Length@replacementList}])(*[[All,1]]*);


signRules =  {n_?NumericQ*q_ -> loopMom};

externalMomenta = replacementList[[All,2,1]];


(* check how long it is to get to the reg from the first vertex clockwise and counter-clockwise *)

(* note that the position of the reg is always at one *)
positionReg = 1;



(* check for single tadpole diagram*)
If[Length@(Union[positionAllVertices])==1,

If[MemberQ[Join[Evaluate[-diag[[positionAllVertices[[1]]+1,"indices",All,2,1]]],diag[[positionAllVertices[[1]]+1,"indices",All,2,1]]],diag[[positionAllVertices[[1]],"indices",#,2,1]]],    
  
		newVar = diag[[positionAllVertices[[1]],"indices",#,2,1]];

	]&/@Range[Length@diag[[positionAllVertices[[1]],"indices",All,2]]];
];


For[irun=1,irun<=Length@positionAllVertices,irun++,

positionFirstVert = positionAllVertices[[irun]];

       leftLength = positionFirstVert-positionReg;
rightLength =  Length@diag- positionFirstVert+positionReg;

myEcho[{leftLength,rightLength},4];
Echo[{leftLength,rightLength}];
If[rightLength<leftLength,

(* check which index connects to the right *)
direction = +1;
If[MemberQ[Join[Evaluate[-diag[[positionFirstVert+1,"indices",All,2,1]]],diag[[positionFirstVert+1,"indices",All,2,1]]],diag[[positionFirstVert,"indices",#,2,1]]],    
  
		newVar = diag[[positionFirstVert,"indices",#,2,1]];

	]&/@Table[i,{i,1,Length@diag[[positionFirstVert,"indices",All,2]]}];

irun = Length@positionAllVertices;
,
If[leftLength<rightLength,

(* check which index connects to the left *)
direction = -1;
If[MemberQ[Join[Evaluate[-diag[[positionFirstVert-1,"indices",All,2,1]]],diag[[positionFirstVert-1,"indices",All,2,1]]],diag[[positionFirstVert,"indices",#,2,1]]],    
  
		newVar = diag[[positionFirstVert,"indices",#,2,1]];

	]&/@Table[i,{i,1,Length@diag[[positionFirstVert,"indices",All,2]]}];

irun = Length@positionAllVertices
,
direction = +1;


];
];
(* else leftLength = rightLength: go to second vertex, etc*)
];


myEcho[{"momentum which connects to the first vertex is: ",newVar},4];



prefVar = newVar/(newVar/.signRules);

	If[prefVar>0,
		MomentumCons = Flatten@Solve[newVar+loopMom==0,newVar];
		,
		MomentumCons = Flatten@Solve[newVar+loopMom==0,prefVar*newVar]
];





FullDeltaList = Join[Evaluate[FullDeltaList/.MomentumCons],MomentumCons];
myEcho[{"Delta List of momenta: ",FullDeltaList},4];


(* go through the diagram in direction, starting from the first vertex *)
If[direction ==1,
directionTable  = Join[Table[i,{i,positionFirstVert+1,Length@diag}],Table[i,{i,1,positionFirstVert-1}]];
,
directionTable = Join[Table[i,{i,positionFirstVert-1,1,-1}],Table[i,{i,Length@diag,positionFirstVert+1,-1}]];
];



(
myEcho[{"check object no. ",#},4];
(*Echo[#];*)

allMomenta = diag[[#,"indices",All,2,1]]/.FullDeltaList;
myEcho[{"all momenta in the object which is momentum fixed right now",allMomenta},4];
(* find out position of new momentum which is not yet fixed *)
myEcho[{"here.............................",allMomenta,externalMomenta,signRules,loopMom},6];
posNewMom = FindPositionNewMomentum[allMomenta,externalMomenta,signRules,loopMom];

myEcho[{"here.............................",diag[[#]],posNewMom,allMomenta,FullDeltaList,signRules},6];
FullDeltaList = FixMomentum[diag[[#]],posNewMom,allMomenta,FullDeltaList,signRules];
(*Echo[FullDeltaList];*)
)&/@directionTable;


Return[FullDeltaList]
]


(* ::Input::Initialization:: *)
(* somehow it also fixes external momenta...*)

Clear[MomentumConservationBRST]
MomentumConservationBRST[diag_,replacementList_,positionFirstVert_,allProps_,loopMom_] := Module[{newReplList = replacementList,externalMomenta,signRules,diagfieldindices, posBRSTfield,BRSTcheck,newVar, prefVar,posNewMom,startingPoint,MemberList,FullDeltaList = {},MomentumCons = {}, allMomenta},

newReplList[[All,2]] = ((List@@(replacementList[[#,2]]))&/@Table[i,{i,1,Length@replacementList}])(*[[All,1]]*);


signRules =  {n_?NumericQ*q_ ->loopMom};

externalMomenta = replacementList[[All,2,1]];




(* check if there is any loop? *)
If[Length@allProps>0,
(* get the BRST momentum and set it to -q *)
(
(*Echo[#];*)

diagfieldindices = diag[[#,"indices",All,1]];
(*Echo[diagfieldindices];*)
BRSTcheck = ToExpression[StringCases[ToString[diagfieldindices],"Q["~~__~~"]"]];
(*Echo[BRSTcheck];*)
If[Length@BRSTcheck>0,
(*Echo["true"];*)
posBRSTfield = {#,"indices",Position[diagfieldindices,BRSTcheck[[1]]][[1,1]],2,1};

(*Echo[{"pos BRST: ",posBRSTfield}];*)
,
Nothing
];

)&/@Table[i,{i,1,Length@diag}];

newVar = diag[[posBRSTfield[[1]],posBRSTfield[[2]],posBRSTfield[[3]],2,1]];
(*Echo[newVar];*)

prefVar = newVar/(newVar/.signRules);
If[prefVar>0,
MomentumCons = Flatten@Solve[newVar+loopMom==0,newVar];
,
MomentumCons = Flatten@Solve[newVar+loopMom==0,prefVar*newVar]
];
startingPoint = posBRSTfield[[1]];
,

If[MemberQ[externalMomenta,diag[[1,"indices",#,2,1]]],
Nothing;
,
newVar = diag[[1,"indices",#,2,1]];

prefVar = newVar/(newVar/.signRules);
If[prefVar>0,
MomentumCons = Flatten@Solve[Total@diag[[1,"indices",All,2,1]] == 0, newVar];
,
MomentumCons = Flatten@Solve[Total@diag[[1,"indices",All,2,1]] == 0, prefVar*newVar];
];

 ]&/@Table[i,{i,1,Length@diag[[1,"indices",All,2]]}];
startingPoint = 1;
];


(*Echo[{"Momentum cons.: ", MomentumCons}];*)



FullDeltaList = Join[Evaluate[FullDeltaList/.MomentumCons],MomentumCons];
myEcho[{"Delta List of momenta: ",FullDeltaList},4];

(* go through the diagram from left to right, starting from the first vertex or BRST vertex *)
(
myEcho[{"check object no. ",#},4];

allMomenta = diag[[#,"indices",All,2,1]]/.FullDeltaList;
myEcho[{"all momenta in the object which is momentum fixed right now",allMomenta},4];
(* find out position of new momentum which is not yet fixed *)
myEcho[{"here.............................",allMomenta,externalMomenta,signRules,loopMom},6];
posNewMom = FindPositionNewMomentum[allMomenta,externalMomenta,signRules,loopMom];

myEcho[{"here.............................",diag[[#]],posNewMom,allMomenta,FullDeltaList,signRules},6];
FullDeltaList = FixMomentum[diag[[#]],posNewMom,allMomenta,FullDeltaList,signRules];

)&/@Join[Table[i,{i,startingPoint+1,Length@diag}],Table[i,{i,1,startingPoint-1}]];


Return[FullDeltaList]
]


(* ::Input::Initialization:: *)
Clear[ShiftLoopMomentum]
ShiftLoopMomentum[Regdot_,loopIndex_] := Module[{SignRules,oldMomentum,  oldMomClear,Prefold, shift,qnew},
SignRules =  {n_?NumericQ*q_ ->  q};
oldMomentum = Regdot[[1,"indices",2,2,1]];
(*Echo[oldMomentum];*)

myEcho[{oldMomentum,Prefold},5];

(*Echo[{{Evaluate[-loopIndex],loopIndex},oldMomentum}];*)

If[MemberQ[{Evaluate[-loopIndex],loopIndex},oldMomentum],

myEcho["nothing to shift",5];
(*Echo["nothing to shift"];*)
Return[{{},Regdot,loopIndex}]

,
(*Echo["correct"];

Echo["shift"];*)
myEcho["need to shift reg mom",5];
(*Echo[oldMomentum == qnew];*)
shift = Flatten@Solve[oldMomentum == qnew, loopIndex];
(*Echo[shift];*)
(*If[Prefold< 0,
myEcho["<0",5];
Echo["<0"];
Echo[{oldMomentum == qnew, loopIndex}];
shift = First[Solve[oldMomentum == qnew, loopIndex]];
,
Echo[">0"];
myEcho[">0",5];
shift = First[Solve[oldMomentum == -qnew, loopIndex]];
];*)

(*Echo[{Regdot,Regdot/.shift}];*)
Return[{shift,Regdot/.shift,qnew}]
]
]


(* ::Input::Initialization:: *)
Clear[SwitchRegIndices]
SwitchRegIndices[allProps_,allRegs_] := Module[{dummyallRegs = allRegs,LeftIndexPos},
myEcho["SwitchRegIndices",3];

LeftIndexPos = Position[(MemberQ[allProps[[1,"indices",All,2]],allRegs[[1,"indices",#,2]]])&/@Table[i,{i,1,Length@allRegs[[1,"indices"]]}],True][[1,1]];
(*Echo[LeftIndexPos];
Echo[allRegs[[1,"indices",2,LeftIndexPos,2]]];*)
dummyallRegs[[1,"indices",2,LeftIndexPos,2]] = - allRegs[[1,"indices",2,LeftIndexPos,2]];
Return[dummyallRegs]
]


(* ::Subsection::Closed:: *)
(*right now....*)


(* ::Input::Initialization:: *)
(* get an assoc with the positions of objects *)

Clear[getAllObjectPositions]
getAllObjectPositions[RHSDiagram_]:= Module[{props ,regs, vertices, classical, brst,positionAssoc},
props = regs = vertices = classical= brst = {};

(Switch[RHSDiagram[[#,"type"]],
"Propagator",props =Join[props,{#}];,
"Regulator",regs=Join[regs,{#}];,
"Regulatordot",regs=Join[regs,{#}];,

"nPoint",
Switch[RHSDiagram[[#,"spec"]],
"none",vertices= Join[vertices,{#}];,
"classical",classical=Join[classical,{#}];,
"BRST", brst=Join[brst,{#}];
];
])&/@Range[1,Length@RHSDiagram,1];

positionAssoc = <|"Propagators" -> props, "Regulators" -> regs, "Vertices"-> vertices, "classicalVertices"-> classical, "BRSTVertices"-> brst|>;

Return[positionAssoc]
]                                                                                       


(* ::Subsection::Closed:: *)
(*Helper*)


(* ::Input::Initialization:: *)
Clear[ReplaceOneIndex]

ReplaceOneIndex[field_, indices_, superindexReplacementList_, fields_
    ] :=
    Module[{newfield, posfield, newIndices},
        If[MemberQ[superindexReplacementList[[All, 2]], indices],
            Return[{Nothing}]
            ,
            If[ToString[Head[field]] == "Q",
                newfield = field[[1]];
                ,
                newfield = field;
            ];
            posfield = Position[fields, newfield[___]];
            newIndices = Map[Unique, List @@ (Extract[fields, posfield
                [[1]]])];
            Return[{indices -> newIndices}]
        ]
    ]


(* ::Input::Initialization:: *)
Clear[ReplaceIndices]
ReplaceIndices[dummyObject_,superindexReplacementList_,fields_] := Module[{iter,objIndices,objFields,IndexRules,newsuperindexReplacementList = superindexReplacementList},
objIndices = dummyObject[["indices",All,2]];
objFields = dummyObject[["indices",All,1]];

For[iter=1,iter<=Length@objIndices,iter++,
IndexRules = ReplaceOneIndex[objFields[[iter]],objIndices[[iter]],superindexReplacementList,fields];
newsuperindexReplacementList = Join[IndexRules,newsuperindexReplacementList];
objIndices = objIndices/.newsuperindexReplacementList;
objFields = objFields/.newsuperindexReplacementList;
];

Return[newsuperindexReplacementList]
]


(* ::Input::Initialization:: *)
Clear[conserveOneObjectMomentum]
conserveOneObjectMomentum[DiagObj_,momConsList_,loopIndex_,BRSTmomentum___] := Module[{newDiagObj, newMomList,openMomIndex, newRule},
newDiagObj = DiagObj/.momConsList;

newMomList = newDiagObj[["indices",All,2,1]];

openMomIndex =  (If[StringMatchQ[ToString[newMomList[[#]]], __~~"$"~~__],
newMomList[[#]],
Nothing])&/@Range[Length@newMomList];

(*Echo[openMomIndex];*)
If[Length@openMomIndex== 0,

Return[momConsList]
,
newRule = conserveOneObjectMomentumbyType[DiagObj,newDiagObj,momConsList,newMomList,openMomIndex,BRSTmomentum];
Return[newRule]
]
]


(* ::Input::Initialization:: *)
Clear[conserveOneObjectMomentumbyType]
conserveOneObjectMomentumbyType[DiagObj_,newDiagObj_,momConsList_,newMomList_,openMomIndex_,BRSTmomentum_] := Module[{newRule},

Switch[DiagObj[["type"]],
"Regulator",newRule = conserveRegulatorMomentum[newDiagObj,momConsList,newMomList,openMomIndex,BRSTmomentum],
"Regulatordot",newRule = conserveRegulatorMomentum[newDiagObj,momConsList,newMomList,openMomIndex,BRSTmomentum],
"Propagator",newRule = conserveVertexMomentum[newDiagObj,momConsList,newMomList,openMomIndex],
"nPoint", newRule = conserveVertexMomentum[newDiagObj,momConsList,newMomList,openMomIndex];
];
Return[newRule]
]

conserveOneObjectMomentumbyType[DiagObj_,newDiagObj_,momConsList_,newMomList_,openMomIndex_] := Module[{newRule},

newRule = conserveVertexMomentum[newDiagObj,momConsList,newMomList,openMomIndex];

Return[newRule]
]


(* ::Input::Initialization:: *)
Clear[conserveRegulatorMomentum]
conserveRegulatorMomentum[DiagObj_,momConsList_,newMomList_,openMomIndex_,BRSTmomentum_] := Module[{openMomIndexnew,newRule},

If[Length@openMomIndex==0,
Return[momConsList]
,

openMomIndexnew = openMomIndex[[1]];

newRule = Solve[(Total[newMomList]+2*QMeSderivation`Private`q)==0, openMomIndexnew];

Return[Join[momConsList,newRule[[1]]]]
]
]


(* ::Input::Initialization:: *)
Clear[conserveVertexMomentum]
conserveVertexMomentum[DiagObj_,momConsList_,newMomList_,openMomIndex_] := Module[{newRule,dummyRule,dummynewIndex},

If[Length@openMomIndex==0,
Return[momConsList]
,

dummyRule = {openMomIndex[[1]]->dummynewIndex};

newRule = Solve[Total[newMomList/.dummyRule]==0, dummynewIndex]/.Reverse/@dummyRule;


Return[Join[momConsList,newRule[[1]]]]
]
]


(* ::Input::Initialization:: *)
Clear[shiftLoopFromBRST]
shiftLoopFromBRST[Diag_,objectPositionAssoc_,loopIndex_] := Module[{posBRSTsource,BRSTmomentum, momConsList,momConsListnew},

posBRSTsource = (Position[Diag[[objectPositionAssoc[["BRSTVertices"]],"indices"]][[1]],Global`Q])[[1]];

BRSTmomentum = Extract[Extract[Diag[[objectPositionAssoc[["BRSTVertices"]],"indices"]][[1]],posBRSTsource[[1]]],{2,1}];
momConsList = {BRSTmomentum->-loopIndex};

momConsListnew = conserveOneObjectMomentum[Diag[[objectPositionAssoc[["BRSTVertices"]][[1]]]],momConsList,loopIndex,BRSTmomentum];


If[objectPositionAssoc[["BRSTVertices"]][[1]] == Length@Diag,

(momConsListnew = conserveOneObjectMomentum[Diag[[#]],momConsListnew,loopIndex,BRSTmomentum])&/@Range[1,Length@Diag-1];
,
(momConsListnew = conserveOneObjectMomentum[Diag[[#]],momConsListnew,loopIndex,BRSTmomentum])&/@Join[Range[objectPositionAssoc[["BRSTVertices"]][[1]]+1,Length@Diag],Range[1,objectPositionAssoc[["BRSTVertices"]][[1]]-1]];
];

Return[Diag/.momConsListnew];
]


(* ::Input::Initialization:: *)
Clear[shiftLoopFromFirstVertex]
shiftLoopFromFirstVertex[Diag_,objectPositionAssoc_,derivativeList_,loopIndex_] := Module[{posfirstDer,direction, iter = 2,loopDirIndex,momConsList,momConsListnew,distanceright,distanceleft,IndexDiag},
posfirstDer = (Position[Diag[[All,"indices"]],{Head[derivativeList[[1]]],List@@(derivativeList[[1]])}])[[1]];

(*unique tad direction*)
While[(Length@Diag-posfirstDer[[1]] == posfirstDer[[1]]-2)&& (iter <= Length@derivativeList),

posfirstDer = (Position[Diag[[All,"indices"]],{Head[derivativeList[[iter]]],List@@(derivativeList[[iter]])}])[[1]];
iter = iter+1;
];

(*Echo[{posfirstDer,Length@Diag}];*)

(*If[Length@Diag-posfirstDer[[1]]==posfirstDer[[1]]-2,

direction = Join[Range[posfirstDer[[1]]+1,Length@Diag],Range[1,posfirstDer[[1]]-1]];
,
Switch[objectPositionAssoc[["Regulators",1]],
posfirstDer[[1]]+2-Length@Diag, 
 direction = Join[Range[posfirstDer[[1]]+1,Length@Diag],Range[1,posfirstDer[[1]]-1]]; 
,
posfirstDer[[1]]-2, direction =Join[Range[posfirstDer[[1]]-1,1,-1],Range[Length@Diag,posfirstDer[[1]]+1,-1]];
];];*)
distanceright = Length@Diag-posfirstDer[[1]]+objectPositionAssoc[["Regulators",1]];
distanceleft = posfirstDer[[1]]-objectPositionAssoc[["Regulators",1]];

If[distanceright(*>*)<distanceleft,
direction = (*Join[Range[posfirstDer[[1]]+1,Length@Diag],Range[1,posfirstDer[[1]]-1]]*)Join[Range[objectPositionAssoc[["Regulators",1]],Length@Diag],Range[1,objectPositionAssoc[["Regulators",1]]]];

,
If[distanceright == distanceleft,
direction = (*Join[Range[posfirstDer[[1]]+1,Length@Diag],Range[1,posfirstDer[[1]]-1]]*)Join[Range[objectPositionAssoc[["Regulators",1]],Length@Diag],Range[1,objectPositionAssoc[["Regulators",1]]]];

,
If[distanceright(*<*)>distanceleft,
direction =(*Join[Range[posfirstDer[[1]]-1,1,-1],Range[Length@Diag,posfirstDer[[1]]+1,-1]]*)Join[Range[objectPositionAssoc[["Regulators",1]],1,-1],Range[Length@Diag,objectPositionAssoc[["Regulators",1]],-1]];

,
direction =(*Join[Range[posfirstDer[[1]]-1,1,-1],Range[Length@Diag,posfirstDer[[1]]+1,-1]]*)Join[Range[objectPositionAssoc[["Regulators",1]],1,-1],Range[Length@Diag,objectPositionAssoc[["Regulators",1]],-1]];

];
];
];

(*loopDirIndex = FindMatchingIndex[Diag[[posfirstDer[[1]]]],Diag[[direction[[1]]]]];*)

loopDirIndex = FindMatchingIndex[Diag[[objectPositionAssoc[["Regulators",1]]]],Diag[[direction[[1]]]]];

momConsList = {loopDirIndex->-loopIndex};



momConsListnew = conserveOneObjectMomentum[Diag[[(*posfirstDer[[1]]*)objectPositionAssoc[["Regulators",1]]]],momConsList,loopIndex];



(momConsListnew = conserveOneObjectMomentum[Diag[[#]],momConsListnew,loopIndex])&/@direction;


Return[Diag/.momConsListnew];
]




(* ::Input::Initialization:: *)
Clear[FindMatchingIndex]
FindMatchingIndex[firstDiag_,nextDiag_] := Module[{allfirstIndices,allnextIndices,matchingIndex},

allfirstIndices = firstDiag[["indices",All,2,1]];
allnextIndices = nextDiag[["indices",All,2,1]];

matchingIndex = (If[MemberQ[allfirstIndices,allnextIndices[[#]]],
allnextIndices[[#]],
Nothing
])&/@Range[Length@allnextIndices];

Return[matchingIndex[[1]]]
]


(* ::Input::Initialization:: *)
Clear[shiftLoopFromFirstVertexDSE]
shiftLoopFromFirstVertexDSE[Diag_,objectPositionAssoc_,derivativeList_,loopIndex_] := Module[{posfirstDer,direction, loopDirIndex,momConsList,momConsListnew},
posfirstDer = (Position[Diag[[All,"indices"]],{Head[derivativeList[[1]]],List@@(derivativeList[[1]])}])[[1]];


 direction = Join[Range[posfirstDer[[1]]+1,Length@Diag],Range[1,posfirstDer[[1]]-1]]; 


loopDirIndex = FindMatchingIndex[Diag[[posfirstDer[[1]]]],Diag[[direction[[1]]]]];

momConsList = {loopDirIndex->-loopIndex};



momConsListnew = conserveOneObjectMomentum[Diag[[posfirstDer[[1]]]],momConsList,loopIndex];



(momConsListnew = conserveOneObjectMomentum[Diag[[#]],momConsListnew,loopIndex])&/@direction;

Return[Diag/.momConsListnew];
]


(* ::Input::Initialization:: *)
Clear[conserveExternalMomenta]
conserveExternalMomenta[dummyDiagObj_,objectPositionAssoc_] := Module[{posBRSTsource,BRSTmomentum,BRSTshift,newdummyDiag,momConsList = {}},

If[Length@(objectPositionAssoc["BRSTVertices"])>0,
posBRSTsource = (Position[dummyDiagObj[[objectPositionAssoc[["BRSTVertices"]],"indices"]][[1]],Global`Q])[[1]];

BRSTmomentum = Extract[Extract[dummyDiagObj[[objectPositionAssoc[["BRSTVertices"]],"indices"]][[1]],posBRSTsource[[1]]],{2,1}];
BRSTshift = {BRSTmomentum->-BRSTmomentum};
newdummyDiag = Join[dummyDiagObj[[1;;objectPositionAssoc[["BRSTVertices",1]]-1]],{dummyDiagObj[[objectPositionAssoc[["BRSTVertices",1]]]]/.BRSTshift},dummyDiagObj[[objectPositionAssoc[["BRSTVertices",1]]+1;;-1]]];

(momConsList= conserveOneObjectMomentum[newdummyDiag[[#]],momConsList,"dummyIndex",BRSTmomentum])&/@Range[Length@dummyDiagObj];
,
newdummyDiag = dummyDiagObj;
(momConsList= conserveOneObjectMomentum[newdummyDiag[[#]],momConsList,"dummyIndex"])&/@Range[Length@dummyDiagObj];
];


Return[newdummyDiag/.momConsList]
]


(* ::Input::Initialization:: *)
Clear[ReplaceObjectsWithFunctions]
ReplaceObjectsWithFunctions[DiagObj_]:= Module[{funcObj = Table[Null,Length@DiagObj],AllVarsList = {},newIndicesBRST},

Switch[DiagObj[[#,"type"]],

"Regulator",funcObj[[#]]= ToExpression[StringJoin["R",Map[ToString,DiagObj[[#,"indices",All,1]]]]][Flatten[DiagObj[[#,"indices",All,2]]]];
AllVarsList = Union@Join[AllVarsList,Union@Flatten@DiagObj[[#,"indices",All,2]]];,
"Regulatordot",funcObj[[#]] = ToExpression[StringJoin["Rdot",Map[ToString,DiagObj[[#,"indices",All,1]]]]][Flatten[DiagObj[[#,"indices",All,2]]]];
AllVarsList = Union@Join[AllVarsList,Union@Flatten@DiagObj[[#,"indices",All,2]]];,
"Propagator",funcObj[[#]] = ToExpression[StringJoin["G",Map[ToString,DiagObj[[#,"indices",All,1]]]]][Flatten[DiagObj[[#,"indices",All,2]]]];AllVarsList = Union@Join[AllVarsList,Union@Flatten@DiagObj[[#,"indices",All,2]]];,
"nPoint", 
Switch[DiagObj[[#,"spec"]],
"none",funcObj[[#]]= ToExpression[StringJoin["\[CapitalGamma]",Map[ToString,DiagObj[[#,"indices",All,1]]]]][Flatten[DiagObj[[#,"indices",All,2]]]];
AllVarsList = Union@Join[AllVarsList,Union@Flatten@DiagObj[[#,"indices",All,2]]];,
"classical",funcObj[[#]]= ToExpression[StringJoin["S",Map[ToString,DiagObj[[#,"indices",All,1]]]]][Flatten[DiagObj[[#,"indices",All,2]]]];
AllVarsList = Union@Join[AllVarsList,Union@Flatten@DiagObj[[#,"indices",All,2]]];,
"BRST", newIndicesBRST = replaceIndicesBRST[DiagObj[[#,"indices"]]];funcObj[[#]] = ToExpression[StringJoin["\[CapitalGamma]",Map[ToString,newIndicesBRST[[All,1]]]]][Flatten[newIndicesBRST[[All,2]]]];
AllVarsList = Union@Join[AllVarsList,Union@Flatten@DiagObj[[#,"indices",All,2]]];
];
]&/@Range[Length@DiagObj];



Return[{AllVarsList,funcObj}]
]


(* ::Input::Initialization:: *)
Clear[replaceIndicesBRST]
replaceIndicesBRST[indices_]:=Module[{positionBRST, newindices,newBRSTName},
positionBRST = If[StringMatchQ[ToString[indices[[#,1]]],"Q"~~__],
#,Nothing]&/@Range[Length@indices];

newBRSTName = ToExpression[StringJoin[ToString[Head[indices[[positionBRST[[1]],1]]]],ToString[(List@@(indices[[positionBRST[[1]],1]]))[[1]]]]];

newindices = Join[indices[[1;;positionBRST[[1]]-1]],{Join[{newBRSTName},{indices[[positionBRST[[1]],2]]}]},indices[[positionBRST[[1]]+1;;-1]]];

Return[newindices]
]


(* ::Subsection::Closed:: *)
(*Main Function*)


(* ::Input::Initialization:: *)
Clear[InsertFeynRules] (*takes as RHS one single diagram*)
InsertFeynRules[diag_,derivativeList_,fields_,loopIndex_] := Module[{dummyDiagObj = Drop[diag,{1}], prefDiag = diag[[1,2]],objectPositionAssoc,newsuperindexReplacementList,shiftedDiagObj,funcObjects,diagVars,fullDiag},

myEcho["Insert Feynman Rules",1];

(*Identification of different objects*)
objectPositionAssoc = getAllObjectPositions[dummyDiagObj];



newsuperindexReplacementList = (derivativeList[[#]]->List@@(derivativeList[[#]]))&/@Range[Length@derivativeList];




(* replace superincides with full set of indices *)
dummyDiagObj = dummyDiagObj/.newsuperindexReplacementList;
(newsuperindexReplacementList= ReplaceIndices[dummyDiagObj[[#]],newsuperindexReplacementList,fields];
dummyDiagObj = dummyDiagObj/.newsuperindexReplacementList;
)&/@Range[Length@dummyDiagObj];


(* ------------------ Check and shift loop momentum ------------------------------ *)

(* check if it's more than 1-loop *)

If[(Length@objectPositionAssoc["Propagators"])-(Length@objectPositionAssoc["Regulators"])> ((Length@objectPositionAssoc["Vertices"])+(Length@objectPositionAssoc["classicalVertices"])+(Length@objectPositionAssoc["BRSTVertices"])),
(* more than 1-loop *)
(*Echo["more than one loop"];*)
shiftedDiagObj = dummyDiagObj;
,
(* if no loop, simply conserve external momenta *)

If[Length@objectPositionAssoc["Propagators"]==0,

shiftedDiagObj = conserveExternalMomenta[dummyDiagObj,objectPositionAssoc];
,

If[Length@dummyDiagObj==1,
shiftedDiagObj = dummyDiagObj;
,
(* If there is a Brst, then that field has -q *)
If[Length@objectPositionAssoc[["BRSTVertices"]]>0,
shiftedDiagObj = shiftLoopFromBRST[dummyDiagObj,objectPositionAssoc,loopIndex];

,
(* Otherwise start from the first vertex with loop mom conservation and shift *)
If[Length@objectPositionAssoc[["Regulators"]]==0,

shiftedDiagObj = shiftLoopFromFirstVertexDSE[dummyDiagObj,objectPositionAssoc,derivativeList,loopIndex];
,

shiftedDiagObj = shiftLoopFromFirstVertex[dummyDiagObj,objectPositionAssoc,derivativeList,loopIndex];
];
];
];
];
];


(* Prepare objects such that one can easily insert the Feynman Rules *)

{diagVars,funcObjects} = ReplaceObjectsWithFunctions[shiftedDiagObj];
fullDiag =  prefDiag*Apply[Times,funcObjects];

Return[{diagVars,fullDiag}]
]


(* ::Input::Initialization:: *)
Clear[InsertFeynRulesAllDiags] (*takes as RHS one single diagram*)
InsertFeynRulesAllDiags[allDiags_,derivativeList_,fields_,loopIndex_] := Module[{allVars = Table[{},Length@allDiags], fullDiags = Table[Null,Length@allDiags]},



(
myEcho[{"Insert Feynman Rules in Diagram no: ",#},1];

{allVars[[#]],fullDiags[[#]]} = InsertFeynRules[allDiags[[#]],derivativeList,fields,loopIndex];

)&/@Table[i,{i,1,Length@allDiags}];

Return[{allVars,fullDiags}]

]


(* ::Section:: *)
(*Derive Equations*)


(* ::Input::Initialization:: *)
Clear[DeriveFunctionalEquation]

DeriveFunctionalEquation[setupAssoc_, derivativeList_, OptionsPattern[]] :=
    Module[{masterEq, derivativeListnew, dse = False, classicalAction,
         fields, truncation, outputLevel, loopIndex, funcDerDiagrams, replacementList,
         superindexReplacementList, superindexDiags, allVars, fullDiags},

        If[AssociationQ[setupAssoc[["MasterEquation"]]] == True,
            classicalAction = setupAssoc[["MasterEquation", "classicalAction"]];
            masterEq = getDSE[classicalAction, Last[derivativeList]];
            derivativeListnew = derivativeList[[1 ;; -2]];
            dse = True;
            ,
            derivativeListnew = derivativeList;
            masterEq = setupAssoc[["MasterEquation"]];
        ];

        fields =Association["bosonic"->{},"fermionic"->{},setupAssoc[["FieldSpace"]]];
        truncation = setupAssoc[["Truncation"]];
        myEcho[{masterEq, fields, truncation}, 1];
        outputLevel = OptionValue["OutputLevel"];
        loopIndex = OptionValue["LoopIndex"];

        Switch[outputLevel,
            "getDSE",
                If[dse == True,
                    Return[masterEq]
                    ,Print["Master equation is aleady given. Please choose a different output (i.e. \[OpenCurlyDoubleQuote]FunctionalDerivatives\[CloseCurlyDoubleQuote], \[OpenCurlyDoubleQuote]SuperindexDiagrams\[CloseCurlyDoubleQuote] or \[OpenCurlyDoubleQuote]FullDiagrams\[CloseCurlyDoubleQuote])."];
                    Return[masterEq]
                ]
            ,

            "FunctionalDerivatives",
                {funcDerDiagrams, replacementList} = MultipleFuncDer[masterEq, derivativeListnew];
                funcDerDiagrams = funcDerDiagrams /. replacementList;
                    
                Return[funcDerDiagrams]
            ,

            "SuperindexDiagrams",
                {funcDerDiagrams, replacementList}=MultipleFuncDer[masterEq, derivativeListnew];

                If[AssociationQ[setupAssoc[["MasterEquation"]]] == True,
                    superindexDiags = TraceOverFields[funcDerDiagrams,derivativeList, replacementList, fields, truncation, classicalAction],
                    superindexDiags = TraceOverFields[funcDerDiagrams,derivativeList, replacementList, fields, truncation]];

                Return[superindexDiags]
            ,

            "FullDiagrams",
                myEcho[MultipleFuncDer[masterEq, derivativeListnew],1];
                {funcDerDiagrams, replacementList} = MultipleFuncDer[masterEq, derivativeListnew];

                If[AssociationQ[setupAssoc[["MasterEquation"]]] == True,
                    superindexDiags = TraceOverFields[funcDerDiagrams,derivativeList, replacementList, fields, truncation, classicalAction],
                    superindexDiags = TraceOverFields[funcDerDiagrams,derivativeList, replacementList, fields, truncation]
                ];

                {allVars, fullDiags} = InsertFeynRulesAllDiags[superindexDiags, derivativeList, fields, loopIndex] /. QMeSderivation`Private`q->Global`q;

                If[OptionValue["DummyVarList"] == False,
                    Return[fullDiags],
                    Return[{allVars, fullDiags}]
                ];
        ];
    ]

Options[DeriveFunctionalEquation] = {"OutputLevel" -> "FunctionalDerivatives",
     "LoopIndex" -> q, "DummyVarList" -> False};


(* ::Input::Initialization:: *)
End[]
EndPackage[]
