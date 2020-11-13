(* ::Package:: *)

(* ::Chapter:: *)
(*getDSE*)


(* ::Input::Initialization:: *)
BeginPackage["getDSE`"]


(* ::Input::Initialization:: *)
getDSE::usage = "DeriveFlowEquation[setup, derivativelist, options]";


(* ::Input::Initialization:: *)
Begin["Private`"]


(* ::Input::Initialization:: *)
$DebugLevel =0;


(* ::Input::Initialization:: *)
myEcho[msg_,lvl_] := If[$DebugLevel >=lvl, Echo[msg];, Nothing;]


(* ::Input:: *)
(**)


(* ::Section:: *)
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



(* ::Input:: *)
(*getDSE[{{Phi,Phi},{Phi,Phi,Phi},{Phi,Phi,Phi,Phi}},Phi[a]]*)


(* ::Input:: *)
(*getDSE[{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},c[-p1-p2,o]]*)


(* ::Subsection:: *)
(*generate Classical Action*)


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


(* ::Input:: *)
(*generateClassicalAction[{{Phi,Phi},{Phi,Phi,Phi},{Phi,Phi,Phi,Phi}}]*)


(* ::Input:: *)
(*generateClassicalAction[{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input::Initialization:: *)
(*transform list of vertices into objects*)
Clear[transformToClassicalObject]
transformToClassicalObject[classicalActionEntry_] := Module[{objectList,fullObjectList},
objectList =  (<|"type"-> "classicalField", "indices"-> {classicalActionEntry[[#]][Unique[Global`a83]]}|>)&/@Range[Length@classicalActionEntry];
fullObjectList = Join[{"Prefactor"-> {1},<|"type"->"nPoint","indices"->Evaluate[objectList[[All,"indices"]]][[All,1]],"nPoint"-> Length@objectList,"spec"->"classical"|>},objectList];

Return[fullObjectList]
]



(* ::Input:: *)
(*transformToClassicalObject[{Phi,Phi}]*)


(* ::Subsection:: *)
(*take first field derivative*)


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


(* ::Input:: *)
(*takeFieldDerivative[{{"Prefactor"->{1},<|"type"->"nPoint","indices"->{Phi[a$2370],Phi[a$2371]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{Phi[a$2370]}|>,<|"type"->"classicalField","indices"->{Phi[a$2371]}|>},{"Prefactor"->{1},<|"type"->"nPoint","indices"->{Phi[a$2374],Phi[a$2375],Phi[a$2376]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{Phi[a$2374]}|>,<|"type"->"classicalField","indices"->{Phi[a$2375]}|>,<|"type"->"classicalField","indices"->{Phi[a$2376]}|>},{"Prefactor"->{1},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a$2382]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{Phi[a$2379]}|>,<|"type"->"classicalField","indices"->{Phi[a$2380]}|>,<|"type"->"classicalField","indices"->{Phi[a$2381]}|>,<|"type"->"classicalField","indices"->{Phi[a$2382]}|>}},Phi[a]]*)


(* ::Input:: *)
(*takeFieldDerivative[{{"Prefactor"->{1},<|"type"->"nPoint","indices"->{A[a$2349],A[a$2350]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{A[a$2349]}|>,<|"type"->"classicalField","indices"->{A[a$2350]}|>},{"Prefactor"->{1},<|"type"->"nPoint","indices"->{c[a$2353],cbar[a$2354]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{c[a$2353]}|>,<|"type"->"classicalField","indices"->{cbar[a$2354]}|>},{"Prefactor"->{1},<|"type"->"nPoint","indices"->{A[a$2357],A[a$2358],A[a$2359]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{A[a$2357]}|>,<|"type"->"classicalField","indices"->{A[a$2358]}|>,<|"type"->"classicalField","indices"->{A[a$2359]}|>},{"Prefactor"->{1},<|"type"->"nPoint","indices"->{A[a$2362],A[a$2363],A[a$2364],A[a$2365]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{A[a$2362]}|>,<|"type"->"classicalField","indices"->{A[a$2363]}|>,<|"type"->"classicalField","indices"->{A[a$2364]}|>,<|"type"->"classicalField","indices"->{A[a$2365]}|>},{"Prefactor"->{1},<|"type"->"nPoint","indices"->{A[a$2368],c[a$2369],cbar[a$2370]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{A[a$2368]}|>,<|"type"->"classicalField","indices"->{c[a$2369]}|>,<|"type"->"classicalField","indices"->{cbar[a$2370]}|>}},c[-p1-p2,o]]*)


(* ::Input:: *)
(*Length@c[a]*)
(*List@@c*)
(*List@@c[iasd]*)


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



(* ::Input:: *)
(*takeOneFieldDerivative[{"Prefactor"->{1},<|"type"->"nPoint","indices"->{Phi[a$148054],Phi[a$148055]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{Phi[a$148054]}|>,<|"type"->"classicalField","indices"->{Phi[a$148055]}|>},Phi[n]]*)


(* ::Input:: *)
(*takeOneFieldDerivative[{"Prefactor"->{1},<|"type"->"nPoint","indices"->{A[a$2349],A[a$2350]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{A[a$2349]}|>,<|"type"->"classicalField","indices"->{A[a$2350]}|>},c[-p1-p2,o]]*)


(* ::Input:: *)
(*takeOneFieldDerivative[{"Prefactor"->{1},<|"type"->"nPoint","indices"->{c[a$2353],cbar[a$2354]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{c[a$2353]}|>,<|"type"->"classicalField","indices"->{cbar[a$2354]}|>},c[-p1-p2,o]]*)


(* ::Subsection:: *)
(*replace with expectation value*)


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


(* ::Input:: *)
(*replaceExpectationValue[{(*{"Prefactor"\[Rule]{1},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{Phi[a],Phi[a$2371]},"nPoint"\[Rule]2,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2371]}\[RightAssociation]},{"Prefactor"\[Rule]{1,{Phi[a$2370],Phi[a]}},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{Phi[a$2370],Phi[a]},"nPoint"\[Rule]2,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2370]}\[RightAssociation]},{"Prefactor"\[Rule]{1},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{Phi[a],Phi[a$2375],Phi[a$2376]},"nPoint"\[Rule]3,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2375]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2376]}\[RightAssociation]},{"Prefactor"\[Rule]{1,{Phi[a$2374],Phi[a]}},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{Phi[a$2374],Phi[a],Phi[a$2376]},"nPoint"\[Rule]3,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2374]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2376]}\[RightAssociation]},{"Prefactor"\[Rule]{1,{Phi[a$2374],Phi[a]},{Phi[a$2375],Phi[a]}},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{Phi[a$2374],Phi[a$2375],Phi[a]},"nPoint"\[Rule]3,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2374]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2375]}\[RightAssociation]},{"Prefactor"\[Rule]{1},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{Phi[a],Phi[a$2380],Phi[a$2381],Phi[a$2382]},"nPoint"\[Rule]4,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2380]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2381]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2382]}\[RightAssociation]},{"Prefactor"\[Rule]{1,{Phi[a$2379],Phi[a]}},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{Phi[a$2379],Phi[a],Phi[a$2381],Phi[a$2382]},"nPoint"\[Rule]4,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2379]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2381]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2382]}\[RightAssociation]},{"Prefactor"\[Rule]{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]}},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{Phi[a$2379],Phi[a$2380],Phi[a],Phi[a$2382]},"nPoint"\[Rule]4,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2379]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2380]}\[RightAssociation],\[LeftAssociation]"type"\[Rule]"classicalField","indices"\[Rule]{Phi[a$2382]}\[RightAssociation]},*){"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{Phi[a$2379]}|>,<|"type"->"classicalField","indices"->{Phi[a$2380]}|>,<|"type"->"classicalField","indices"->{Phi[a$2381]}|>}}]*)


(* ::Input:: *)
(*replaceExpectationValue[{{"Prefactor"->{1},<|"type"->"nPoint","indices"->{c[-p1-p2,o],cbar[a$2354]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{cbar[a$2354]}|>},{"Prefactor"->{1,{A[a$2368],c[-p1-p2,o]}},<|"type"->"nPoint","indices"->{A[a$2368],c[-p1-p2,o],cbar[a$2370]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{A[a$2368]}|>,<|"type"->"classicalField","indices"->{cbar[a$2370]}|>}}]*)


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


(* ::Input:: *)
(*replaceOneExpectationValue[{"Prefactor"->1,<|"type"->"nPoint","indices"->{Phi[a],Phi[a$148059],Phi[a$148060]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{Phi[a$148059]}|>,<|"type"->"classicalField","indices"->{Phi[a$148060]}|>}]*)


(* ::Input:: *)
(*replaceOneExpectationValue[{},{{"Prefactor"->1,<|"type"->"nPoint","indices"->{Phi[a],Phi[a$148059],Phi[a$148060]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Field","indices"->{Phi[a$148059]}|>,<|"type"->"Field","indices"->{Phi[a$148060]}|>},{"Prefactor"->1,<|"type"->"nPoint","indices"->{Phi[a],Phi[a$148059],Phi[a$148060]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Field","indices"->{Phi[a$148059]}|>,<|"type"->"Propagator","indices"->{Phi[a$148060],a$9808}|>,<|"type"->"DerField","indices"->{a$9808}|>},{"Prefactor"->1,<|"type"->"nPoint","indices"->{Phi[a],Phi[a$148059],Phi[a$148060]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$148059],a$9805}|>,<|"type"->"DerField","indices"->{a$9805}|>,<|"type"->"classicalField","indices"->{Phi[a$148060]}|>}}]*)


(* ::Input:: *)
(*replaceOneExpectationValue[{{"Prefactor"->1,<|"type"->"nPoint","indices"->{Phi[a],Phi[a$148059],Phi[a$148060]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Field","indices"->{Phi[a$148059]}|>,<|"type"->"Field","indices"->{Phi[a$148060]}|>}},{{"Prefactor"->1,<|"type"->"nPoint","indices"->{Phi[a],Phi[a$148059],Phi[a$148060]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Field","indices"->{Phi[a$148059]}|>,<|"type"->"Propagator","indices"->{Phi[a$148060],a$9808}|>,<|"type"->"DerField","indices"->{a$9808}|>},{"Prefactor"->1,<|"type"->"nPoint","indices"->{Phi[a],Phi[a$148059],Phi[a$148060]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$148059],a$9805}|>,<|"type"->"DerField","indices"->{a$9805}|>,<|"type"->"classicalField","indices"->{Phi[a$148060]}|>}}]*)


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




(* ::Input:: *)
(*scanForFirstClassicalFieldAndReplace[{"Prefactor"->1,<|"type"->"nPoint","indices"->{Phi[a],Phi[a$148055]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"classicalField","indices"->{Phi[a$148055]}|>}]*)


(* ::Subsection:: *)
(*take final derivatives*)


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


(* ::Input:: *)
(*takeFinalFieldDerivatives[{{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Field","indices"->{Phi[a$2379]}|>,<|"type"->"Field","indices"->{Phi[a$2380]}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>},{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Field","indices"->{Phi[a$2379]}|>,<|"type"->"Field","indices"->{Phi[a$2380]}|>,<|"type"->"Propagator","indices"->{Phi[a$2381],a$9946}|>,<|"type"->"DerField","indices"->{a$9946}|>},{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Field","indices"->{Phi[a$2379]}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$9943}|>,<|"type"->"DerField","indices"->{a$9943}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>},{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Field","indices"->{Phi[a$2379]}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$9943}|>,<|"type"->"DerField","indices"->{a$9943}|>,<|"type"->"Propagator","indices"->{Phi[a$2381],a$9953}|>,<|"type"->"DerField","indices"->{a$9953}|>},{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Field","indices"->{Phi[a$2380]}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>},{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Field","indices"->{Phi[a$2380]}|>,<|"type"->"Propagator","indices"->{Phi[a$2381],a$9963}|>,<|"type"->"DerField","indices"->{a$9963}|>},{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$9960}|>,<|"type"->"DerField","indices"->{a$9960}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>},{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$9960}|>,<|"type"->"DerField","indices"->{a$9960}|>,<|"type"->"Propagator","indices"->{Phi[a$2381],a$9970}|>,<|"type"->"DerField","indices"->{a$9970}|>}}]*)


(* ::Input:: *)
(*takeFinalFieldDerivatives[{{"Prefactor"->{1},<|"type"->"nPoint","indices"->{c[-p1-p2,o],cbar[a$2354]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"Field","indices"->{cbar[a$2354]}|>},{"Prefactor"->{1},<|"type"->"nPoint","indices"->{c[-p1-p2,o],cbar[a$2354]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{cbar[a$2354],a$4144}|>,<|"type"->"DerField","indices"->{a$4144}|>},{"Prefactor"->{1,{A[a$2368],c[-p1-p2,o]}},<|"type"->"nPoint","indices"->{A[a$2368],c[-p1-p2,o],cbar[a$2370]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Field","indices"->{A[a$2368]}|>,<|"type"->"Field","indices"->{cbar[a$2370]}|>},{"Prefactor"->{1,{A[a$2368],c[-p1-p2,o]}},<|"type"->"nPoint","indices"->{A[a$2368],c[-p1-p2,o],cbar[a$2370]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Field","indices"->{A[a$2368]}|>,<|"type"->"Propagator","indices"->{cbar[a$2370],a$4156}|>,<|"type"->"DerField","indices"->{a$4156}|>},{"Prefactor"->{1,{A[a$2368],c[-p1-p2,o]}},<|"type"->"nPoint","indices"->{A[a$2368],c[-p1-p2,o],cbar[a$2370]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2368],a$4153}|>,<|"type"->"DerField","indices"->{a$4153}|>,<|"type"->"Field","indices"->{cbar[a$2370]}|>},{"Prefactor"->{1,{A[a$2368],c[-p1-p2,o]}},<|"type"->"nPoint","indices"->{A[a$2368],c[-p1-p2,o],cbar[a$2370]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2368],a$4153}|>,<|"type"->"DerField","indices"->{a$4153}|>,<|"type"->"Propagator","indices"->{cbar[a$2370],a$4163}|>,<|"type"->"DerField","indices"->{a$4163}|>}}]*)


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


(* ::Input:: *)
(*takeOneFinalFieldDerivative[{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$9960}|>,<|"type"->"DerField","indices"->{a$9960}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>}]*)


(* ::Input:: *)
(*takeOneFinalFieldDerivative[{},{{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{a$9960,Phi[a]},{a$273040,a$273040},{a$9940,Phi[a$2380]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],a$9960,Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$273039}|>,<|"type"->"nPoint","indices"->{a$273039,a$9940,a$273040},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{a$273040,a$9960}|>}}]*)


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


(* ::Input:: *)
(*scanForLastDerAndReplace[{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$9960}|>,<|"type"->"DerField","indices"->{a$9960}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>}]*)


(* ::Input:: *)
(*scanForLastDerAndReplace[{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"DerField","indices"->{a$9960}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$9960}|>}]*)


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


(* ::Input:: *)
(*takeRightDer[{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Propagator","indices"->{Phi[a$2380],a$9960}|>,<|"type"->"DerField","indices"->{a$9960}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>},6]*)


(* ::Input:: *)
(*takeRightDer[{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"Field","indices"->{Phi[a$2380]}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>},3]*)


(* ::Input:: *)
(*takeRightDer[{"Prefactor"->{1,{Phi[a$2379],Phi[a]},{Phi[a$2380],Phi[a]},{Phi[a$2381],Phi[a]}},<|"type"->"nPoint","indices"->{Phi[a$2379],Phi[a$2380],Phi[a$2381],Phi[a]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"DerField","indices"->{a$9940}|>,<|"type"->"Propagator","indices"->{Phi[a$2379],a$9940}|>,<|"type"->"Field","indices"->{Phi[a$2380]}|>,<|"type"->"Field","indices"->{Phi[a$2381]}|>},3]*)


(* ::Input:: *)
(*{Phi[a$2379],Phi[a]}[[All,1]]*)


(* ::Input::Initialization:: *)
End[]
EndPackage[]
