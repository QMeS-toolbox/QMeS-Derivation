(* ::Package:: *)

(* ::Chapter:: *)
(*Functional Derivatives*)


(* ::Subsection::Closed:: *)
(*maybe not needed *)


(* ::Input::Initialization:: *)
Clear[GenerateMasterEquation]
GenerateMasterEquation[Prefactor_,ObjectStructure_] := Module[{MasterEquation},

myEcho["GenerateMasterEquation",1];

MasterEquation = Join[{Association[ "Prefactor"-> {Prefactor}]}, ObjectStructure];

Return[MasterEquation]

]


(* ::Input:: *)
(*WettEq = GenerateMasterEquation[1/2,{<|"type"->"Propagator","indices"->{i,j}|>,<|"type"->"Regulatordot","indices"->{i,j}|>}]*)


(* ::Input:: *)
(*mSTIEq = GenerateMasterEquation[1,{<|"type"->"Regulator", "indices"-> {i,j}|>, <|"type"->"Propagator", "indices"-> {j,l}|>, <|"type"-> "nPoint", "indices"-> {l,iBRST}, "nPoint"->2, "spec"-> "BRST"|>}]*)


(* ::Subsection:: *)
(*Multiple Functional Derivatives*)


(* ::Input::Initialization:: *)
BeginPackage["FunctionalDerivatives`"]
Begin["Private`"]


(* ::Input:: *)
(**)


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
firstDummyProp[["indices",2]] =  firstDummyIndices;
secondDummyProp = operator;
secondDummyProp[["indices",1]] =  secondDummyIndices;
dummynPoint = <|"type"-> "nPoint", "indices"-> Table[Null,3], "nPoint"-> 3, "spec"-> "none"|>;
dummynPoint[["indices"]][[1]] = firstDummyIndices;
dummynPoint[["indices"]][[2]] = dummyFieldIndex;
dummynPoint[["indices"]][[3]] = secondDummyIndices;

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


(* ::Input:: *)
(*DerRules[<|"type"->"Propagator","indices"->{i,j}|>,k]*)


(* ::Input:: *)
(*DerRules[<|"type"->"Regulator","indices"->{i,j}|>,k]*)


(* ::Input:: *)
(*DerRules[<|"type"->"nPoint","indices"->{A[a$3526],c[-p1-p2,o],cbar[a$3528]},"nPoint"->3,"spec"->"classical"|>,cbar[p2,n]]*)


(* ::Input:: *)
(*DerRules[<|"type"->"nPoint","indices"->{A[a$3526],c[-p1-p2,o],cbar[a$3528]},"nPoint"->3,"spec"->"none"|>,cbar[p2,n]]*)


(* ::Input:: *)
(*DerRules[<|"type"->"Field","indices"->{A[a$3526]}|>,cbar[p2,n]]*)


(* ::Input:: *)
(*DerRules[<|"type"->"Field","indices"->{cbar[a$3526]}|>,cbar[p2,n]]*)


(* ::Input:: *)
(*DerRules[<|"type"->"Propagator","indices"->{A[a$3526],a$3555}|>,cbar[p2,n]]*)


(* ::Input::Initialization:: *)
Clear[MatchDerIndices]
MatchDerIndices[field_,dummyindex_] := Module[{replacementList},
myEcho["MatchDerIndices",2];
replacementList = {dummyindex->field};
Return[replacementList]
]



(* ::Input:: *)
(*MatchDerIndices[A[p,mu,i],iext$92115]*)


(* ::Input:: *)
(**)


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


(* ::Input:: *)
(*MultiplyFactorsFuncDer[{-1,1/2,1,{i,iext$164934},{i$164937,i$164937}}]*)


(* ::Input:: *)
(**)


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


(* ::Input:: *)
(*FuncDer[WettEq,A[p,mu,i]]*)


(* ::Input:: *)
(*FuncDer[(*{"Prefactor"\[Rule]{1},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{c[-p1-p2,o],cbar[a$3512]},"nPoint"\[Rule]2,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"Field","indices"\[Rule]{cbar[a$3512]}\[RightAssociation]},*){"Prefactor"->{1,{A[a$3526],c[-p1-p2,o]}},<|"type"->"nPoint","indices"->{A[a$3526],c[-p1-p2,o],cbar[a$3528]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Field","indices"->{A[a$3526]}|>,<|"type"->"Field","indices"->{cbar[a$3528]}|>}(*,{"Prefactor"\[Rule]{1,{A[a$3526],c[-p1-p2,o]}},\[LeftAssociation]"type"\[Rule]"nPoint","indices"\[Rule]{A[a$3526],c[-p1-p2,o],a$3555},"nPoint"\[Rule]3,"spec"\[Rule]"classical"\[RightAssociation],\[LeftAssociation]"type"\[Rule]"Propagator","indices"\[Rule]{A[a$3526],a$3555}\[RightAssociation]}*),cbar[p2,n]]*)


(* ::Input:: *)
(**)


$DebugLevel = 0;


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


(* ::Input:: *)
(*AcWettEq = MultipleFuncDer[WettEq,DerivativeList]*)


(* ::Input:: *)
(*DerivativeList*)


(* ::Input:: *)
(*AAWettEq = MultipleFuncDer[WettEq,DerivativeList]*)


(* ::Input:: *)
(*AcmSTI = MultipleFuncDer[mSTIEq,DerivativeList]*)


(* ::Input:: *)
(*Clear[myEcho]*)


(* ::Input:: *)
(*StringContainsQ[ToString@{"Prefactor"->{1},<|"type"->"nPoint","indices"->{c[-p1-p2,o],cbar[a$3512]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"Field","indices"->{cbar[a$3512]}|>},"Field"]*)


(* ::Input:: *)
(*MultipleFuncDer[{{"Prefactor"->{1},<|"type"->"nPoint","indices"->{c[-p1-p2,o],cbar[a$3512]},"nPoint"->2,"spec"->"classical"|>,<|"type"->"Field","indices"->{cbar[a$3512]}|>},{"Prefactor"->{1,{A[a$3526],c[-p1-p2,o]}},<|"type"->"nPoint","indices"->{A[a$3526],c[-p1-p2,o],cbar[a$3528]},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Field","indices"->{A[a$3526]}|>,<|"type"->"Field","indices"->{cbar[a$3528]}|>},{"Prefactor"->{1,{A[a$3526],c[-p1-p2,o]}},<|"type"->"nPoint","indices"->{A[a$3526],c[-p1-p2,o],a$3555},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$3526],a$3555}|>}},{cbar[p2,n]}]*)


(* ::Input::Initialization:: *)
End[]
EndPackage[]
