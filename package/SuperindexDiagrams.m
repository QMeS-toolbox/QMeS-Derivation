(* ::Package:: *)

(* ::Chapter:: *)
(*Summation over Indices which gives superindex diagrams*)


(* ::Input::Initialization:: *)
BeginPackage["SuperindexDiagrams`"]
Begin["Private`"]
$DebugLevel = 5;


(* ::Section:: *)
(*Functions *)


(* ::Subsection::Closed:: *)
(*Get Fields and Misc*)


(* ::Input::Initialization:: *)
IndexProjection[iii_,jjj_] := Module[{},Return[iii->jjj]]


(* ::Input:: *)
(**)


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


(* ::Input:: *)
(*GetFieldsList[fields]*)


(* ::Input:: *)
(*fields*)


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


(* ::Input:: *)
(*TestPossibleProps = GetPossibleProps[fields,Truncation]*)


(* ::Input:: *)
(**)


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


(* ::Input:: *)
(*getAllObjectPositions[#]&/@{{"Prefactor"->{1,{A[p1,mu,m],c[-p1-p2,o]},{A[p1,mu,m],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},"nPoint"->3,"spec"->"classical"|>},{"Prefactor"->{1,{i$2506,i$2506},{i$2530,i$2530},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2529}|>,<|"type"->"nPoint","indices"->{i$2529,A[p1,mu,m],i$2530},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2530,i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},{"Prefactor"->{-1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,A[p1,mu,m],cbar[p2,n],i$2506},"nPoint"->4,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},{"Prefactor"->{1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{i$2506,A[p1,mu,m]},{i$2506,A[p1,mu,m]},{i$2536,i$2536},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]},{cbar[p2,n],A[p1,mu,m]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,i$2535}|>,<|"type"->"nPoint","indices"->{i$2535,A[p1,mu,m],i$2536},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2536,a$2422}|>}}*)


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


(* ::Input:: *)
(*{"Prefactor"->{-1,{a$10352,c[-p1-p2,{o}]},{a$10352,c[-p1-p2,{o}]},{a$10352,cbar[p2,{n}]},{a$10352,cbar[p2,{n}]},{a$10353,a$10353},{a$9757,c[a$9439]},{a$9757,c[-p1-p2,{o}]},{a$9757,cbar[p2,{n}]},{A[a$9437],A[p1,{mu,m}]},{A[a$9437],c[-p1-p2,{o}]},{A[a$9437],cbar[p2,{n}]},{c[a$9439],c[-p1-p2,{o}]},{c[a$9439],cbar[p2,{n}]}},<|"type"->"nPoint","indices"->{A[a$9437],A[p1,{mu,m}],c[a$9439],cbar[a$9440]},"nPoint"->4,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$9437],a$9757}|>,<|"type"->"Propagator","indices"->{c[a$9439],a$10352}|>,<|"type"->"nPoint","indices"->{a$10352,cbar[p2,{n}],c[-p1-p2,{o}],a$9757,a$10353},"nPoint"->5,"spec"->"none"|>,<|"type"->"Propagator","indices"->{a$10353,cbar[a$9440]}|>}*)


(* ::Input:: *)
(*getAllPossibleSets[{{a$10352,cbar[p2,{n}],c[-p1-p2,{o}],a$9757,a$10353}},{{A,A},{cbar,c},{c,cbar},{A,A,A},{A,cbar,c},{A,c,cbar},{c,A,cbar},{c,cbar,A},{cbar,c,A},{cbar,A,c}}]*)


(* ::Input:: *)
(*getAllPossibleSets[{{a,b},{c,d},{e,f,g}(*,{h,i,j,k}*)},{{A,A},{cbar,c},{c,cbar},{A,A,A},{A,cbar,c},{A,c,cbar},{c,A,cbar},{c,cbar,A},{cbar,c,A},{cbar,A,c}}]*)


(* ::Input:: *)
(*Permutations[{cbar,c}]*)


(* ::Input:: *)
(*getAllPossibleSets[{{a,b},{c,d},{e,f,g}},{{A,A},{cbar,c},{c,cbar},{A,A,A},{A,cbar,c},{A,c,cbar},{c,A,cbar},{c,cbar,A},{cbar,c,A},{cbar,A,c}}]*)


(* ::Input:: *)
(*getAllPossibleSets[{{a,b},{c,d},{e,f,g}},{{A,A},{cbar,c},{c,cbar},{A,A,A},{A,cbar,c},{A,c,cbar},{c,A,cbar},{c,cbar,A},{cbar,c,A},{cbar,A,c}}]*)


(* ::Input:: *)
(*Flatten[Q[A]]*)
(*Head[Flatten[A[i]]]*)
(*Head[A[i]]*)
(*ToString[Flatten[{{A,A[-p,{mu,m}],Q[A]}}][[3,1]]]*)


(* ::Input:: *)
(*Switch[Head[Flatten[{{cbar,A[-p,{mu,m}],Q[A]}}][[#]]],*)
(**)
(*Symbol, (If[MemberQ[{A,cbar,c},Flatten[{{cbar,A[-p,{mu,m}],Q[A]}}][[#]]],*)
(*{#,Flatten[{{cbar,A[-p,{mu,m}],Q[A]}}][[#]]},*)
(*Nothing]),*)
(*_, If[ToString[Head[Flatten[{{cbar,A[-p,{mu,m}],Q[A]}}][[#]]]] == "Q",*)
(*Echo["here"];*)
(*Echo[ToString[Flatten[{{cbar,A[-p,{mu,m}],Q[A]}}][[#,1]]]];*)
(*If[MemberQ[{A,cbar,c},Flatten[{{cbar,A[-p,{mu,m}],Q[A]}}][[#,1]]],*)
(*Echo["here2"];*)
(*{#,Flatten[{{cbar,A[-p,{mu,m}],Q[A]}}][[#]]}*)
(*,Nothing*)
(*]*)
(*,*)
(*{#,Head[Flatten[{{cbar,A[-p,{mu,m}],Q[A]}}][[#]]] }*)
(*]*)
(*]&/@Range[3]*)


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


(* ::Input:: *)
(*Head[Flatten[{Q[i],j}][[1]]] *)


(* ::Input:: *)
(*getObjectIndexMapping[{"Prefactor"->{-(1/2),{i,A[-p,mu,m]},{i,A[p,nu,n]},{i$1659,A[-p,mu,m]},{i$1659,A[-p,mu,m]},{i$1660,i$1660}},<|"type"->"Regulatordot","indices"->{i,j}|>,<|"type"->"Propagator","indices"->{i,i$1659}|>,<|"type"->"nPoint","indices"->{i$1659,A[-p,mu,m],A[p,nu,n],i$1660},"nPoint"->4,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$1660,j}|>},<|"Propagators"->{3,5},"Regulators"->{2},"Vertices"->{4},"classicalVertices"->{},"BRSTVertices"->{}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{i->A,i$1659->A,i$1660->A,j->A},{i->c,i$1659->cbar,i$1660->c,j->cbar},{i->cbar,i$1659->c,i$1660->cbar,j->c}},"Vertices"]*)


(* ::Input:: *)
(*getObjectIndexMapping[{"Prefactor"->{1,{i$2506,i$2506},{i$2530,i$2530},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2529}|>,<|"type"->"nPoint","indices"->{i$2529,A[p1,mu,m],i$2530},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2530,i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},<|"Propagators"->{3,5,7},"Regulators"->{},"Vertices"->{4,6},"classicalVertices"->{2},"BRSTVertices"->{}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A[a$2393]->A,i$2529->A,i$2530->A,i$2505->A,i$2506->A,a$2422->A},{A[a$2393]->A,i$2529->A,i$2530->A,i$2505->A,i$2506->c,a$2422->cbar},{A[a$2393]->A,i$2529->A,i$2530->c,i$2505->cbar,i$2506->A,a$2422->A},{A[a$2393]->A,i$2529->A,i$2530->c,i$2505->cbar,i$2506->c,a$2422->cbar}},"Vertices"]*)


(* ::Input:: *)
(*getObjectIndexMapping[{"Prefactor"->{-1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,A[p1,mu,m],cbar[p2,n],i$2506},"nPoint"->4,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},<|"Propagators"->{3,5},"Regulators"->{},"Vertices"->{4},"classicalVertices"->{2},"BRSTVertices"->{}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A[a$2393]->A,i$2505->A,i$2506->A,a$2422->A},{A[a$2393]->A,i$2505->A,i$2506->c,a$2422->cbar},{A[a$2393]->A,i$2505->A,i$2506->cbar,a$2422->c}},"Vertices"]*)


(* ::Input:: *)
(*getObjectIndexMapping[{"Prefactor"->{-1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,A[p1,mu,m],cbar[p2,n],i$2506},"nPoint"->4,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},<|"Propagators"->{3,5},"Regulators"->{},"Vertices"->{4},"classicalVertices"->{2},"BRSTVertices"->{}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A[a$2393]->A,i$2505->A,i$2506->A,a$2422->A},{A[a$2393]->A,i$2505->A,i$2506->c,a$2422->cbar},{A[a$2393]->A,i$2505->A,i$2506->cbar,a$2422->c}},"Vertices"]*)


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



(* ::Input:: *)
(*getBRSTTruncation[{{A,A,A},{A,Q[A],c},{A,cbar,c}}]*)


(* ::Input:: *)
(*Append[{},{a}]*)
(*Append[{{a}},{b}]*)
(*MemberQ[StringMatchQ[Map[ToString,{A,Q[A],c}],"Q*"],True]*)


(* ::Input:: *)
(*TestBRSTIndexSummation = GetPossBRSTConfigs[AcmSTI[[1,1]]/.AcmSTI[[2]],fields, TestpositionBRST]*)


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


(* ::Input:: *)
(*SplitIndexList[{{cbar,i},{c,j},{cbar,l},{c,k}}]*)


(* ::Input:: *)
(*SplitIndexList[{{cbar,i},{cbar,j},{c,l},{c,k}}]*)


(* ::Input:: *)
(*SplitIndexList[{{cbar,i},{c,j},{cbar,l},{c,k},{A,m},{c,n}}]*)


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


(* ::Input:: *)
(*OrderByFieldTypes[{{cbar,i},{c,j}},1,fields]*)


(* ::Input:: *)
(*OrderByFieldTypes[{{c,i},{cbar,j}},1,fields]*)


(* ::Input:: *)
(*OrderByFieldTypes[{{cbar,j}},1,fields]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*fields[["BRSTsources",All,1]]*)


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




(* ::Input:: *)
(*JoinSplitLists[{{{cbar,i}}},{{cbar,j},{c,l}},1,fields]*)


(* ::Input:: *)
(*JoinSplitLists[{{{c,i}}},{{cbar,j},{c,l}},1,fields]*)


(* ::Input:: *)
(*JoinSplitLists[{{{cbar,i}},{{c,j}}},{{c,l},{cbar,k}},1,fields]*)


(* ::Input:: *)
(*JoinSplitLists[{{{cbar,i}},{{c,j}}},{{cbar,l},{c,k}},1,fields]*)


(* ::Input:: *)
(**)


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


(* ::Input:: *)
(*MergeIndexLists[{{{cbar,i},{c,j}},{{cbar,l},{c,k}}},1,fields]*)


(* ::Input:: *)
(*MergeIndexLists[{{{cbar,i},{c,j}},{{c,l},{cbar,k}}},1,fields]*)


(* ::Input:: *)
(*DefaultVertexOrdering[{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},{A[p1,mu,m]->A,c[-p1-p2,o]->c,cbar[p2,n]->cbar},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar},{A,Q[c]}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input:: *)
(*Head[A]*)


(* ::Input:: *)
(*DefaultVertexOrdering[{i$2529,A[p1,mu,m],i$2530},{a$2422->cbar,i$2505->A,i$2506->c,i$2529->A,i$2530->A,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar},{A,Q[c]}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar},{A,Q[c]}}]*)


(* ::Input:: *)
(*SortObject["classicalVertices",{A[a$2393],c[-p1-p2,o],a$2422},{a$2422->cbar,i$2505->A,i$2506->c,i$2529->A,i$2530->A,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input:: *)
(*List@@A[a$2393][[All]]*)
(*List@@c[-p1-p2,o]*)


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


(* ::Input:: *)
(*DefaultPropOrdering[{i,j},#,fields]&/@{{i->cbar,j->c},{i->c,j->cbar},{i->A,j->A}}*)


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


(* ::Input:: *)
(*SortObject["classicalVertices",{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},{A[p1,mu,m]->A,c[-p1-p2,o]->c,cbar[p2,n]->cbar},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input:: *)
(*SortObject["Propagators",{A[a$2393],i$2529},{a$2422->cbar,i$2505->A,i$2506->c,i$2529->A,i$2530->A,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input:: *)
(*SortObject["Propagators",{i$2530,i$2505},{a$2422->cbar,i$2505->A,i$2506->c,i$2529->A,i$2530->A,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input:: *)
(*SortObject["Vertices",{i$2529,A[p1,mu,m],i$2530},{a$2422->cbar,i$2505->A,i$2506->c,i$2529->A,i$2530->A,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input:: *)
(*SortOneDiag[{{"Prefactor"->{1,{A[p1,mu,m],c[-p1-p2,o]},{A[p1,mu,m],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},"nPoint"->3,"spec"->"classical"|>},*)
(**)
(*{"Prefactor"->{1,{i$2506,i$2506},{i$2530,i$2530},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2529}|>,<|"type"->"nPoint","indices"->{i$2529,A[p1,mu,m],i$2530},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2530,i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},*)
(**)
(*{"Prefactor"->{1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{i$2506,A[p1,mu,m]},{i$2506,A[p1,mu,m]},{i$2536,i$2536},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]},{cbar[p2,n],A[p1,mu,m]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,i$2535}|>,<|"type"->"nPoint","indices"->{i$2535,A[p1,mu,m],i$2536},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2536,a$2422}|>}*)
(**)
(*}[[#]],{{{A[p1,mu,m]->A,c[-p1-p2,o]->c,cbar[p2,n]->cbar}},{{a$2422->cbar,i$2505->A,i$2506->c,i$2529->A,i$2530->A,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar}},{{a$2422->cbar,i$2505->A,i$2506->c,i$2535->cbar,i$2536->c,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar}}}[[#,1]],<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{<|"Propagators"->{},"Regulators"->{},"Vertices"->{},"classicalVertices"->{2},"BRSTVertices"->{}|>,<|"Propagators"->{3,5},"Regulators"->{},"Vertices"->{4},"classicalVertices"->{2},"BRSTVertices"->{}|>}[[#]],{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]&/@{2}*)


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


(* ::Input:: *)
(*SortDiags[{"Prefactor"->{1,{A[p1,mu,m],c[-p1-p2,o]},{A[p1,mu,m],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},"nPoint"->3,"spec"->"classical"|>},{{A[p1,mu,m]->A,c[-p1-p2,o]->c,cbar[p2,n]->cbar}},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,<|"Propagators"->{},"Regulators"->{},"Vertices"->{},"classicalVertices"->{2},"BRSTVertices"->{}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input:: *)
(*SortOneDiag[{{"Prefactor"->{1,{A[p1,mu,m],c[-p1-p2,o]},{A[p1,mu,m],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},"nPoint"->3,"spec"->"classical"|>},*)
(**)
(*{"Prefactor"->{1,{i$2506,i$2506},{i$2530,i$2530},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2529}|>,<|"type"->"nPoint","indices"->{i$2529,A[p1,mu,m],i$2530},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2530,i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},*)
(**)
(*{"Prefactor"->{1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{i$2506,A[p1,mu,m]},{i$2506,A[p1,mu,m]},{i$2536,i$2536},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]},{cbar[p2,n],A[p1,mu,m]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,i$2535}|>,<|"type"->"nPoint","indices"->{i$2535,A[p1,mu,m],i$2536},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2536,a$2422}|>}*)
(**)
(*}[[#]],{{{A[p1,mu,m]->A,c[-p1-p2,o]->c,cbar[p2,n]->cbar}},{{a$2422->cbar,i$2505->A,i$2506->c,i$2529->A,i$2530->A,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar}},{{a$2422->cbar,i$2505->A,i$2506->c,i$2535->cbar,i$2536->c,A[a$2393]->A,A[p1,mu,m]->A,cbar[p2,n]->cbar}}}[[#]],<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{<|"Propagators"->{},"Regulators"->{},"Vertices"->{},"classicalVertices"->{2},"BRSTVertices"->{}|>,<|"Propagators"->{3,5},"Regulators"->{},"Vertices"->{4},"classicalVertices"->{2},"BRSTVertices"->{}|>}[[#]],{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]&/@{1}*)


(* ::Subsection::Closed:: *)
(*Signs*)


(* ::Input:: *)
(*GetSign[{"Prefactor"->{-1,1,{A[p1,mu,m],c[-p1-p2,o]},{A[p1,mu,m],cbar[p2,n]}},<|"type"->"nPoint","indices"->{{A,{p1,mu,m}},{cbar,{p2,n}},{c,{-p1-p2,o}}},"nPoint"->3,"spec"->"classical"|>}[[1]],<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>]*)


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


(* ::Input:: *)
(*GetSign[("Prefactor"->{-1,-1,1,1,1,1,{cbar,cbar},{cbar,cbar},{A,A},{A,c}}),fields]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Subsection:: *)
(*Trace over Fields*)


(* ::Input:: *)
(**)


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


(* ::Input:: *)
(*TraceOverFields[{{"Prefactor"->{1,{A[p1,mu,m],c[-p1-p2,o]},{A[p1,mu,m],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},"nPoint"->3,"spec"->"classical"|>},{"Prefactor"->{1,{i$2506,i$2506},{i$2530,i$2530},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2529}|>,<|"type"->"nPoint","indices"->{i$2529,A[p1,mu,m],i$2530},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2530,i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},{"Prefactor"->{-1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,A[p1,mu,m],cbar[p2,n],i$2506},"nPoint"->4,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},{"Prefactor"->{1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{i$2506,A[p1,mu,m]},{i$2506,A[p1,mu,m]},{i$2536,i$2536},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]},{cbar[p2,n],A[p1,mu,m]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,i$2535}|>,<|"type"->"nPoint","indices"->{i$2535,A[p1,mu,m],i$2536},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2536,a$2422}|>}},{A[p1,mu,m],cbar[p2,n]},{cbar[p2,n]->cbar[p2,n],cbar[p2,n]->cbar[p2,n],cbar[p2,n]->cbar[p2,n],A[p1,mu,m]->A[p1,mu,m],A[p1,mu,m]->A[p1,mu,m],A[p1,mu,m]->A[p1,mu,m]},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*traceSingleDiagram[{{"Prefactor"->{1,{A[p1,mu,m],c[-p1-p2,o]},{A[p1,mu,m],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},"nPoint"->3,"spec"->"classical"|>},{"Prefactor"->{1,{i$2506,i$2506},{i$2530,i$2530},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2529}|>,<|"type"->"nPoint","indices"->{i$2529,A[p1,mu,m],i$2530},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2530,i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},{"Prefactor"->{-1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,A[p1,mu,m],cbar[p2,n],i$2506},"nPoint"->4,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},{"Prefactor"->{1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{i$2506,A[p1,mu,m]},{i$2506,A[p1,mu,m]},{i$2536,i$2536},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]},{cbar[p2,n],A[p1,mu,m]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,i$2535}|>,<|"type"->"nPoint","indices"->{i$2535,A[p1,mu,m],i$2536},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2536,a$2422}|>}}[[#]],{A[p1,mu,m],cbar[p2,n]},{cbar[p2,n]->cbar[p2,n],cbar[p2,n]->cbar[p2,n],cbar[p2,n]->cbar[p2,n],A[p1,mu,m]->A[p1,mu,m],A[p1,mu,m]->A[p1,mu,m],A[p1,mu,m]->A[p1,mu,m]},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]&/@{1}*)


(* ::Input:: *)
(*traceSingleDiagram[{{"Prefactor"->{1,{A[p1,mu,m],c[-p1-p2,o]},{A[p1,mu,m],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[p1,mu,m],c[-p1-p2,o],cbar[p2,n]},"nPoint"->3,"spec"->"classical"|>},*)
(**)
(*{"Prefactor"->{1,{i$2506,i$2506},{i$2530,i$2530},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2529}|>,<|"type"->"nPoint","indices"->{i$2529,A[p1,mu,m],i$2530},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2530,i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},*)
(**)
(*{"Prefactor"->{-1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,A[p1,mu,m],cbar[p2,n],i$2506},"nPoint"->4,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,a$2422}|>},*)
(**)
(*{"Prefactor"->{1,{i$2505,A[p1,mu,m]},{i$2505,A[p1,mu,m]},{i$2506,i$2506},{i$2506,A[p1,mu,m]},{i$2506,A[p1,mu,m]},{i$2536,i$2536},{A[a$2393],A[p1,mu,m]},{A[a$2393],c[-p1-p2,o]},{A[a$2393],cbar[p2,n]},{cbar[p2,n],A[p1,mu,m]}},<|"type"->"nPoint","indices"->{A[a$2393],c[-p1-p2,o],a$2422},"nPoint"->3,"spec"->"classical"|>,<|"type"->"Propagator","indices"->{A[a$2393],i$2505}|>,<|"type"->"nPoint","indices"->{i$2505,cbar[p2,n],i$2506},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2506,i$2535}|>,<|"type"->"nPoint","indices"->{i$2535,A[p1,mu,m],i$2536},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{i$2536,a$2422}|>}*)
(**)
(*}[[#]],{A[p1,mu,m],cbar[p2,n]},{cbar[p2,n]->cbar[p2,n],cbar[p2,n]->cbar[p2,n],cbar[p2,n]->cbar[p2,n],A[p1,mu,m]->A[p1,mu,m],A[p1,mu,m]->A[p1,mu,m],A[p1,mu,m]->A[p1,mu,m]},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}},{{A,A},{c,cbar},{A,A,A},{A,A,A,A},{A,c,cbar}}]&/@Range[4]*)


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


(* ::Subtitle:: *)
(*End*)


(* ::Input::Initialization:: *)
End[]
EndPackage[]
