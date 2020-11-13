(* ::Package:: *)

(* ::Chapter:: *)
(*Insert Feynman rules and get momentum rooting*)


(* ::Subsection:: *)
(*Aux Functions*)


(* ::Input::Initialization:: *)
BeginPackage["FullDiagrams`"]


(* ::Input::Initialization:: *)
Begin["Private`"]


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


(* ::Input:: *)
(*FindFirstVertex[{"Prefactor"->{-(1/2)},<|"type"->"Propagator","indices"->{{c,i},{cbar,i$3391}}|>,<|"type"->"nPoint","indices"->{{A,iext$3389},{cbar,i$3391},{c,i$3392}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,i$3392},{cbar,i$3408}}|>,<|"type"->"nPoint","indices"->{{A,iext$3400},{cbar,i$3408},{c,i$3409}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,i$3409},{cbar,j}}|>,<|"type"->"Regulatordot","indices"->{{cbar,j},{c,i}}|>},iext$3389]*)


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


(* ::Input:: *)
(*SuperindexRules[{{c,i},{cbar,i$3391}},fields]*)


(* ::Input:: *)
(*SuperindexRules[{{A,i},{A,i$3391}},fields]*)


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


(* ::Input:: *)
(*FindPositionNewMomentum[{-p-q,p$138684},{-p,p},{q$_ n$_?NumericQ->q},q]*)


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


(* ::Input:: *)
(*FixMomentum[<|"type"->"Propagator","indices"->{{A,{p$87607,{mu$87608,i$87608}}},{A,{p$87609,{mu$87610,i$87610}}}}|>,2,{-q,p$87609},{p$87607->-q},{q$_ n$_?NumericQ->q}]*)


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


(* ::Input:: *)
(*MomentumConservationVert[{<|"type"->"Propagator","indices"->{{c,{p$138677,i$138678}},{cbar,{p$138679,i$138680}}}|>,<|"type"->"nPoint","indices"->{{A,{p,nu,n}},{cbar,{p$138679,i$138680}},{c,{p$138682,i$138683}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{p$138682,i$138683}},{cbar,{p$138684,i$138685}}}|>,<|"type"->"nPoint","indices"->{{A,{-p,mu,m}},{cbar,{p$138684,i$138685}},{c,{p$138687,i$138688}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{p$138687,i$138688}},{cbar,{p$138689,i$138690}}}|>,<|"type"->"Regulatordot","indices"->{{cbar,{p$138689,i$138690}},{c,{p$138677,i$138678}}}|>},{iext$138399->{-p,mu,m},iext$138410->{p,nu,n}},4,q]*)


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


(* ::Input:: *)
(*MomentumConservationBRST[{<|"type"->"Propagator","indices"->{{c,{p$138677,i$138678}},{cbar,{p$138679,i$138680}}}|>,<|"type"->"nPoint","indices"->{{A,{p,nu,n}},{cbar,{p$138679,i$138680}},{c,{p$138682,i$138683}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{p$138682,i$138683}},{cbar,{p$138684,i$138685}}}|>,<|"type"->"nPoint","indices"->{{A,{-p,mu,m}},{cbar,{p$138684,i$138685}},{c,{p$138687,i$138688}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{p$138687,i$138688}},{cbar,{p$138689,i$138690}}}|>,<|"type"->"Regulatordot","indices"->{{cbar,{p$138689,i$138690}},{c,{p$138677,i$138678}}}|>},{iext$138399->{-p,mu,m},iext$138410->{p,nu,n}},4,q]*)


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


(* ::Input:: *)
(*ShiftLoopMomentum[{<|"type"->"Regulatordot","indices"->{{A,{-q-p,{mu$205772,i$205772}}},{A,{q+p,{mu$205784,i$205784}}}}|>},q]*)


(* ::Input:: *)
(*ShiftLoopMomentum[{<|"type"->"Regulatordot","indices"->{{A,{-q,{mu$230216,i$230216}}},{A,{q,{mu$230228,i$230228}}}}|>},q]*)


(* ::Input:: *)
(**)


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


(* ::Input:: *)
(*{<|"type"->"Regulatordot","indices"->{{A,{p$258779,{mu$258780,i$258780}}},{A,{p$258791,{mu$258792,i$258792}}}}|>}[[1,"indices",All,2,1]]*)


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


(* ::Subsection:: *)
(*Helper*)


(* ::Input::Initialization:: *)
Clear[ReplaceOneIndex]
ReplaceOneIndex[field_,indices_,superindexReplacementList_,fields_] := Module[{newfield,posfield,newIndices},
If[MemberQ[superindexReplacementList[[All,2]],indices],
Return[{Nothing}]
,
If[ToString[Head[field]]=="Q",
newfield = field[[1]];
,
newfield = field;
];

posfield = Position[fields,newfield[___]];

newIndices = Map[Unique,List@@(Extract[fields,posfield[[1]]])];

Return[{indices->newIndices}]
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

newRule = Solve[(Total[newMomList]+2*Private`q)==0, openMomIndexnew];

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


(* ::Input:: *)
(*shiftLoopFromBRST[{<|"type"->"Regulator","indices"->{{A,{p$68351,{mu$68352,i$68352}}},{A,{p$68354,{mu$68355,i$68355}}}}|>,<|"type"->"Propagator","indices"->{{A,{p$68358,{mu$68359,i$68359}}},{A,{p$68354,{mu$68355,i$68355}}}}|>,<|"type"->"nPoint","indices"->{{A,{p$68358,{mu$68359,i$68359}}},{cbar,{p$68364,i$68365}},{c,{p,n}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{p$68369,i$68370}},{cbar,{p$68364,i$68365}}}|>,<|"type"->"nPoint","indices"->{{A,{-p,mu,m}},{c,{p$68369,i$68370}},{Q[A],{p$68351,{mu$68352,i$68352}}}},"nPoint"->3,"spec"->"BRST"|>},<|"Propagators"->{2,4},"Regulators"->{1},"Vertices"->{3},"classicalVertices"->{},"BRSTVertices"->{5}|>,Private`q]*)


(* ::Input::Initialization:: *)
Clear[shiftLoopFromFirstVertex]
shiftLoopFromFirstVertex[Diag_,objectPositionAssoc_,derivativeList_,loopIndex_] := Module[{posfirstDer,direction, iter = 2,loopDirIndex,momConsList,momConsListnew,distanceright,distanceleft},
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
direction = Join[Range[posfirstDer[[1]]+1,Length@Diag],Range[1,posfirstDer[[1]]-1]];
,
If[distanceright == distanceleft,
direction = Join[Range[posfirstDer[[1]]+1,Length@Diag],Range[1,posfirstDer[[1]]-1]];
,
If[distanceright(*<*)>distanceleft,
direction =Join[Range[posfirstDer[[1]]-1,1,-1],Range[Length@Diag,posfirstDer[[1]]+1,-1]];
,
direction =Join[Range[posfirstDer[[1]]-1,1,-1],Range[Length@Diag,posfirstDer[[1]]+1,-1]];
];
];
];

loopDirIndex = FindMatchingIndex[Diag[[posfirstDer[[1]]]],Diag[[direction[[1]]]]];

momConsList = {loopDirIndex->-loopIndex};



momConsListnew = conserveOneObjectMomentum[Diag[[posfirstDer[[1]]]],momConsList,loopIndex];



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


(* ::Input:: *)
(*{4}[[1]]*)


(* ::Input:: *)
(*<|"type"->"Regulator","indices"->{{A,{p$68351,{mu$68352,i$68352}}},{A,{p$68354,{mu$68355,i$68355}}}}|>[["indices",All,2,1]]*)


(* ::Input:: *)
(*shiftLoopFromFirstVertex[{<|"type"->"Regulator","indices"->{{A,{p$23,{mu$23,i$23}}},{A,{p$68354,{mu$68355,i$68355}}}}|>,<|"type"->"Propagator","indices"->{{A,{p$68358,{mu$68359,i$68359}}},{A,{p$68354,{mu$68355,i$68355}}}}|>,<|"type"->"nPoint","indices"->{{A,{p$68358,{mu$68359,i$68359}}},{cbar,{p$68364,i$68365}},{c,{p,{n}}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{p$68369,i$68370}},{cbar,{p$68364,i$68365}}}|>,<|"type"->"nPoint","indices"->{{A,{p$68351,{mu$68352,i$68352}}},{cbar,{-p,{m}}},{c,{p$68369,i$68370}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{A,{p$68351,{mu$68352,i$68352}}},{A,{p$23,{mu$23,i$23}}}}|>},<|"Propagators"->{2,4,6},"Regulators"->{1},"Vertices"->{3,5},"classicalVertices"->{},"BRSTVertices"->{}|>,{cbar[-p,{m}],c[p,{n}]},Private`q]*)
(**)


(* ::Input:: *)
(*ReplaceObjectsWithFunctions[{<|"type"->"Regulator","indices"->{{A,{p$23,{mu$23,i$23}}},{A,{p$68354,{mu$68355,i$68355}}}}|>,<|"type"->"Propagator","indices"->{{A,{p$68358,{mu$68359,i$68359}}},{A,{p$68354,{mu$68355,i$68355}}}}|>,<|"type"->"nPoint","indices"->{{A,{p$68358,{mu$68359,i$68359}}},{cbar,{p$68364,i$68365}},{c,{p,{n}}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{p$68369,i$68370}},{cbar,{p$68364,i$68365}}}|>,<|"type"->"nPoint","indices"->{{A,{p$68351,{mu$68352,i$68352}}},{cbar,{-p,{m}}},{c,{p$68369,i$68370}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{A,{p$68351,{mu$68352,i$68352}}},{A,{p$23,{mu$23,i$23}}}}|>}]*)


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


(* ::Input:: *)
(*replaceIndicesBRST[{{Q[A],{p$68351,{mu$68352,i$68352}}},{cbar,{-p,{m}}},{c,{p$68369,i$68370}}}]*)
(*replaceIndicesBRST[{{Q[c],{i}},{c,{l}},{c,{-p,{n}}}}]*)


(* ::Input:: *)
(*ReplaceObjectsWithFunctions[{<|"type"->"Regulator","indices"->{{A,{p$23,{mu$23,i$23}}},{A,{p$68354,{mu$68355,i$68355}}}}|>,<|"type"->"Propagator","indices"->{{A,{p$68358,{mu$68359,i$68359}}},{A,{p$68354,{mu$68355,i$68355}}}}|>,<|"type"->"nPoint","indices"->{{A,{p$68358,{mu$68359,i$68359}}},{cbar,{p$68364,i$68365}},{c,{p,{n}}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{p$68369,i$68370}},{cbar,{p$68364,i$68365}}}|>,<|"type"->"nPoint","indices"->{{Q[A],{p$68351,{mu$68352,i$68352}}},{cbar,{-p,{m}}},{c,{p$68369,i$68370}}},"nPoint"->3,"spec"->"BRST"|>,<|"type"->"Propagator","indices"->{{A,{p$68351,{mu$68352,i$68352}}},{A,{p$23,{mu$23,i$23}}}}|>}]*)


(* ::Input:: *)
(*StringMatchQ[ToString[Q[A]],"Q"~~__]*)
(*Head[({{Q[A],{p$68351,{mu$68352,i$68352}}},{cbar,{-p,{m}}},{c,{p$68369,i$68370}}}[[1,1]])]*)
(*(List@@({{Q[A],{p$68351,{mu$68352,i$68352}}},{cbar,{-p,{m}}},{c,{p$68369,i$68370}}}[[1,1]]))[[1]]*)


(* ::Subsection:: *)
(*Main Function*)


(* ::Input:: *)
(*InsertFeynRules[{"Prefactor"->{1},<|"type"->"Regulator","indices"->{{A,{i}},{A,{j}}}|>,<|"type"->"Propagator","indices"->{{A,{i$2392}},{A,{j}}}|>,<|"type"->"nPoint","indices"->{{A,{i$2392}},{cbar,{i$2393}},{c,{p,n}}},"nPoint"->3,"spec"->"none"|>,<|"type"->"Propagator","indices"->{{c,{l}},{cbar,{i$2393}}}|>,<|"type"->"nPoint","indices"->{{A,{-p,mu,m}},{c,{l}},{Q[A],{i}}},"nPoint"->3,"spec"->"BRST"|>},{A[-p,mu,m],c[p,n]},{c[p,n]->{p,n},A[-p,mu,m]->{-p,mu,m},A[-p,mu,m]->{-p,mu,m}},<|"bosonic"->{A[p,{mu,i}]},"fermionic"->{{cbar[p,i],c[p,i]}},"BRSTsources"->{{Q[A],"fermionic"},{Q[cbar],"bosonic"},{Q[c],"bosonic"}}|>,Private`q]*)


(* ::Input:: *)
(*(*Add loop index as option*)*)


(* ::Input:: *)
(*({{A[-p,mu,m],c[p,n]}[[#]]->List@@({A[-p,mu,m],c[p,n]}[[#]])})&/@{1,2}*)


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
Echo["more than one loop"];
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


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
Clear[InsertFeynRulesAllDiags] (*takes as RHS one single diagram*)
InsertFeynRulesAllDiags[allDiags_,derivativeList_,fields_,loopIndex_] := Module[{allVars = Table[{},Length@allDiags], fullDiags = Table[Null,Length@allDiags]},



(
myEcho[{"Insert Feynman Rules in Diagram no: ",#},1];

{allVars[[#]],fullDiags[[#]]} = InsertFeynRules[allDiags[[#]],derivativeList,fields,loopIndex];

)&/@Table[i,{i,1,Length@allDiags}];

Return[{allVars,fullDiags}]

]


(* ::Input::Initialization:: *)
End[]
EndPackage[]
