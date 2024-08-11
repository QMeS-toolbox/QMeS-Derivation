(* ::Package:: *)

(* ::Chapter:: *)
(*Derive Functional Equation Tools*)


(* ::Input::Initialization:: *)
BeginPackage["QMeSderivation`Tools`"]


(* ::Section:: *)
(*Exports*)


(* ::Input::Initialization:: *)
ReduceIdenticalFlowDiagrams::usage = "ReduceIdenticalFlowDiagrams[diagrams,derivativeList,symmetries]

Reduce a set of flow equations by matching identical topologies and/or by utilizing a given set of symmetries.
The user has to supply the list of derivatives (i.e. external legs in the diagrams). Then, the symmetries are specified in the following way:
symmetries = {
	{ {1, 3}, Minus},
	{ {2, 4}, Minus}
	{ {1, 3}, {2, 4}, Plus},
}
Here, two antisymmetries (as specified by the Minus) are given, which consist of a single cycle each (1,3) and (2,4). These integers refer to the legs as given in derivativeList (in order).
ReduceIdenticalFlowDiagrams does not construct all symmetries from the given ones - therefore, the full symmetry group needs to be specified. Therefore, we also supply the symmetry consisting of both cycles explicitly in the third line.
";


(* ::Input::Initialization:: *)
PlotSuperindexDiagram::usage = "PlotSuperindexDiagram[diagram, setup, options]

Plot one or multiple superindex diagrams. The first argument to this function are the diagram(s), the second one is the corresponding QMeS setup.
The diagram(s) have to be superindex diagram(s).

The options for the function PlotSuperindexDiagram are:
\"ShowEdgeLabels\" ->  False / True
	This option will toggle whether edges are plotted together with labels to identify them.
\"EdgeStyle\" ->  List
	Using this option, one can specify the edge styles of different propagators. e.g.,
		\"EdgeStyle\"->{q->Blue,A->Orange,\[CapitalPi]->{Dashed,Thick},\[Sigma]->{Dashed,Thick,Purple}}.
";


(* ::Input::Initialization:: *)
RerouteFermionicMomenta::usage="RerouteFermionicMomenta[diagrams, setup, derivativeList]

Reroute external fermionic momenta in diagrams such that Matsubara frequencies are automatically correctly routed.
If a closed fermion loop is present, the momentum in this loop is marked by attaching the character 'f' to it (i.e. q -> qf in such a loop).
The first argument to this function is a list of diagrams, the second one is the corresponding QMeS setup. The third argument is the derivative list that has been used to derive the set of diagrams.
Be aware, that the rerouting exploits momentum conservation - therefore, one of the external momenta may be eliminated if this is not yet the case.
"


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Section:: *)
(*Debug*)


(* ::Input::Initialization:: *)
$DebugLevel =0;


(* ::Input::Initialization:: *)
myEcho[msg_,lvl_] := If[$DebugLevel >=lvl, Echo[msg];, Nothing;]


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*Tests and assertions*)


(* ::Input::Initialization:: *)
TestIsSuperindexDiagram[diag_]:=Module[{hasPrefactor,hasAssociations},
If[Not[ListQ[diag]&&Length[diag]>1],Return[False]];
hasAssociations=AllTrue[Map[AssociationQ,Flatten@diag[[2;;]]],Identity]&&Length[Flatten@diag[[2;;]]]>0;
hasPrefactor=Extract[diag[[1]],1]=="Prefactor";
Return[hasAssociations&&hasPrefactor]
]
AssertIsSuperindexDiagram[diag_,context_String:""]:=If[Not@TestIsSuperindexDiagram[diag],Print[context," Argument is not a SuperindexDiagram."];Abort[]];
AssertAllSuperindexDiagrams[diags_List,context_String:""]:=Map[AssertIsSuperindexDiagram[#,context]&,diags];


(* ::Input::Initialization:: *)
TestIsOneLoop[diag_]:=Module[{internalIndices,objectIndices,assignNumber,mapAssignNumber,testNumbers},
AssertIsSuperindexDiagram[diag,"TestIsOneLoop:"];

(*An expression is one-loop if every object (vertex or propagator) has only one incoming and one outgoing index.
Therefore, what we do here is to check the number of internal indices of any object in the diagram and testing whether this is exactly 2 for all objects.*)

internalIndices=GetInternalIndices[diag];
objectIndices=Map[#["indices"]&,diag[[2;;]]];

assignNumber=If[MemberQ[internalIndices,#],1,0]&;
mapAssignNumber=Map[assignNumber,#]&;
testNumbers=Map[Total[mapAssignNumber[#]]&,objectIndices];
Return[AllTrue[testNumbers,#==2&]]
];
AssertIsOneLoop[diag_List,context_String:""]:=If[Not@TestIsOneLoop[diag],Print[context," Diagram is not one-loop."];Abort[]];

TestIsFlowDiagram[diag_]:=Module[{content,regulatordotMatch},
content=SuperIndexDiagramContent[diag];
regulatordotMatch=Select[content,StringMatchQ[#,"Regulatordot"~~___]&];
If[Length[regulatordotMatch]==1,True,False]
];
AssertIsFlowDiagram[diag_List,context_String:""]:=If[Not@TestIsFlowDiagram[diag],Print[context," Diagram is not a flow diagram."];Abort[]];


(* ::Input::Initialization:: *)
TestIsFullDiagram[diag_/;Head[diag]=!=List]:=Module[{extr,symbols},
If[Head[diag]===Association||Head[diag]===Rule,Return[False]];
extr=diag/.h_[{a___}]:>h;
symbols=GetAllSymbols[extr];
AllTrue[symbols,
(StringPart[ToString[#],1]=="G")||(StringPart[ToString[#],1]=="\[CapitalGamma]")||(StringPart[ToString[#],1]=="R")&
]
];
TestIsFullDiagram[diag_List]:=AllTrue[diag,TestIsFullDiagram];
AssertIsFullDiagram[diag_,context_String:""]:=If[Not@TestIsFullDiagram[diag],Print[context," Argument is not a FullDiagram."];Abort[]];


(* ::Subsection::Closed:: *)
(*Getting objects*)


(* ::Input::Initialization:: *)
GetIndices[diag_List]:=Module[{},
AssertIsSuperindexDiagram[diag,"TestIsOneLoop:"];
Flatten[Map[#["indices"]&,diag[[2;;]]],1]
];
GetExternalIndices[diag_List]:=Module[{allIndices},
AssertIsSuperindexDiagram[diag,"TestIsOneLoop:"];
allIndices=GetIndices[diag];
Keys@Select[Counts[allIndices],#==1&]
];
GetInternalIndices[diag_List]:=Module[{allIndices},
AssertIsSuperindexDiagram[diag,"TestIsOneLoop:"];
allIndices=GetIndices[diag];
Keys@Select[Counts[allIndices],#>1&]
];

GetIndices[obj_Association]:=Module[{},
obj["indices"]
];
GetExternalIndices[diag_List,obj_Association]:=Module[{allIndices,externalIndices},
allIndices=GetIndices[obj];
externalIndices=GetExternalIndices[diag];
Select[allIndices,MemberQ[externalIndices,#]&]
];
GetInternalIndices[diag_List,obj_Association]:=Module[{allIndices,internalIndices},
AssertIsSuperindexDiagram[diag,"TestIsOneLoop:"];
allIndices=GetIndices[obj];
internalIndices=GetInternalIndices[diag];
Select[allIndices,MemberQ[internalIndices,#]&]
];


(* ::Input::Initialization:: *)
GetObjectIdentifier[ob_Association]:=Module[{},
ob["type"]<>StringJoin[Map[ToString[#[[1]]]&,ob["indices"]]]
];

GetRegulatordot[diag_]:=Module[{},
AssertIsFlowDiagram[diag,"GetRegulatordot:"];
Select[diag,MemberQ[#,"Regulatordot",Infinity]&][[1]]
];


(* ::Input::Initialization:: *)
GetBosons[setup_Association]:=Map[Head,setup["FieldSpace"]["bosonic"]];
GetFermionPairs[setup_Association]:=Map[{Head[#[[1]]],Head[#[[2]]]}&,setup["FieldSpace"]["fermionic"]];
GetFermions[setup_Association]:=Map[Head[#[[2]]]&,setup["FieldSpace"]["fermionic"]];
GetAntiFermions[setup_Association]:=Map[Head[#[[1]]]&,setup["FieldSpace"]["fermionic"]];


(* ::Input::Initialization:: *)
GetPartnerField[field_,setup_]:=Module[{fermions,bosons,sel},
fermions=GetFermionPairs[setup];
bosons=GetBosons[setup];

If[MemberQ[fermions,field,Infinity],
sel=Select[fermions,MemberQ[#,field,Infinity]&][[1]];
sel=DeleteCases[sel,field];
Return[sel[[1]]];
];

If[MemberQ[bosons,field,Infinity],
Return[field]
];

Print["field ",field," not found!"];
Abort[];
];


(* ::Input::Initialization:: *)
GetAllSymbols[expr_]:=DeleteDuplicates@Cases[{expr},_Symbol,Infinity]


(* ::Section:: *)
(*Diagram reduction*)


(* ::Subsection::Closed:: *)
(*Diagram grouping*)


(* ::Input::Initialization:: *)
SuperIndexDiagramContent[diag_]:=Module[{},
AssertIsSuperindexDiagram[diag,"SuperIndexDiagramContent:"];
Sort[Map[GetObjectIdentifier,diag[[2;;]]]]
];
SeparateSuperIndexDiagramGroups[diags_List]:=Module[{identifierRep,removeFirsts,groupedDiagrams},
AssertAllSuperIndexDiagrams[diags,"SeparateSubdiagramGroups:"];

(*We group all diagrams into groups that could be potentially identical. 
We simply make sure that in each group all diagrams have the same objects, ignoring topology.*)
identifierRep=Map[SuperIndexDiagramContent,diags];
identifierRep=Thread[{identifierRep,diags}];
removeFirsts[ex_]:=Map[#[[2]]&,ex];
groupedDiagrams=Map[removeFirsts,GatherBy[identifierRep,#[[1]]&]];

Return[groupedDiagrams]
];


(* ::Subsection::Closed:: *)
(*Comparison of two diagrams*)


(* ::Input::Initialization:: *)
IterateDiagram[diag_,curPos_,entryIdx_]:=Module[{curIdxs,exitIdx,nextPos,externalIndices,i,candidates},
(*Given a superindex diagram diag, find the entry (curPos) in the diagram. Then, remove the entryIdx from this entry, grab the next index (exitIxd),
  and find the next entry (nextPos) which is connected by this exitIdx.*)

curIdxs=curPos["indices"];
If[Not@MemberQ[curIdxs,entryIdx],Print["IterateDiagram: Invalid entryIdx"];Abort[]];
externalIndices=GetExternalIndices[diag];
For[i=1,i<=Length[externalIndices],i++,curIdxs=DeleteCases[curIdxs,externalIndices[[i]]]];
exitIdx=DeleteCases[curIdxs,entryIdx][[1]];

candidates=Select[diag[[2;;]],MemberQ[#,exitIdx,Infinity]&];
If[Length[candidates]!=2,Print["IterateDiagram: Logic Error"];Abort[]];
nextPos=DeleteCases[candidates,curPos][[1]];

Return[{nextPos,exitIdx}];
];

CheckOneLoopDiagramIdentity[indiag1_,indiag2_,symmetryList_List:{}]:=Module[{startPos,CheckIteration,entryIdx,diag1,diag2,symmidx,sign},
(*Given two one-loop diagrams, start in both at the regulator. 
Then, step through the diagrams (at the same time) in one direction and compare whether they are identical.
Do this several times: Both directions and utilizing (one after the other) all symmetry transformations (symmetries).
If any of these procedures matches perfectly, return True. Otherwise, return always False. 
*)
AssertIsOneLoop[indiag1,"CheckDiagramIdentity:"];
AssertIsOneLoop[indiag2,"CheckDiagramIdentity:"];

CheckIteration[entry_]:=Module[{curPos,curIdx,iter,externalIndices={0,0},objectIdentifiers={"",""}},
curPos=startPos;
curIdx=entry;
iter=0;


While[(iter==0||curPos!=startPos)&&iter<50,

externalIndices[[1]]=GetExternalIndices[diag1,curPos[[1]]];
externalIndices[[2]]=GetExternalIndices[diag2,curPos[[2]]];

objectIdentifiers[[1]]=GetObjectIdentifier[curPos[[1]]];
objectIdentifiers[[2]]=GetObjectIdentifier[curPos[[2]]];

If[externalIndices[[1]]=!=externalIndices[[2]],myEcho["Diff: ",externalIndices[[1]]," vs ",externalIndices[[2]],2];Return[False]];
If[objectIdentifiers[[1]]=!=objectIdentifiers[[2]],myEcho["Diff: ",objectIdentifiers[[1]]," vs ",objectIdentifiers[[2]],2];Return[False]];

{curPos[[1]],curIdx[[1]]}=IterateDiagram[diag1,curPos[[1]],curIdx[[1]]];
{curPos[[2]],curIdx[[2]]}=IterateDiagram[diag2,curPos[[2]],curIdx[[2]]];
iter++;
];
If[iter>=50,Return[False]];

Return[True]
];

For[symmidx=0,symmidx<=Length[symmetryList],symmidx++,
diag1=indiag1;
If[symmidx>0,diag2=indiag2/.symmetryList[[symmidx]]["Rule"],diag2=indiag2];
If[symmidx>0,
sign=If[symmetryList[[symmidx]]["Sign"]===Plus,1,-1],
sign=1];

startPos=Map[GetRegulatordot,{diag1,diag2}];

entryIdx=Map[#["indices"][[1]]&,startPos];
If[entryIdx[[1,1]]==entryIdx[[2,1]],
If[CheckIteration[entryIdx],Return[{True,sign}]];
];

entryIdx[[2]]=startPos[[2]]["indices"][[2]];
If[entryIdx[[1,1]]==entryIdx[[2,1]],
If[CheckIteration[entryIdx],Return[{True,sign}]];
];
];

Return[{False,0}]
];


(* ::Subsection::Closed:: *)
(*Diagram set reduction*)


(* ::Input::Initialization:: *)
BuildSymmetryList[symmetries_,derivativeList_]:=Module[{procDerList,buildOneSymmetry},
If[Head[symmetries]=!=List,Print["Symmetries must be given as a list!"];Abort[]];
If[Length[symmetries]==0,Return[{}]];
If[Length[derivativeList]==0,Return[{}]];

procDerList=Map[{Head[#],List@@#}&,derivativeList];

buildOneSymmetry[sym_]:=Module[{valid=True,buildCycle,pairs},
If[sym[[-1]]=!=Plus&&sym[[-1]]=!=Minus,valid=False];
If[AnyTrue[sym[[;;-2]],Not[Head[#]===List]&],valid=False];
pairs=Subsets[sym[[;;-2]],{2}];
valid=Not@AnyTrue[Map[ContainsAny[#[[1]],#[[2]]]&,pairs],Identity];
If[Not@valid,Print[sym," is  not a valid symmetry!"];Abort[]];

buildCycle[cyc_]:=Module[{cycvalid=True,numberRules,idx,nextIdx},
If[AnyTrue[cyc,Not[IntegerQ[#]]&],cycvalid=False];
If[AnyTrue[cyc,(#>Length[derivativeList])||(#<1)&],cycvalid=False];
If[Not@cycvalid,Print[cyc," is  not a valid cycle!"];Abort[]];

numberRules={};
For[idx=1,idx<=Length[cyc],idx++,
nextIdx=Mod[(idx),Length[cyc]]+1;
numberRules=Join[numberRules,{{cyc[[idx]],cyc[[nextIdx]]}}];
];

Return[Map[procDerList[[#[[1]]]]->procDerList[[#[[2]]]]&,numberRules]];
];

<|
"Rule"->Flatten[Map[buildCycle,sym[[;;-2]]],1],
"Sign"->sym[[-1]]
|>
];

Map[buildOneSymmetry,symmetries]
];


(* ::Input::Initialization:: *)
ReduceIdenticalFlowDiagrams[diags_,derivativeList_:{},symmetries_List:{}]:=Module[{separatedGroups,reduceGroup,reducedGroups,symmetryList},
AssertAllSuperIndexDiagrams[diags,"ReduceIdenticalFlowDiagrams"];

separatedGroups=SeparateSuperIndexDiagramGroups[diags];
symmetryList=BuildSymmetryList[symmetries,derivativeList];

reduceGroup[groupDiags_]:=Module[{i,j,multiplicities,identitiesSigns,signs,identities,prefactors,newDiags},
multiplicities=Table[1,{i,Length[groupDiags]}];
For[i=1,i<=Length[groupDiags],i++,
If[multiplicities[[i]]>0,
identitiesSigns=Table[{False,0},{k,i+1,Length[groupDiags]}];
For[j=i+1,j<=Length[groupDiags],j++,
If[multiplicities[[j]]>0,
identitiesSigns[[j-i]]=CheckOneLoopDiagramIdentity[groupDiags[[i]],groupDiags[[j]],symmetryList];
];
];
signs=Map[If[#[[1]],#[[2]],0]&,identitiesSigns];
identities=Map[If[#[[1]],1,0]&,identitiesSigns];
identities=Join[
Table[0,{j,i-1}],
{Total[signs]},
-identities];
multiplicities=multiplicities+identities;
];
];

prefactors=Map["Prefactor"/.#[[1]]&,groupDiags];
prefactors=multiplicities*prefactors;

newDiags=groupDiags;

For[i=1,i<=Length[groupDiags],i++,
newDiags[[i]][[1]]=("Prefactor"->prefactors[[i]]);
];
newDiags=Select[newDiags,("Prefactor"/.#[[1]])=!={0}&];

Return[newDiags];
];

reducedGroups=ParallelMap[reduceGroup,separatedGroups];

Return[Flatten[reducedGroups,1]];
];



(* ::Input:: *)
(*Diagramsqbqqbq=Select[Import["./diagramsqbqqbq.m"],Not[MemberQ[#,\[CapitalPi],Infinity]]&&Not[MemberQ[#,\[Sigma],Infinity]]&];*)
(*DerivativeListqbqqbq= {qb[p4,{d4,c4,f4}],q[p3,{d3,c3,f3}],qb[p2,{d2,c2,f2}],q[p1,{p1,c1,f1}]};*)
(*Diagramsqbqqbq//Length*)
(*symm={*)
(*{{1,3},Plus},*)
(*{{2,4},Plus},*)
(*{{1,3},{2,4},Plus}*)
(*};*)
(*res=ReduceIdenticalFlowDiagrams[Diagramsqbqqbq,DerivativeListqbqqbq,symm];*)
(*PlotSuperindexDiagrams[res,SetupfRG]*)


(* ::Section::Closed:: *)
(*Diagram drawing*)


(* ::Input::Initialization:: *)
GetEdgeRule[obj_,fields_,setup_]:=Module[{fermions,bosons,sel},
If[Length[obj]!=Length[fields]||Length[obj]!=2,Print["Mismatch!"];Abort[]];
fermions=GetFermionPairs[setup];
bosons=GetBosons[setup];

If[MemberQ[fermions,fields[[1]],Infinity]&&MemberQ[fermions,fields[[2]],Infinity],
sel=Select[fermions,MemberQ[#,fields[[1]],Infinity]&][[1]];
If[fields===sel,
Return[obj[[1]]->obj[[2]]],
Return[obj[[2]]->obj[[1]]];
]
];

If[MemberQ[bosons,fields[[1]],Infinity]&&MemberQ[bosons,fields[[2]],Infinity],
Return[obj[[1]]<->obj[[2]]]
];

Print["fields ",fields," not found!"];
Abort[];
];


(* ::Input::Initialization:: *)
MakeEdgeStyle[style_,setup_]:=Module[{corStyle,havePairs,allPairs,missingPairs},
corStyle=style/.{
a_[c_,d_]/;
a===Rule&&Head[c]=!=List:>
Sort[{c,GetPartnerField[c,setup]}]->d};
corStyle=corStyle/.{a_[{c__},d_]/;a===Rule:>Sort[{c}]->d};

havePairs=corStyle/.{a_[{c__},d_]/;a===Rule:>{c}};
allPairs=Join[Map[{#,#}&,GetBosons[setup]],Map[Sort,GetFermionPairs[setup]]];
missingPairs=DeleteCases[allPairs,Alternatives@@havePairs];
corStyle=Join[corStyle,Thread[missingPairs->ColorData[97,"ColorList"][[1;;Length[missingPairs]]]]];
Return[corStyle]
];


(* ::Input::Initialization:: *)
PlotOneSuperindexDiagram[diag_,setup_,OptionsPattern[]]:=Module[
{ShowEdgeLabels,EdgeStyle,
transformedDiag,vertices,edges,prefactor,
externalLegs,externalIndices,idx,field,partnerField,outerIdx,externalLeg,
regulatorVertex,curVertex,rules,corStyle,cross,
explVertices,explEdges,vertexShapes,vertexLabels,edgeLabels},

AssertIsSuperindexDiagram[diag];

EdgeStyle=OptionValue["EdgeStyle"];
ShowEdgeLabels=OptionValue["ShowEdgeLabels"];

transformedDiag=Map[
If[Head[#]===Association,
If[#["type"]=="Propagator",
<|
"Rule"->GetEdgeRule[{#["indices"][[1,2]],#["indices"][[2,2]]},{#["indices"][[1,1]],#["indices"][[2,1]]},setup],
"Style"->Sort[{#["indices"][[1,1]],#["indices"][[2,1]]}]
|>,
<|
"Vertex"->#["indices"][[All,2]],
"Style"->#["type"]
|>],
#]&
,diag];

vertices=Select[transformedDiag,MemberQ[Keys[#],"Vertex",Infinity]&];
edges=Select[transformedDiag,MemberQ[Keys[#],"Rule",Infinity]&];
prefactor=("Prefactor"/.transformedDiag[[1]])[[1]];

externalLegs=GetExternalIndices[diag];
externalIndices={};
For[idx=1,idx<=Length[externalLegs],idx++,
field=externalLegs[[idx,1]];
partnerField=GetPartnerField[field,setup];
outerIdx=Unique[externalLeg];
externalIndices=Join[externalIndices,{outerIdx}];
vertices=vertices\[Union]{<|
"Vertex"->{{outerIdx}},
"Style"->externalLeg[field]
|>};
edges=edges\[Union]{<|
"Rule"->GetEdgeRule[{externalLegs[[idx,2]],{outerIdx}},{field,partnerField},setup],"Style"->Sort[{field,partnerField}]
|>};
];

regulatorVertex=0;
For[idx=1,idx<=Length[vertices],idx++,
curVertex=vertices[[idx]]["Vertex"];
If[Length[curVertex]>1,
rules=Map[#->curVertex[[1]]&,curVertex[[2;;]]];
vertices=vertices/.rules;
edges=edges/.rules;
];
vertices[[idx]]=<|"Vertex"->vertices[[idx]]["Vertex"][[1]],"Style"->vertices[[idx]]["Style"]|>;
If[vertices[[idx]]["Style"]=="Regulatordot",regulatorVertex=vertices[[idx]]["Vertex"]];
];

corStyle=MakeEdgeStyle[EdgeStyle,setup];

 cross[r_] := Graphics[{Thick, Line[{{r / Sqrt[2], r / Sqrt[2]
            }, {-r / Sqrt[2], -r / Sqrt[2]}}], Line[{{r / Sqrt[2], -r / Sqrt[2]},
             {-r / Sqrt[2], r / Sqrt[2]}}], Circle[{0, 0}, r]}];

explVertices=Map[#["Vertex"]&,vertices];
vertexShapes=Join[
{regulatorVertex->cross[1]}
];
vertexLabels=Map[
#["Vertex"]->(#["Style"]/.a_[b_]:>b)&
,
Select[vertices,Head[#["Style"]]===externalLeg&]
];
explEdges=Map[Style[#["Rule"],#["Style"]/.corStyle]&,edges];
edgeLabels=If[ShowEdgeLabels,
Map[#["Rule"]->ToString[#["Style"][[1]]]<>ToString[#["Style"][[2]]]&,edges],
{}
];

Return[
{prefactor,
Graph[explVertices,explEdges,
VertexShape->vertexShapes,
VertexLabels->vertexLabels,
VertexSize -> {regulatorVertex -> Medium},
EdgeLabels->edgeLabels
]
}
];
];
Options[PlotOneSuperindexDiagram]={"ShowEdgeLabels"->False,"EdgeStyle"->{}};

PlotSuperindexDiagram[diags_List,setup_,a___]:=Module[{},
If[AllTrue[diags,TestIsSuperindexDiagram],
Return[Map[PlotOneSuperindexDiagram[#,setup,a]&,diags]];
];
If[TestIsSuperindexDiagram[diags],
Return[PlotOneSuperindexDiagram[diags,setup,a]];
];
Print["PlotSuperindexDiagram: diagram argument is not a superindex diagram or a list thereof!"];
Abort[];
];


(* ::Section:: *)
(*Momentum  routing*)


(* ::Input::Initialization:: *)
ReduceDiagramToMomenta[diag_,momenta_]:=diag/.Cor_[{k___}]:>Cor@@Select[{k},ContainsAny[GetAllSymbols[#],momenta]&]


(* ::Input::Initialization:: *)
RerouteFermionicMomenta[diag_/;Head[diag]=!=List,setup_,derivativeList_]:=Module[
{loopMomentum,fermions,antifermions,bosons,allMomenta,fermionMomenta,antifermionMomenta,
reducedDiagram,
fermionicPropNames,bosonicPropNames,fermionicProps,bosonicProps,
nonFermMomentum,momentumConservation,
fermionArgs,bosonArgs,idx,problems,nothing,dummyMom},

AssertIsFullDiagram[diag,"RerouteFermionicMomenta:"];

loopMomentum=Global`q;

fermions=GetFermions[setup];
antifermions=GetAntiFermions[setup];
bosons=GetBosons[setup];

allMomenta=derivativeList//.Map[#[a_,___]:>a&,fermions\[Union]antifermions\[Union]bosons];
fermionMomenta=Select[derivativeList,MemberQ[fermions,Head[#]]&]//.Map[#[a_,___]:>a&,fermions];
antifermionMomenta=Select[derivativeList,MemberQ[antifermions,Head[#]]&]//.Map[#[a_,___]:>a&,antifermions];
If[Length[fermionMomenta]!=Length[antifermionMomenta],Print["Diagram has not the same number of fermions as antifermions!"];Abort[]];
reducedDiagram=ReduceDiagramToMomenta[diag,{allMomenta}\[Union]{loopMomentum}];

fermionicPropNames=Map[Symbol["G"<>ToString[#[[2]]]<>ToString[#[[1]]]]&,GetFermionPairs[setup]];
bosonicPropNames=Map[Symbol["G"<>ToString[#]<>ToString[#]]&,GetBosons[setup]];
fermionicProps=Flatten[Union[Map[Cases[reducedDiagram,#,Infinity]&,Map[#[__]&,fermionicPropNames]]]];
bosonicProps=Flatten[Union[Map[Cases[reducedDiagram,#,Infinity]&,Map[#[__]&,bosonicPropNames]]]];

nonFermMomentum=Select[allMomenta,Not@MemberQ[fermionMomenta\[Union]antifermionMomenta,#,Infinity]&];
momentumConservation=If[Length[nonFermMomentum]==0,
allMomenta[[1]]->-Total[allMomenta]+allMomenta[[1]],
nonFermMomentum[[1]]->-Total[allMomenta]+nonFermMomentum[[1]]
];

(*If there are no boson propagators, there is nothing to do, except to mark the loop-momentum with an f (as it is a closed fermion loop)!*)
If[Length[bosonicProps]==0,
Return[diag/.loopMomentum->Symbol[ToString[loopMomentum]<>"f"]/.momentumConservation//Simplify]
];

(*Reduce further by getting rid of field information*)
fermionArgs=Union[fermionicProps/.Map[#[m___]:>{m}&,fermionicPropNames]];
bosonArgs=Union[bosonicProps/.Map[#[m___]:>{m}&,bosonicPropNames]];

(*If a femion momentum appears in a boson propagator, this needs to be fixed*)
problems={};
For[idx=1,idx<=Length[fermionMomenta],idx++,
problems=Union[problems,Select[bosonArgs/.momentumConservation,MemberQ[#,fermionMomenta[[idx]],Infinity]&]];
problems=Union[problems,Select[bosonArgs/.momentumConservation,MemberQ[#,antifermionMomenta[[idx]],Infinity]&]];
];

(*Fix the problems by doing shifts of the loop momentum*)
If[Length[problems]>0,
(*There may be no problem at all: If everywhere the same number of fermion and antifermions enter, we do not need to reroute.*)
If[
Not@MemberQ[
problems/.momentumConservation/.Map[#->dummyMom&,fermionMomenta]/.Map[#->-dummyMom&,antifermionMomenta]//Simplify,dummyMom,Infinity],
Return[diag/.momentumConservation//Simplify]
];

(*Otherwises, see if there is a shift of the loop integration, using which we can get the momenta routed correctly.*)
For[idx=1,idx<=Length[fermionMomenta],idx++,
If[
Not@MemberQ[
problems/.loopMomentum->loopMomentum-fermionMomenta[[idx]]/.momentumConservation/.Map[#->dummyMom&,fermionMomenta]/.Map[#->-dummyMom&,antifermionMomenta]//Simplify,dummyMom,Infinity],
Return[diag/.{loopMomentum->loopMomentum-fermionMomenta[[idx]]}/.momentumConservation//Simplify]
];
If[
Not@MemberQ[
problems/.loopMomentum->loopMomentum-antifermionMomenta[[idx]]/.momentumConservation/.Map[#->dummyMom&,fermionMomenta]/.Map[#->-dummyMom&,antifermionMomenta]//Simplify,dummyMom,Infinity],
Return[diag/.{loopMomentum->loopMomentum-antifermionMomenta[[idx]]}/.momentumConservation//Simplify]
];
];
,
Return[diag/.momentumConservation//Simplify];
];

Print["Routing momenta failed! Unsolved problems: ",problems];
Abort[];
];
RerouteFermionicMomenta[diags_List,setup_,derivativeList_]:=Map[RerouteFermionicMomenta[#,setup,derivativeList]&,diags];


(* ::Section:: *)
(*Testing*)


(* ::Input:: *)
(*Needs["QMeSderivation`"]*)
(*fRGEq = {"Prefactor"->{1/2},*)
(*<|"type"->"Regulatordot", "indices"->{i,j}|>,*)
(*<|"type"->"Propagator", "indices"->{i,j}|>};*)
(**)
(*fieldsNf2p1 = <|"bosonic"-> {\[Sigma][p],\[CapitalPi][p,{a}], A[p,{v, c}]},*)
(*                 "fermionic"->{{sb[p,{d,c}],s[p,{d,c}]},{qb[p,{d,c,f}],q[p,{d,c,f}]},{cb[p,{c}],c[p,{c}]}}|>;*)
(**)
(*TruncationNf2p1 = {{\[Sigma],\[Sigma]},{\[CapitalPi],\[CapitalPi]},{q,qb},{s,sb},{A,A},{c,cb},(* propagators *)*)
(*{qb,q,A},{cb,c,A},{sb,s,A},{A,A,A},{A,A,A,A},(* gluon sector scatterings *)*)
(*(*{qb,qb,q,q},*)(* four-quark scatterings *)*)
(*{qb,q,\[Sigma]},{qb,q,\[CapitalPi]},{qb,q,\[Sigma],\[Sigma]},{qb,q,\[CapitalPi],\[CapitalPi]},{qb,q,\[Sigma],\[CapitalPi]},{qb,q,\[Sigma],\[Sigma],\[Sigma]},{qb,q,\[Sigma],\[Sigma],\[CapitalPi]},{qb,q,\[Sigma],\[CapitalPi],\[CapitalPi]},{qb,q,\[CapitalPi],\[CapitalPi],\[CapitalPi]}, (* quark-meson scatterings *)*)
(* {\[Sigma],\[Sigma],\[Sigma]},{\[Sigma],\[CapitalPi],\[CapitalPi]},{\[Sigma],\[Sigma],\[Sigma],\[Sigma]},{\[Sigma],\[Sigma],\[CapitalPi],\[CapitalPi]},{\[CapitalPi],\[CapitalPi],\[CapitalPi],\[CapitalPi]}(* meson scatterings *)*)
(*};*)
(**)
(*SetupfRG= <|"MasterEquation"->fRGEq,*)
(*"FieldSpace"->fieldsNf2p1,*)
(*"Truncation"->TruncationNf2p1|>;*)


(* ::Input:: *)
(**)
(*DerivativeListqbq= {qb[p4,{d4,c4,f4}],q[p3,{d3,c3,f3}],qb[p2,{d2,c2,f2}],q[p1,{p1,c1,f1}]};*)
(*QMeSderivation`Private`$DebugLevel=0*)
(*Timing[DeriveFunctionalEquation[SetupfRG,DerivativeListqbq,"OutputLevel"->"SuperindexDiagrams"]]*)


(* ::Input:: *)
(*PlotDiagram[*)
(*Diagramsqbq[[5]],SetupfRG,*)
(*"EdgeStyle"->{q->Blue,A->Orange,\[CapitalPi]->{Dashed,Thick},\[Sigma]->{Dashed,Thick,Purple}},*)
(*"ShowEdgeLabels"->False*)
(*]*)


(* ::Input:: *)
(*symm={*)
(*{1,3,Plus}*)
(*};*)


(* ::Input:: *)
(**)
(*symm={*)
(*{1,3,Plus}*)
(*};*)
(*res=ReduceIdenticalFlowDiagrams[Diagramsqbq,DerivativeListqbq,symm];*)
(*res//Length*)


(* ::Input:: *)
(*fullres=SuperindexToFullDiagrams[res,DerivativeListqbq,SetupfRG];*)


(* ::Input:: *)
(*Select[res,Not[MemberQ[#,\[CapitalPi],Infinity]]&&Not[MemberQ[#,\[Sigma],Infinity]]&]*)


(* ::Input:: *)
(*ReduceIdenticalFlowDiagrams[Diagramsqbq];*)
(*SuperindexToFullDiagrams[%,DerivativeListqbq,SetupfRG]*)


(* ::Input:: *)
(*DerivativeListqbq= {qb[p2,{d2,c2,f2}],q[p1,{p1,c1,f1}]};*)
(*Diagramsqbq=DeriveFunctionalEquation[SetupfRG,DerivativeListqbq,"OutputLevel"->"FullDiagrams"];*)
(**)
(*RerouteFermionicMomenta[Diagramsqbq,SetupfRG,DerivativeListqbq]*)


(* ::Section:: *)
(*End  the package*)


(* ::Input::Initialization:: *)
End[]

EndPackage[]
