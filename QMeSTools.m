(* ::Package:: *)

(* ::Chapter:: *)
(*Derive Functional Equation Tools*)


(* ::Input::Initialization:: *)
BeginPackage["QMeSderivation`Tools`"]


(* ::Section:: *)
(*Exports*)


(* ::Input::Initialization:: *)
ReduceIdenticalFlowDiagrams::usage = "ReduceIdenticalFlowDiagrams[diagrams,symmetries]

Reduce a set of flow equations by matching identical topologies and/or by utilizing a given set of symmetries.";


(* ::Input::Initialization:: *)
PlotSuperindexDiagram::usage = "PlotSuperindexDiagram[diagram, setup, options]

Plot a single superindex diagram. The first argument to this function is the diagram itself, the second one is a valid QMeS setup.
The diagram has to be a superindex diagram.

The options for the function PlotSuperindexDiagram are:
\"ShowEdgeLabels\" ->  False / True
	This option will toggle whether edges are plotted together with labels to identify them.
\"EdgeStyle\" ->  List
	Using this option, one can specify the edge styles of different propagators. e.g.,
		\"EdgeStyle\"->{q->Blue,A->Orange,\[CapitalPi]->{Dashed,Thick},\[Sigma]->{Dashed,Thick,Purple}}
	may produce a diagram like
	{-\!\(\*FractionBox[\(1\), \(2\)]\),Graph[{{$CellContext`externalLeg$155993$156004}, {$CellContext`externalLeg$155993$156007}, {$CellContext`externalLeg$155993$156010}, {$CellContext`externalLeg$155993$156013}, {$CellContext`i}, {$CellContext`a83$471882}, {$CellContext`a83$471893}, {$CellContext`a83$471910}, {$CellContext`a83$471979}}, {{{6, 5}, {8, 7}, {2, 8}, {4, 6}, {5, 9}, {7, 3}, {9, 1}}, {{6, 7}, {8, 9}}}, {FormatType -> TraditionalForm, EdgeStyle -> {UndirectedEdge[{$CellContext`a83$471910}, {$CellContext`a83$471979}] -> {RGBColor[1, 0.5, 0]}, DirectedEdge[{$CellContext`a83$471882}, {$CellContext`i}] -> {RGBColor[0, 0, 1]}, UndirectedEdge[{$CellContext`a83$471882}, {$CellContext`a83$471893}] -> {{Dashing[{Small, Small}], Thickness[Large]}}, DirectedEdge[{$CellContext`i}, {$CellContext`a83$471979}] -> {RGBColor[0, 0, 1]}, DirectedEdge[{$CellContext`externalLeg$155993$156013}, {$CellContext`a83$471882}] -> {RGBColor[0, 0, 1]}, DirectedEdge[{$CellContext`a83$471979}, {$CellContext`externalLeg$155993$156004}] -> {RGBColor[0, 0, 1]}, DirectedEdge[{$CellContext`a83$471910}, {$CellContext`a83$471893}] -> {RGBColor[0, 0, 1]}, DirectedEdge[{$CellContext`a83$471893}, {$CellContext`externalLeg$155993$156010}] -> {RGBColor[0, 0, 1]}, DirectedEdge[{$CellContext`externalLeg$155993$156007}, {$CellContext`a83$471910}] -> {RGBColor[0, 0, 1]}}, VertexLabels -> {{$CellContext`externalLeg$155993$156010} -> $CellContext`qb, {$CellContext`externalLeg$155993$156004} -> $CellContext`qb, {$CellContext`externalLeg$155993$156007} -> $CellContext`q, {$CellContext`externalLeg$155993$156013} -> $CellContext`q}, VertexShape -> {{$CellContext`i} -> Graphics[{Thickness[Large], Line[{{2^Rational[-1, 2], 2^Rational[-1, 2]}, {-2^Rational[-1, 2], -2^Rational[-1, 2]}}], Line[{{2^Rational[-1, 2], -2^Rational[-1, 2]}, {-2^Rational[-1, 2], 2^Rational[-1, 2]}}], Circle[{0, 0}, 1]}]}, VertexSize -> {{$CellContext`i} -> Medium}}]}
";


(* ::Input::Initialization:: *)
PlotSuperindexDiagrams::usage=""


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Section:: *)
(*Debug*)


(* ::Input::Initialization:: *)
$DebugLevel =0;


(* ::Input::Initialization:: *)
myEcho[msg_,lvl_] := If[$DebugLevel >=lvl, Echo[msg];, Nothing;]


(* ::Section::Closed:: *)
(*Utilities*)


(* ::Subsection:: *)
(*Tests and assertions*)


(* ::Input::Initialization:: *)
TestIsSuperindexDiagram[diag_]:=Module[{hasPrefactor,hasAssociations},
If[Not[ListQ[diag]&&Length[diag]>1],Return[False]];
hasAssociations=AllTrue[Map[AssociationQ,Flatten@diag[[2;;]]],Identity]&&Length[Flatten@diag[[2;;]]]>0;
hasPrefactor=Extract[diag[[1]],1]=="Prefactor";
Return[hasAssociations&&hasPrefactor]
]
AssertIsSuperIndexDiagram[diag_,context_String:""]:=If[Not@TestIsSuperindexDiagram[diag],Print[context," Argument is not a SuperIndexDiagram."];Abort[]];
AssertAllSuperIndexDiagrams[diags_List,context_String:""]:=Map[AssertIsSuperIndexDiagram[#,context]&,diags];


(* ::Input::Initialization:: *)
TestIsOneLoop[diag_]:=Module[{internalIndices,objectIndices,assignNumber,mapAssignNumber,testNumbers},
AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];

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


(* ::Subsection:: *)
(*Getting objects*)


(* ::Input::Initialization:: *)
GetIndices[diag_List]:=Module[{},
AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];
Flatten[Map[#["indices"]&,diag[[2;;]]],1]
];
GetExternalIndices[diag_List]:=Module[{allIndices},
AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];
allIndices=GetIndices[diag];
Keys@Select[Counts[allIndices],#==1&]
];
GetInternalIndices[diag_List]:=Module[{allIndices},
AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];
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
AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];
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


(* ::Section::Closed:: *)
(*Diagram reduction*)


(* ::Subsection:: *)
(*Diagram grouping*)


(* ::Input::Initialization:: *)
SuperIndexDiagramContent[diag_]:=Module[{},
AssertIsSuperIndexDiagram[diag,"SuperIndexDiagramContent:"];
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


(* ::Subsection:: *)
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

CheckOneLoopDiagramIdentity[indiag1_,indiag2_,symmetries_:{}]:=Module[{startPos,CheckIteration,entryIdx,diag1,diag2,symmidx},
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

For[symmidx=0,symmidx<=Length[symmetries],symmidx++,
diag1=indiag1;
If[symmidx>0,diag2=indiag2/.symmetries[[symmidx]],diag2=indiag2];

startPos=Map[GetRegulatordot,{diag1,diag2}];

entryIdx=Map[#["indices"][[1]]&,startPos];
If[entryIdx[[1,1]]==entryIdx[[2,1]],
If[CheckIteration[entryIdx],Return[True]];
];

entryIdx[[2]]=startPos[[2]]["indices"][[2]];
If[entryIdx[[1,1]]==entryIdx[[2,1]],
If[CheckIteration[entryIdx],Return[True]];
];
];

Return[False]
];


(* ::Subsection:: *)
(*Diagram set reduction*)


(* ::Input::Initialization:: *)
ReduceIdenticalFlowDiagrams[diags_,symmetries_List]:=Module[{separatedGroups,reduceGroup,reducedGroups},
AssertAllSuperIndexDiagrams[diags,"ReduceIdenticalFlowDiagrams"];

separatedGroups=SeparateSuperIndexDiagramGroups[diags];

reduceGroup[groupDiags_]:=Module[{i,j,multiplicities,identities,prefactors,newDiags},
multiplicities=Table[1,{i,Length[groupDiags]}];
For[i=1,i<=Length[groupDiags],i++,
If[multiplicities[[i]]>0,
identities=Map[CheckOneLoopDiagramIdentity[groupDiags[[i]],#,symmetries]&,groupDiags[[i+1;;]]];
identities=Map[If[#,1,0]&,identities];
identities=Join[
Table[0,{j,i-1}],
{Total[identities]},
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


(* ::Section:: *)
(*Diagram drawing*)


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
PlotSuperindexDiagram[diag_,setup_,OptionsPattern[]]:=Module[
{ShowEdgeLabels,EdgeStyle,
transformedDiag,vertices,edges,prefactor,
externalLegs,externalIndices,idx,field,partnerField,outerIdx,externalLeg,
regulatorVertex,curVertex,rules,corStyle,cross,
explVertices,explEdges,vertexShapes,vertexLabels,edgeLabels},

AssertIsSuperIndexDiagram[diag];

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
Options[PlotSuperindexDiagram]={"ShowEdgeLabels"->False,"EdgeStyle"->{}};
PlotSuperindexDiagrams[diags_List,setup_,a___]:=Module[{},
Map[PlotSuperindexDiagram[#,setup,a]&,diags]
];


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
(*"Truncation"->TruncationNf2p1|>;DerivativeListqbq= {qb[p4,{d4,c4,f4}],q[p3,{d3,c3,f3}],qb[p2,{d2,c2,f2}],q[p1,{p1,c1,f1}]};*)
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
(*{*)
(*{qb,{p4,{d4,c4,f4}}}->{qb,{p2,{d2,c2,f2}}},*)
(*{qb,{p2,{d2,c2,f2}}}->{qb,{p4,{d4,c4,f4}}},*)
(*{q,{p3,{d3,c3,f3}}}->{q,{p1,{p1,c1,f1}}},*)
(*{q,{p1,{p1,c1,f1}}}->{q,{p3,{d3,c3,f3}}}*)
(*},*)
(*{*)
(*{qb,{p4,{d4,c4,f4}}}->{qb,{p2,{d2,c2,f2}}},*)
(*{qb,{p2,{d2,c2,f2}}}->{qb,{p4,{d4,c4,f4}}}*)
(*},*)
(*{*)
(*{q,{p3,{d3,c3,f3}}}->{q,{p1,{p1,c1,f1}}},*)
(*{q,{p1,{p1,c1,f1}}}->{q,{p3,{d3,c3,f3}}}*)
(*}*)
(*};*)


(* ::Input:: *)
(*res=ReduceIdenticalFlowDiagrams[Diagramsqbq,symm];*)
(*res//Length*)


(* ::Input:: *)
(*fullres=SuperindexToFullDiagrams[res,DerivativeListqbq,SetupfRG];*)


(* ::Input:: *)
(*Select[res,Not[MemberQ[#,\[CapitalPi],Infinity]]&&Not[MemberQ[#,\[Sigma],Infinity]]&]*)


(* ::Input:: *)
(*ReduceIdenticalFlowDiagrams[Diagramsqbq];*)
(*SuperindexToFullDiagrams[%,DerivativeListqbq,SetupfRG]*)


(* ::Section:: *)
(*End  the package*)


(* ::Input::Initialization:: *)
End[]

EndPackage[]
