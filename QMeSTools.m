(* ::Package:: *)

(* ::Chapter:: *)
(*Derive Functional Equation Tools*)


(* ::Input::Initialization:: *)
BeginPackage["QMeSderivation`Tools`"]


(* ::Section:: *)
(*Exports*)


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Section:: *)
(*Debug*)


(* ::Input::Initialization:: *)
qmesDerivationDirectory=SelectFirst[
Join[
{
FileNameJoin[{$UserBaseDirectory,"Applications","QMeSderivation"}],
FileNameJoin[{$BaseDirectory,"Applications","QMeSderivation"}],
FileNameJoin[{$InstallationDirectory,"AddOns","Applications","QMeSderivation"}],
FileNameJoin[{$InstallationDirectory,"AddOns","Packages","QMeSderivation"}],
FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","QMeSderivation"}]
},
Select[$Path,StringContainsQ[#,"QMeS-Derivation"]&]
],
DirectoryQ[#]&
];


(* ::Input::Initialization:: *)
$DebugLevel =0;


(* ::Input::Initialization:: *)
myEcho[msg_,lvl_] := If[$DebugLevel >=lvl, Echo[msg];, Nothing;]


(* ::Section:: *)
(*Testing*)


(* ::Input:: *)
(*Get["QMeSderivation`"]*)
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
(*DerivativeListqbq= {qb[-p,{d2,c2,f2}],q[p,{d1,c1,f1}]};*)
(*Diagramsqbq = DeriveFunctionalEquation[SetupfRG,DerivativeListqbq,"OutputLevel"->"FullDiagrams","ReturnAll"->True];*)


(* ::Input:: *)
(*TestIsSuperindexDiagram[diag_]:=Module[{hasPrefactor,hasAssociations},*)
(*If[Not[ListQ[diag]&&Length[diag]>1],Return[False]];*)
(*hasAssociations=AllTrue[Map[AssociationQ,Flatten@diag[[2;;]]],Identity]&&Length[Flatten@diag[[2;;]]]>0;*)
(*hasPrefactor=Extract[diag[[1]],1]=="Prefactor";*)
(*Return[hasAssociations&&hasPrefactor]*)
(*]*)
(*AssertIsSuperIndexDiagram[diag_,context_String:""]:=If[Not@TestIsSuperindexDiagram[diag],Print[context," Argument is not a SuperIndexDiagram."];Abort[]];*)
(*AssertAllSuperIndexDiagrams[diags_List,context_String:""]:=Map[AssertIsSuperIndexDiagram[#,context]&,diags];*)


(* ::Input:: *)
(*GetIndices[diag_List]:=Module[{},*)
(*AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];*)
(*Flatten[Map[#["indices"]&,diag[[2;;]]],1]*)
(*];*)
(*GetExternalIndices[diag_List]:=Module[{allIndices},*)
(*AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];*)
(*allIndices=GetIndices[diag];*)
(*Keys@Select[Counts[allIndices],#==1&]*)
(*];*)
(*GetInternalIndices[diag_List]:=Module[{allIndices},*)
(*AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];*)
(*allIndices=GetIndices[diag];*)
(*Keys@Select[Counts[allIndices],#>1&]*)
(*];*)
(**)
(*GetIndices[obj_Association]:=Module[{},*)
(*obj["indices"]*)
(*];*)
(*GetExternalIndices[diag_List,obj_Association]:=Module[{allIndices,externalIndices},*)
(*allIndices=GetIndices[obj];*)
(*externalIndices=GetExternalIndices[diag];*)
(*Select[allIndices,MemberQ[externalIndices,#]&]*)
(*];*)
(*GetInternalIndices[diag_List,obj_Association]:=Module[{allIndices,internalIndices},*)
(*AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];*)
(*allIndices=GetIndices[obj];*)
(*internalIndices=GetInternalIndices[diag];*)
(*Select[allIndices,MemberQ[internalIndices,#]&]*)
(*];*)


(* ::Input:: *)
(*TestIsOneLoop[diag_]:=Module[{internalIndices,objectIndices,assignNumber,mapAssignNumber,testNumbers},*)
(*AssertIsSuperIndexDiagram[diag,"TestIsOneLoop:"];*)
(**)
(*(*An expression is one-loop if every object (vertex or propagator) has only one incoming and one outgoing index.*)
(*Therefore, what we do here is to check the number of internal indices of any object in the diagram and testing whether this is exactly 2 for all objects.*)*)
(**)
(*internalIndices=GetInternalIndices[diag];*)
(*objectIndices=Map[#["indices"]&,diag[[2;;]]];*)
(**)
(*assignNumber=If[MemberQ[internalIndices,#],1,0]&;*)
(*mapAssignNumber=Map[assignNumber,#]&;*)
(*testNumbers=Map[Total[mapAssignNumber[#]]&,objectIndices];*)
(*Return[AllTrue[testNumbers,#==2&]]*)
(*];*)
(*AssertIsOneLoop[diag_List,context_String:""]:=If[Not@TestIsOneLoop[diag],Print[context," Diagram is not one-loop."];Abort[]];*)
(**)
(*TestIsFlowDiagram[diag_]:=Module[{content,regulatordotMatch},*)
(*content=SuperIndexDiagramContent[diag];*)
(*regulatordotMatch=Select[content,StringMatchQ[#,"Regulatordot"~~___]&];*)
(*If[Length[regulatordotMatch]==1,True,False]*)
(*];*)
(*AssertIsFlowDiagram[diag_List,context_String:""]:=If[Not@TestIsFlowDiagram[diag],Print[context," Diagram is not a flow diagram."];Abort[]];*)


(* ::Input:: *)
(*GetObjectIdentifier[ob_Association]:=Module[{},*)
(*ob["type"]<>StringJoin[Map[ToString[#[[1]]]&,ob["indices"]]]*)
(*];*)
(*SuperIndexDiagramContent[diag_]:=Module[{},*)
(*AssertIsSuperIndexDiagram[diag,"SuperIndexDiagramContent:"];*)
(*Sort[Map[GetObjectIdentifier,diag[[2;;]]]]*)
(*];*)
(*SeparateSuperIndexDiagramGroups[diags_List]:=Module[{identifierRep,removeFirsts,groupedDiagrams},*)
(*AssertAllSuperIndexDiagrams[diags,"SeparateSubdiagramGroups:"];*)
(**)
(*(*We group all diagrams into groups that could be potentially identical. *)
(*We simply make sure that in each group all diagrams have the same objects, ignoring topology.*)*)
(*identifierRep=Map[SuperIndexDiagramContent,diags];*)
(*identifierRep=Thread[{identifierRep,diags}];*)
(*removeFirsts[ex_]:=Map[#[[2]]&,ex];*)
(*groupedDiagrams=Map[removeFirsts,GatherBy[identifierRep,#[[1]]&]];*)
(**)
(*Return[groupedDiagrams]*)
(*];*)
(**)
(*GetBosons[setup_Association]:=Map[Head,setup["FieldSpace"]["bosonic"]];*)
(*GetFermions[setup_Association]:=Map[Head[#[[2]]]&,setup["FieldSpace"]["fermionic"]];*)
(*GetAntiFermions[setup_Association]:=Map[Head[#[[1]]]&,setup["FieldSpace"]["fermionic"]];*)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*GetRegulatordot[diag_]:=Module[{},*)
(*AssertIsFlowDiagram[diag,"GetRegulatordot:"];*)
(*Select[diag,MemberQ[#,"Regulatordot",Infinity]&][[1]]*)
(*]*)


(* ::Input:: *)
(*IterateDiagram[diag_,curPos_,entryIdx_]:=Module[{curIdxs,exitIdx,nextPos,externalIndices,i,candidates},*)
(*curIdxs=curPos["indices"];*)
(*If[Not@MemberQ[curIdxs,entryIdx],Print["IterateDiagram: Invalid entryIdx"];Abort[]];*)
(*externalIndices=GetExternalIndices[diag];*)
(*For[i=1,i<=Length[externalIndices],i++,curIdxs=DeleteCases[curIdxs,externalIndices[[i]]]];*)
(*exitIdx=DeleteCases[curIdxs,entryIdx][[1]];*)
(**)
(*candidates=Select[diag[[2;;]],MemberQ[#,exitIdx,Infinity]&];*)
(*If[Length[candidates]!=2,Print["IterateDiagram: Logic Error"];Abort[]];*)
(*nextPos=DeleteCases[candidates,curPos][[1]];*)
(**)
(*Return[{nextPos,exitIdx}];*)
(*];*)
(**)
(*CheckOneLoopDiagramIdentity[diag1_,diag2_]:=Module[{startPos,entryIdx},*)
(*AssertIsOneLoop[diag1,"CheckDiagramIdentity:"];*)
(*AssertIsOneLoop[diag2,"CheckDiagramIdentity:"];*)
(**)
(*startPos=Map[GetRegulatordot,{diag1,diag2}];*)
(**)
(*CheckIteration[entry_]:=Module[{curPos,curIdx,iter,externalIndices={0,0},objectIdentifiers={"",""}},*)
(*curPos=startPos;*)
(*curIdx=entry;*)
(*iter=0;*)
(**)
(*While[(iter==0||curPos!=startPos)&&iter<50,*)
(**)
(*externalIndices[[1]]=GetExternalIndices[diag1,curPos[[1]]];*)
(*externalIndices[[2]]=GetExternalIndices[diag2,curPos[[2]]];*)
(**)
(*objectIdentifiers[[1]]=GetObjectIdentifier[curPos[[1]]];*)
(*objectIdentifiers[[2]]=GetObjectIdentifier[curPos[[2]]];*)
(**)
(*If[externalIndices[[1]]!=externalIndices[[2]],Print["Diff: ",externalIndices[[1]]," vs ",externalIndices[[2]]]];*)
(*If[objectIdentifiers[[1]]!=objectIdentifiers[[2]],Print["Diff: ",objectIdentifiers[[1]]," vs ",objectIdentifiers[[2]]]];*)
(**)
(*{curPos[[1]],curIdx[[1]]}=IterateDiagram[diag1,curPos[[1]],curIdx[[1]]];*)
(*{curPos[[2]],curIdx[[2]]}=IterateDiagram[diag2,curPos[[2]],curIdx[[2]]];*)
(*iter++;*)
(*];*)
(*If[iter>=50,Return[False]];*)
(**)
(*Return[True]*)
(*];*)
(**)
(*entryIdx=Map[#["indices"][[1]]&,startPos];*)
(*If[entryIdx[[1,1]]==entryIdx[[2,1]],*)
(*If[CheckIteration[entryIdx],Return[True]];*)
(*];*)
(**)
(*entryIdx[[2]]=startPos[[2]]["indices"][[2]];*)
(*If[entryIdx[[1,1]]==entryIdx[[2,1]],*)
(*If[CheckIteration[entryIdx],Return[True]];*)
(*];*)
(**)
(*Return[False]*)
(*];*)
(**)
(*seps=Diagramsqbq[[1]]//SeparateSuperIndexDiagramGroups;*)
(*CheckOneLoopDiagramIdentity[seps[[2,1]],seps[[2,2]]]*)


(* ::Input:: *)
(*seps[[1,1,2]]*)


(* ::Input:: *)
(**)


(* ::Section:: *)
(*End  the package*)


(* ::Input::Initialization:: *)
End[]

EndPackage[]
