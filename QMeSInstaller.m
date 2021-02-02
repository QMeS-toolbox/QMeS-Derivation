(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Input::Initialization:: *)
(*"
Copyright (C) 2020, Jan M. Pawlowski, Coralie S. Schneider and Nicolas Wink.
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"*)


(* ::Input::Initialization:: *)
QMeSInstaller::allowinternetuse="You have forbidden Mathematica to access the internet. Either allow Mathematica to access the internet or download the QMeS from https://github.com/QMeS-toolbox/QMeS-Derivation manually.";
If[Not["AllowInternetUse" /. SystemInformation["Network"]],
Message[QMeSInstaller::allowinternetuse];
Abort[];
];


(* ::Input::Initialization:: *)
(* just for backwards compatibility *)
If[ToString[Context[URLDownload]]=!="System`",URLDownload=URLSave];


(* ::Input::Initialization:: *)
QMeSRepositoryAddress=(*"https://raw.githubusercontent.com/CoralieSchneider/QMeS---Derivation/main/"*)"https://raw.githubusercontent.com/QMeS-toolbox/QMeS-Derivation/main/";


(* ::Input::Initialization:: *)
(*If[Head[QMeSZipLocation]=!=String,*)QMeSZipLocation=QMeSRepositoryAddress<>"QMeS-Derivation.zip"(*]*);

QMeSInstallDir=FileNameJoin[{$UserBaseDirectory,"Applications"}];


(* ::Input::Initialization:: *)
QMeSInstaller::zipdownloadfailed="Download from "<>QMeSZipLocation<>" failed.";
QMeSInstaller::installationfailed="\nInstallation failed. Please read the error messages for more information!";

Print["Downloading QMeS ..."];
QMeSArchive=FileNameJoin[{$TemporaryDirectory,"QMeS-Derivation.zip"}];
URLDownload[QMeSZipLocation,QMeSArchive]

tmpQMeSImport=Import[QMeSArchive];
If[tmpQMeSImport==="{\"error\":\"Not Found\"}"||tmpQMeSImport==="404: Not Found",Message[QMeSInstaller::zipdownloadfailed];Abort[];];

newVersionString=Version/.List@@Import[QMeSArchive,FileNameJoin[{"QMeS-Derivation","QMeS_PacletInfo.m"}]];
QMeSFiles=FileNameJoin[{QMeSInstallDir,#}]&/@Import[QMeSArchive];
QMeSFilesExist=FileExistsQ/@QMeSFiles;
QMeSExistingInstallation=Or@@QMeSFilesExist;
QMeSExistingPacletInfo=FileNameJoin[{QMeSInstallDir,"QMeS-Derivation","QMeS_PacletInfo.m"}];
QMeSExistingVersionString=If[FileExistsQ[QMeSExistingPacletInfo],Version/.List@@Import[QMeSExistingPacletInfo],"unknown"];


(* ::Input::Initialization:: *)
deleteExisting=False;
deleteExisting=If[QMeSExistingInstallation,
ChoiceDialog["The installer has found an existing QMeS installation.
Do you want to overwrite the existing installation version "<>QMeSExistingVersionString<>" with version "<>newVersionString<>"?
Otherwise the installation will be aborted.",
WindowTitle->"QMeS Installation",WindowSize->{Medium,All}],False];

If[deleteExisting,DeleteFile[Pick[QMeSFiles,QMeSFilesExist]]];

If[QMeSExistingInstallation&&deleteExisting===False,
(*abort installation*)
Print["QMeS installation aborted."];,
(*install QMeS*)
installationSuccess=Check[
ExtractArchive[QMeSArchive,QMeSInstallDir];
getQMeS = FileNameJoin[{"QMeS-Derivation","DeriveFunctionalEquation.m"}];
Get[getQMeS]
,$Failed];
If[installationSuccess===$Failed,
(*installation failed*)
Message[QMeSInstaller::installationfailed];,
(*installation successful*)
RebuildPacletData[];
Print["
Installation was successful. Have fun deriving diagrams!
"];
];
];

Quiet[DeleteFile[QMeSArchive]];
