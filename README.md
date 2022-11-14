# QMeS-Derivation
QMeS-derivation is a package to take functional derivatives of Functional Renormalization Group, Dyson-Schwinger, modified Slavnov-Taylor Identities and similar master equations in the context of Quantum Field Theories.

## Installation
To install the package run

	Import["https://raw.githubusercontent.com/QMeS-toolbox/QMeS-Derivation/main/QMeSInstaller.m"];

## Usage
A simple example is given by fRG flow equation of the two-point function in a $\phi^4$-theory

	<<"QMeSderivation`"

	masterEquation = {"Prefactor" -> {1/2},
	<|"type" -> "Regulatordot", "indices" -> {a, b}|>,
	<|"type" -> "Propagator", "indices" -> {a, b}|>
	};
	fields = <|"bosonic" -> {phi[p]}|>;
	truncation = {{phi, phi}, {phi, phi, phi}, {phi, phi, phi, phi}};

	setup = <|"MasterEquation" -> masterEquation, "FieldSpace" -> fields, "Truncation" -> truncation|>;

	DeriveFunctionalEquation[setup, {phi[-p], phi[p]}, "OutputLevel" -> "FullDiagrams"]

Futher details and examples can be found in the correspond publication ([arXiv:2102.01410](https://arxiv.org/pdf/2102.01410.pdf)) and the examples

## Citation

If used in scientific publications, please cite the corresponding paper,

	Jan M. Pawlowski, Coralie S. Schneider and Nicolas Wink.
	arXiv:2102.01410 [hep-ph].

or as bibtex entry

	@article{Pawlowski:2021tkk,
	author = "Pawlowski, Jan M. and Schneider, Coralie S. and Wink, Nicolas",
	title = "{QMeS-Derivation: Mathematica package for the symbolic derivation of functional equations}",
	eprint = "2102.01410",
	archivePrefix = "arXiv",
	primaryClass = "hep-ph",
	month = "2",
	year = "2021"
	}