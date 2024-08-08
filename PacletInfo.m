(* ::Package:: *)

PacletObject[
    <|
        "Name" -> "QMeS",
        "Version" -> "1.2",
        "WolframVersion" -> "12+",
    	"Description" -> "We present the mathematica package QMeS. It derives symbolic flow equations from a master equation (FRG, mSTI, DSE) by using a superfield formalism. Explicitly it's modules allow to derive DSEs, take functional derivatives, trace over field space and do a momentum routing for 1-loop diagrams while keeping track of prefactors and signs that arise from fermionic commutation relations.",
    	"Creator" -> "Jan M. Pawlowski, Coralie S. Schneider, Nicolas Wink",
        "Extensions" ->
            {
                {
                    "Kernel",
                    "Context" -> {
                        { "QMeSderivation`" , "QMeSderivation.m" }
                        { "QMeSderivation`Tools`" , "QMeSTools.m" }
                    }
                },
                {
                    "Documentation",
                    "Language" -> "English"
                }
            }
    |>
]
