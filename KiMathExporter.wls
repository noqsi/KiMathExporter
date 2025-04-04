#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsection:: *)
(*Extraction of the network structure from the XML netlist*)


(* ::Text:: *)
(*Extract the "export" structure.*)


(* ::Input::Initialization:: *)
expart[nl_]:=nl/.XMLObject[_][___,e:XMLElement["export",___],___]->e


(* ::Text:: *)
(*Extract list of substructures by name.*)


(* ::Input::Initialization:: *)
exsub[XMLElement[_,_,sublist_],name_]:=Cases[sublist,XMLElement[name,___]]


(* ::Text:: *)
(*Extract list of nets.*)


(* ::Input::Initialization:: *)
nets[nl_]:=exsub[exsub[expart[nl],"nets"][[1]],"net"]


(* ::Text:: *)
(*Extract attributes from an XMLElement.*)


(* ::Input::Initialization:: *)
elemAttr[XMLElement[_,attr_,_]]:=attr


(* ::Text:: *)
(*Extract net voltage variable from a net element.*)


(* ::Input::Initialization:: *)
netVolts[el_]:=v["name"/.elemAttr[el]]


(* ::Text:: *)
(*Extract device pin voltage variable (node in KiCAD terminology).*)


(* ::Input::Initialization:: *)
nodeVolts[el_]:=v["ref","pin"]/.elemAttr[el]


(* ::Text:: *)
(*Equate the voltage on a pin to the voltage on its connected net.*)


(* ::Input::Initialization:: *)
netVeq[nv_][node_]:=nv==nodeVolts[node]


(* ::Text:: *)
(*Construct list of voltage equations for a net (all pins have same voltage).*)


(* ::Input::Initialization:: *)
netVeqs[net_]:=Map[netVeq[netVolts[net]],exsub[net,"node"]]


(* ::Text:: *)
(*Extract device pin current variable (node in KiCAD terminology).*)


(* ::Input::Initialization:: *)
nodeAmps[el_]:=i["ref","pin"]/.elemAttr[el]


(* ::Text:: *)
(*Total current into a net is zero, unless it's "GND".*)


(* ::Input::Initialization:: *)
netIeq[net_]:=Plus@@Map[nodeAmps,exsub[net,"node"]]==0
netIeq[XMLElement["net",{___,Rule["name","GND"],___},___]]:=Nothing


(* ::Text:: *)
(*Lists of voltage and current variables in the topology. Ground is excluded, as it is not a variable.*)


(* ::Input::Initialization:: *)
allNetVolts[nl_]:=Map[netVolts,nets[nl]]/.v["GND"]->Nothing


(* ::Input::Initialization:: *)
pinVolts[net_]:=Map[nodeVolts,exsub[net,"node"]]
allPinVolts[nl_]:=Join@@Map[pinVolts,nets[nl]]


(* ::Input::Initialization:: *)
pinAmps[net_]:=Map[nodeAmps,exsub[net,"node"]]
allPinAmps[nl_]:=Join@@Map[pinAmps,nets[nl]]


(* ::Text:: *)
(*Construct list of topology equations, voltage and current.*)


(* ::Input::Initialization:: *)
topoEqs[netlist_]:=
With[{n=nets[netlist]},
Flatten[{Map[netIeq,n],Map[netVeqs,n]}]]/.v["GND"]->0


(* ::Text:: *)
(*Extract list of components*)


(* ::Input::Initialization:: *)
comps[nl_]:=exsub[exsub[expart[nl],"components"][[1]],"comp"]


(* ::Text:: *)
(*Get component reference designator*)


(* ::Input::Initialization:: *)
refdes[XMLElement["comp",r_,__]]:="ref"/.r


(* ::Text:: *)
(*Get component value. If there's no value property, use the refdes as a lower case symbol.*)


(* ::Input::Initialization:: *)
extractValue[XMLElement["comp",_,{XMLElement["value",_,{v_}],___}]]:=v
extractValue[el_]:="~"      (* default *)

valueProperty[el_]:=
With[
{v=extractValue[el]},
If[
StringMatchQ[v,"~"],
Symbol[ToLowerCase[refdes[el]]],
ToExpression[v]]]


(* ::Text:: *)
(*Get the MathModel property*)


(* ::Input::Initialization:: *)
mathModel[XMLElement["comp",_,{___,XMLElement["property",{"name"->"MathModel","value"->m_},_],___}]]:=m


(* ::Text:: *)
(*Get model, refdes, and value for a component.*)


(* ::Input::Initialization:: *)
compVitals[comp_]:={mathModel[comp],refdes[comp],valueProperty[comp]}


(* ::Text:: *)
(*Turn imported XML expressions into a circuit design suitable for building a model. The result is an Association. *)
(**)
(*The "topology" element captures the circuit connections as a list of equations. Pin voltages, of the form v["refdes","pin"] are equated to  connected net voltages, of the form v["netname"].*)
(**)
(*The "components" element is a list of components, with each component represented by a list of the form {"model","refdes",value}. If the value property is missing from the schematic, the "refdes" string, converted to a lower case Symbol, is used. Otherwise, the value property from the schematic is interpreted as a Mathematica expression.*)
(**)
(*"netVolts", "pinVolts", and "netamps" are the names of the variables representing the various voltages and currents.*)


(* ::Input::Initialization:: *)
design[netlist_]:=<|
"topology"->topoEqs[netlist],
"components"->compVitals/@comps[netlist],
"netVolts"->allNetVolts[netlist],
"pinVolts"->allPinVolts[netlist],
"pinAmps"->allPinAmps[netlist]
|>


(* ::Input::Initialization:: *)
importCircuit[xmlfile_String]:=design[Import[xmlfile,"XML"]]


(* ::Input::Initialization:: *)
Put[importCircuit[$ScriptCommandLine[[2]]],$ScriptCommandLine[[3]]]
