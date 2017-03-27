(* ::Package:: *)

(* ::Section:: *)
(*Fuzzy Logic operations*)


(* :Title: Fuzzy Logic *)

(* :Context: Fuzzy` *)

(* :Author: Flip Phillips *)

(* :Summary: *)

(* :Package Version: $Revision: 1 $ *)

(* :Mathematica Version: 10.0+ *)

(* :Copyright: Copyright 2017, Flip Phillips, All Rights Reserved.  *)

(* :History: mar 17  *)

(* :Keywords: *)

(* :Limitations: *)

(* :Discussion: See https://commons.wikimedia.org/wiki/Fuzzy_operator *)


BeginPackage["Fuzzy`"];


FuzzyNop::usage = "FuzzyNop[x] is the monadic fuzzy identity operator.";
FuzzyNot::usage = "FuzzyNot[x] is the monadic fuzzy Not operator.";
FuzzyConcentrate::usage = "FuzzyConcentrate[x] is the monadic concentrate operator.";
FuzzyDilate::usage = "FuzzyDilate[x] is the monadic dilate operator.";
FuzzyAandNotA::usage = "FuzzyAandNotA[x] is the monadic A and (Not A) operator.";


FuzzyAnd::usage = "FuzzyAnd[x,y,opts] is the fuzzy And operator.";
FuzzyOr::usage = "FuzzyOr[x,y,opts] is the fuzzy Or operator.";
FuzzyXor::usage = "FuzzyXor[x,y,opts] is the fuzzy Xor operator.";
FuzzyImplies::usage = "FuzzyImplies[x,y] is the fuzzy Implies operator.";
FuzzyNand::usage = "FuzzyNand[x,y] is the fuzzy Nand operator.";
FuzzyNor::usage = "FuzzyNor[x,y] is the fuzzy Nor operator.";
FuzzyNxr::usage = "FuzzyNxr[x,y] is the fuzzy Not Xor operator.";
FuzzyNimplies::usage = "FuzzyNimplies[x,y] is the fuzzy Not Implies operator.";


FuzzyPonder::usage = "FuzzyPonder[x,y,p] is the ternary fuzzy ponder / weighting operator.";


Begin["`Private`"];


(* ::Subsection:: *)
(*monadic*)


FuzzyNop[x_]:=x


FuzzyNot[x_]:=1-x


FuzzyConcentrate[x_]:=x^2


FuzzyDilate[x_]:=Sqrt[x]


FuzzyAandNotA[x_]:=x*(1-x)


(* ::Subsection:: *)
(*dyadic*)


Fuzzy::badmethod = "Invalid method `1`.";


Options[FuzzyAnd]={Method->"Zadeh"};

FuzzyAnd[x_,y_,opts:OptionsPattern[]]:=Min[x,y] /; OptionValue[Method]=="Zadeh" 
FuzzyAnd[x_,y_,opts:OptionsPattern[]]:=x y /; OptionValue[Method]=="HyperbolicParaboloid" 
FuzzyAnd[x_,y_,opts:OptionsPattern[]]:=1-Min[1,Sqrt[(1-x)^2+(1-y)^2]] /; OptionValue[Method]=="Yager" 
FuzzyAnd[x_,y_,opts:OptionsPattern[]]:=Message[Fuzzy::badmethod,OptionValue[Method]]


Options[FuzzyOr]={Method->"Zadeh"};

FuzzyOr[x_,y_,opts:OptionsPattern[]]:=Max[x,y]  /; OptionValue[Method]=="Zadeh" 
FuzzyOr[x_,y_,opts:OptionsPattern[]]:=x+y-(x*y)  /; OptionValue[Method]=="HyperbolicParaboloid" 
FuzzyOr[x_,y_,opts:OptionsPattern[]]:=Min[1,(x^2+y^2)^2] /; OptionValue[Method]=="Yager"
FuzzyOr[x_,y_,opts:OptionsPattern[]]:=Message[Fuzzy::badmethod,OptionValue[Method]] 


Options[FuzzyXor]={Method->"Zadeh"};

FuzzyXor[x_,y_,opts:OptionsPattern[]]:=x+y-2Min[x,y]  /; OptionValue[Method]=="Zadeh" 
FuzzyXor[x_,y_,opts:OptionsPattern[]]:=x+y-(2 x y) /; OptionValue[Method]=="HyperbolicParaboloid"
FuzzyXor[x_,y_,opts:OptionsPattern[]]:=Message[Fuzzy::badmethod,OptionValue[Method]] 


Options[FuzzyImplies]={Method->"Zadeh"};

FuzzyImplies[x_,y_,opts:OptionsPattern[]]:=1-Min[x,1-y] /; OptionValue[Method]=="Zadeh" 
FuzzyImplies[x_,y_,opts:OptionsPattern[]]:=1-x+(x y) /; OptionValue[Method]=="HyperbolicParaboloid"
FuzzyImplies[x_,y_,opts:OptionsPattern[]]:=If[x<=y,0,1] /; OptionValue[Method]=="Boolean"
FuzzyImplies[x_,y_,opts:OptionsPattern[]]:=Message[Fuzzy::badmethod,OptionValue[Method]] 


Options[FuzzyNand]={Method->"Zadeh"};

FuzzyNand[x_,y_,opts:OptionsPattern[]]:=1-Min[x,y] /; OptionValue[Method]=="Zadeh" 
FuzzyNand[x_,y_,opts:OptionsPattern[]]:=1-(x y) /; OptionValue[Method]=="HyperbolicParaboloid"
FuzzyNand[x_,y_,opts:OptionsPattern[]]:=Message[Fuzzy::badmethod,OptionValue[Method]] 


Options[FuzzyNor]={Method->"Zadeh"};

FuzzyNor[x_,y_,opts:OptionsPattern[]]:=1-Max[x,y] /; OptionValue[Method]=="Zadeh" 
FuzzyNor[x_,y_,opts:OptionsPattern[]]:=1-x-y+(x y) /; OptionValue[Method]=="HyperbolicParaboloid"
FuzzyNor[x_,y_,opts:OptionsPattern[]]:=Message[Fuzzy::badmethod,OptionValue[Method]] 


Options[FuzzyNxr]={Method->"Zadeh"};

FuzzyNxr[x_,y_,opts:OptionsPattern[]]:=1-x-y+2Min[x,y] /; OptionValue[Method]=="Zadeh" 
FuzzyNxr[x_,y_,opts:OptionsPattern[]]:=1-x-y+(2 x y) /; OptionValue[Method]=="HyperbolicParaboloid"
FuzzyNxr[x_,y_,opts:OptionsPattern[]]:=Message[Fuzzy::badmethod,OptionValue[Method]] 


Options[FuzzyNimplies]={Method->"Zadeh"};

FuzzyNimplies[x_,y_,opts:OptionsPattern[]]:=Min[x,1-y] /; OptionValue[Method]=="Zadeh" 
FuzzyNimplies[x_,y_,opts:OptionsPattern[]]:=x(1-y) /; OptionValue[Method]=="HyperbolicParaboloid"
FuzzyNimplies[x_,y_,opts:OptionsPattern[]]:=If[x>y,0,1] /; OptionValue[Method]=="Boolean"
FuzzyNimplies[x_,y_,opts:OptionsPattern[]]:=Message[Fuzzy::badmethod,OptionValue[Method]] 


(* ::Subsection:: *)
(*Ternary*)


FuzzyPonder[x_,y_,p_]:=(p x)+((1-p) y)


End[];


EndPackage[];


(* ::Section:: *)
(*Tests*)


(* ::Code:: *)
(*Plot[FuzzyNop[x],{x,0,1}]*)


(* ::Code:: *)
(*Plot[FuzzyNot[x],{x,0,1}]*)


(* ::Code:: *)
(*Plot[FuzzyConcentrate[x],{x,0,1}]*)


(* ::Code:: *)
(*Plot[FuzzyDilate[x],{x,0,1}]*)


(* ::Code:: *)
(*Plot[FuzzyAandNotA[x],{x,0,1}]*)


(* ::Code:: *)
(*FuzzyAnd[x,y,Method->"Poop"]*)


(* ::Code:: *)
(*GraphicsRow[{Plot3D[FuzzyAnd[x,y],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyAnd[x,y,Method->"HyperbolicParaboloid"],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyAnd[x,y,Method->"Yager"],{x,0,1},{y,0,1}]},ImageSize->Large]*)


(* ::Code:: *)
(*GraphicsRow[{Plot3D[FuzzyOr[x,y],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyOr[x,y,Method->"HyperbolicParaboloid"],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyOr[x,y,Method->"Yager"],{x,0,1},{y,0,1}]},ImageSize->Large]*)


(* ::Code:: *)
(*GraphicsRow[{Plot3D[FuzzyXor[x,y],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyXor[x,y,Method->"HyperbolicParaboloid"],{x,0,1},{y,0,1}]},ImageSize->Large]*)


(* ::Code:: *)
(*GraphicsRow[{Plot3D[FuzzyImplies[x,y],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyImplies[x,y,Method->"HyperbolicParaboloid"],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyImplies[x,y,Method->"Boolean"],{x,0,1},{y,0,1}]},ImageSize->Large]*)


(* ::Code:: *)
(*GraphicsRow[{Plot3D[FuzzyNand[x,y],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyNand[x,y,Method->"HyperbolicParaboloid"],{x,0,1},{y,0,1}]},ImageSize->Large]*)


(* ::Code:: *)
(*GraphicsRow[{Plot3D[FuzzyNor[x,y],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyNor[x,y,Method->"HyperbolicParaboloid"],{x,0,1},{y,0,1}]},ImageSize->Large]*)


(* ::Code:: *)
(*GraphicsRow[{Plot3D[FuzzyNxr[x,y],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyNxr[x,y,Method->"HyperbolicParaboloid"],{x,0,1},{y,0,1}]},ImageSize->Large]*)


(* ::Code:: *)
(*GraphicsRow[{Plot3D[FuzzyNimplies[x,y],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyNimplies[x,y,Method->"HyperbolicParaboloid"],{x,0,1},{y,0,1}],*)
(*	Plot3D[FuzzyNimplies[x,y,Method->"Boolean"],{x,0,1},{y,0,1}]},ImageSize->Large]*)


(* ::Code:: *)
(*ContourPlot3D[FuzzyPonder[x,y,p],{x,0,1},{y,0,1},{p,0,1},PlotLegends->Automatic]*)
