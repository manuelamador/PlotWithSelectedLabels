

(* Mathematica Raw Program *)

BeginPackage["PlotWithSelectedLabels`"]

listPlotWithSelectedLabels::usage=
"listPlotSelectedLabels[data_, A_, options_, labelPosition_: Automatic] makes a scatter plot \
with only a subset of labels selected. `A` determines the density of the labels, and \
labelPosition, the position of the labels. This function allows for ex-post customization \
of the plot by clicking in the points in the graph." 

Begin["`Private`"] (* Begin Private Context *) 

(* prepares a function for a sequential algorithm *)
ClearAll[addLabelOrNot];
addLabelOrNot[stack_List, point_, dist_, scaling_] := 
    If[
        Min[ChessboardDistance[
            Dot[point[[1 ;; 2]], scaling],
            Dot[#, scaling]] & /@ stack[[All, 1]]] < dist,
        Append[stack, {point[[1 ;; 2]], point[[3]],  False}],
        Append[stack, {point[[1 ;; 2]], point[[3]], True}]
    ];

(* gets the scaling right *)
ClearAll[scaling];
scaling[data_] := {
    {1/6 (Max[#] - Min[#])^(-1) &@data[[All, 1]], 0},
    {0,  (Max[#] - Min[#])^(-1) &@data[[All, 2]]}
};

(* Plots the data using a sequential algorithm to select the labels *)
(* Allows you to interactively change the graph as user clicks on country data points *)
listPlotWithSelectedLabels[data_, A_, options_, labelPosition_: Automatic]:= 
    With[{scaling = scaling[data]}, 
        DynamicModule[
            {dataToPlot = Fold[
                (addLabelOrNot[#1, #2, A, scaling] &), 
                    {{data[[1, 1 ;; 2]], data[[1, 3]], True}}, 
                    Rest@data]
            },
            Dynamic@Show@Table[
                ListPlot[x[[3]][#[[1]], #[[2]]] & /@ x[[1]], 
                    Frame -> True, 
                    x[[2]]],
                {x, {
                    {
                        dataToPlot, {
                            PlotStyle -> Gray,
                            options, 
                            PlotMarkers -> Automatic
                        },
                        #1 &
                    }, 
                    {
                        Cases[dataToPlot, {_, _, True}],{
                            PlotStyle ->  Darker[Gray], 
                            PlotMarkers -> Automatic
                        }, 
                        Labeled[#1, Style[#2, Darker[Gray]], labelPosition] &
                    },
                    {
                        dataToPlot, {
                            PlotStyle -> Opacity[0], 
                            PlotMarkers -> Automatic
                        }, 
                        EventHandler[
                            Tooltip[#1, #2],  
                            "MouseClicked" :> (dataToPlot = dataToPlot /.
                                {#1, #2, state_} :> {#1, #2, Not[state]})] &
                    }} /. {{}, _, _} :> Sequence[]
                }
            ]
            (*// (Join[#, {RegionPlot[ChessboardDistance[
                                Dot[{Log[1/16],.25},scaling], 
                                Dot[{x,y},scaling]
                            ]<A,{x, Log[1/128], Log[1]}, {y, 0.1, 0.4}]}
                    ]&)*)
        ]
    ];
    
    
End[] (* End Private Context *)

EndPackage[]
    
