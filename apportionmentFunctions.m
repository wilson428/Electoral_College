(* ::Package:: *)

Clear["Global`*"];
detectEnvironment[] := StringContainsQ[NotebookDirectory[], "wolframcloud"];
SetDirectory@NotebookDirectory[];


(* ::Title:: *)
(*What's the Best Way to Allocate Members of Congress?*)


(* ::Subsection:: *)
(*Every ten years, after the decennial Census, the U.S. reallocates the 435 members of Congress based on updated state populations. This has major implications for both legislation and presidential elections, since a state's electoral votes are determined the size of its delegation. Is there a better way?*)


(* ::Section:: *)
(*Loading the Data*)


(* ::Text:: *)
(*While Mathematica has population conveniently built in to state entities through the Wolfram Data Repository, let's be extra careful and use the same decennial Census figures the government has used for apportionment, as well as the official number of representatives per state allotted every tens years to compare to alternate methods. This data is conveniently organized on Census.gov and gathered in `getPopulationApportionment.nb`*)


statesByAbbreviation = If[detectEnvironment[],
	CloudGet["https://www.wolframcloud.com/objects/ad3e2c70-ebd6-42c0-b038-60f937486a84"],
	Import["./data/state_population_reps_evs.wl"]
];


(* ::Text:: *)
(*Let's check that this loaded correctly with all the data we need*)


statesByAbbreviation["CA"]


stateAbbrs = Keys@statesByAbbreviation;


(* ::Section:: *)
(*How it works now: The Huntington-Hill method*)


(* ::Text:: *)
(*Since the 1940 Census, the United States has used the Huntington-Hill algorithm, known as the "method of equal proportions," to divvy up the 435 House seats. This begins by allotting one electoral vote to each state, leaving 385 left. The 50 states are then allotted these remaining seats one-by-one according to which one has the highest priority as determined by a simple equation: its population divided by the square root of its current seats times one extra seat (the Geometric Mean): *)


(* ::Text:: *)
(*P/Sqrt[n (1+n)]*)


(* ::Text:: *)
(*Hyperlink["https://en.wikipedia.org/wiki/United_States_congressional_apportionment#The_method_of_equal_proportions","https://en.wikipedia.org/wiki/United_States_congressional_apportionment#The_method_of_equal_proportions"]*)


(* ::Text:: *)
(*Let's make this equation a function for a given state, getting the 2010 decennial Census data from the data we loaded and feeding it the current number of allocated seats*)


priorityHuntingtonHill[stateAbbr_, stateEVs_] := 1.0 * statesByAbbreviation[stateAbbr]["population_decennial"]["2010"] / Sqrt[stateEVs * (stateEVs + 1)]


(* ::Text:: *)
(*The allocation function will assign electoral votes according to the above formula one at a time. We're also going to explicitly pass the priority function in case we want to try a different one later, which we definitely will, and parameterize all aspects of the allocation algorithm:*)


(* ::Text:: *)
(*To save time, we' re also going to store the priorities until we update the number of seats in a state rather than run 50 square roots 385 times.*)


(* ::Item:: *)
(*total_ is the total number of seats to allocate, defaulting to 435*)


(* ::Item:: *)
(*min_ is the default number of seats that every state begins with, defaulting to 1*)


(* ::Item:: *)
(*priorityFunc_ is the means of determining a state's priority, defaulting to the Huntington-Hill function defined above*)


(* We need a list of state abbreviations without DC since it is not factored into allocation -- "Taxation Without Representation!" *)
stateAbbrs50 = Select[stateAbbrs, # != "DC"&];

AllocateOne[evs_, priorities_, priorityFunc_:priorityHuntingtonHill] := (
	topPriority = First[Keys[Reverse[Sort[priorities]]]];
	updatedPriorities = priorities;
	updatedEVs = evs;	
	updatedEVs[topPriority] += 1;
	updatedPriorities[topPriority] = priorityFunc[topPriority, updatedEVs[topPriority]];
	{ updatedEVs, updatedPriorities }
)

calculateAllocations[total_:435, min_:1, priorityFunc_:priorityHuntingtonHill]:= (
	stateEVs = AssociationThread[stateAbbrs50, ConstantArray[1, 50]];
	statePriorities = AssociationThread[stateAbbrs50, priorityFunc[#, 1]& /@ stateAbbrs50];
	While[Total@stateEVs < total, { stateEVs, statePriorities } = AllocateOne[stateEVs, statePriorities, priorityFunc]];
	KeySort@stateEVs
)


(* ::Text:: *)
(*Here goes nothing:*)


results = calculateAllocations[];
{ results["AL"], results["KY"], results["CA"] }
Total@results


(* ::Text:: *)
(*And let's see how it performs with a huge House of Representations*)


bigHouse = calculateAllocations[2000];


(* ::Section:: *)
(*Visualizing the outputs*)


(* ::Text:: *)
(*Let's write a function to compare our calculated allocations to the official ones*)


compareAllocations[calculatedEVs_] := (
	diffs = calculatedEVs - houseOfRepresentatives;
	
	(* colors *)
	maxEV = Max[{Max[calculatedEVs], Max[houseOfRepresentatives]}];
	{ minDiff, maxDiff } = MinMax[diffs];
	colorScaleEV = ColorData[{"SiennaTones", "Reverse"}, Rescale[#1, {0, maxEV * 1.25}]]&;
	colorScaleDiff = ColorData[{"ValentineTones", "Reverse"}, Rescale[#1, {0, maxDiff * 1.25}]]&;

	(* cells as items *)
	header = Item[#, Alignment -> Right, ItemSize->{9, 1.5}]& /@ { "state", "actual reps", "calculated reps", "difference" };
	houseOfRepresentativesItems = Item[#, Background->colorScaleEV[#]]& /@ houseOfRepresentatives;
	calculatedEVItems = Item[#, Background->colorScaleEV[#]]& /@ calculatedEVs;
	diffItems = Item[#, Background->colorScaleDiff[#], ItemSize->{2,1.5}]& /@ diffs;
	totals = Item[Style[#, Bold], ItemSize->{4.8, 1.5}]& /@ { "Total", Total@houseOfRepresentatives, Total@calculatedEVs, Total@diffs };

	(* build rows of grids *)
	columns = { Keys@houseOfRepresentatives, Values@houseOfRepresentativesItems, Values@calculatedEVItems, Values@diffItems };	
	parts = First@Partition[Map[Append[columns[[#]], totals[[#]]]&, Range[4]], { 4, UpTo@13 }];
	rows = Map[Flatten, Transpose[{ header, # }]& /@ parts, {2}];
	Column[Grid[#, Frame -> All, ItemSize->All]& /@ rows, Spacings->0.25]
)
compareAllocations[results]


(* ::Text:: *)
(*Our function worked perfectly. Let's compare reality to our "Big House"*)


compareAllocations[bigHouse]


(* ::Section:: *)
(*Analyzing the results*)


(* ::Text:: *)
(*One way to measure the effectiveness of allocation is by measuring the ratio of the population to its number of representatives. We have the population conveniently baked in to the state data*)


statePopulations = AssociationThread[stateAbbrs50, statesByAbbreviation[#]["population_decennial"]["2010"]& /@ stateAbbrs50];
totalUSPopulation = 308745538; (* https://www.census.gov/data/tables/2010/dec/popchange-data-text.html *)


peoplePerRepresentative[allocation_] := (
	perRep = AssociationThread[stateAbbrs50, Round[statePopulations[#] / allocation[#] * 1.0]& /@ stateAbbrs50]
)
houseRatio = peoplePerRepresentative[houseOfRepresentatives];
bigHouseRatio = peoplePerRepresentative[bigHouse];


(* ::Text:: *)
(*Let's visualize the distribution of these ratios and use the standard deviation as a barometer*)


visualizeRatio[allocation_, ratio_] := (
	total = Total@allocation;
	ideal = Round[totalUSPopulation / total];
	ratioWithUS = Append[ratio, "US" -> ideal];
	sd = StandardDeviation[ratio] * 1.0;
	cov = sd / Mean@ratio;
	sdPrint = NumberForm[Round[sd], DigitBlock->3];
	
	colorScale = ColorData["Rainbow", Rescale[#1, MinMax@ratio]]&;
	colorFunction = Function[v, If[v == ideal, Yellow, colorScale[v]]];

	sorted = Sort@ratioWithUS;
	labels = Style[#, If[# == "US", Bold, Plain], FontSize->8]& /@ Keys@sorted;
	
	BarChart[sorted,
		ChartLabels->labels, GridLines->Automatic, ImageSize->{800, Automatic}, AspectRatio->1/4,
		PlotRangePadding -> { -1, 0 }, PlotRange->{ Automatic, Max[sorted] * 1.1 }, ChartStyle->Purple,
		ColorFunctionScaling -> False,
		ColorFunction->colorFunction,
		PlotLabel-> Column[{
			Style[ToString@total <> " Seats", FontSize->20],
			Style["Target: " <> ToString@NumberForm[ideal, DigitBlock -> 3] <> " people per representative", FontSize->14],
			Style["Standard Deviation: " <> ToString[sdPrint], FontSize->14],
			Style["Coefficient of Variation: " <> ToString@cov, FontSize->14]
		}, Alignment->Center]
	]
)
visualizeRatio[houseOfRepresentatives, houseRatio]


visualizeRatio[bigHouse, bigHouseRatio]


(* ::Section:: *)
(*Wrapping up*)


(* ::Text:: *)
(*We now have all the tools we need to play with different ways to configure the House. We'll import this Notebook into a playpen and get started.*)
