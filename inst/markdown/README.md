## TODO
DATA

 - [ ] robust data preparation for all kinds of config
 - [ ] clean trailing whitespaces
 - [ ] more categories
 - [ ] averaging, quick overview
 - [ ] option to return data instead of plots
 - [ ] refactor to functions
 - [ ] avoid errors in case config refers to unavailable data, skip instead


PLOT

 - [ ] ordering of scenarios, years, regions
 - [ ] grouping of regions (currently causes layout problems)
 - [ ] detailed summary plots
 - [ ] clean interactive plot layout
 
 
## Questions

- [ ]  “short-term” only?
- [ ]  how to choose historical source as reference? if none specified - to all? or make source mandatory
- [ ]  automatic way of handling scenario names? via `cm_rcp_scen`? support comparison to other scenarios/Baseline?
- [ ]  grouping by topic? “net-zero” checks, eoc temp checks?
- [ ]  interactive heatmap useful? other plot ideas?
- [ ]  include summation checks?
- [ ]  only REMIND as model, support multiple models?
- [ ]  quantification of quality? easiest: number of “red”/”yellow”, best: ?
- [ ]  normalize value+thresholds to plot barplots, summarize?


## How-To validateScenario

**Cornerstones**

- input: one or multiple `REMIND_generic.mif`, one `historical.mif` and 
         one `validationConfig.csv`
- output: `validateScenarios.html`, an interactive Notebook
- output: data frame containing the information which elements failed or passed

**HTML Notebook**

- one plot per variable
- interactive plot gives all meta information that is necessary to understand
  why specific traffic light color is chosen

### General Config Rules

- each set of thresholds (one value for each `min/max_yel/red`) gets one line 
  in the config
- one variable per row (so far)
- later rows overwrite earlier rows (e.g. specifying a region after setting 
  thresholds for the same variable/time/scenario in all regions before)
- currently only either absolute OR relative targets are allowed for a variable

columns `region`, `scenario` and `period`

- empty cells mean "all"
- enter one or multiple entries, separated by `,` (exclusion not yet supported)

column `variable`

- cannot be empty
- only one entry allowed (for now)

### Category Definitions

**Category 1: comparison to reference values**

Evaluate whether results deviate from a reference source 
(from historical.mif or same variable from another scenario(not supported yet)).

Relative deviation is defined as `abs(run_value - ref_value)/ref_value`.

required:

- `metric`: `absolute` or `relative` (default)
- at least one threshold, e.g. `max_red`: define strong threshold, enter desired
  value (decimal for `relative`, e.g. `0.1` for `10%`)

optional:

- `ref_model`: specify one or multiple sources from the historical.mif the data 
               should be compared to. If empty, all available sources are used.
               If more than one source is chosen, their *mean* is used as 
               reference.
- `max_yel`: define a weaker threshold, otherwise same as `max_red`


**Category 2: Target Values**

Check whether variables are above or below absolute or relative thresholds.

required:

- `metric`: `absolute` (default) or `relative`. Default reference year
  is 2020. If you want to compare to another reference year, use `relative2015` etc.
- at least one of `min_red`, `min_yel`, `max_yel` or `max_red`


optional:
- `max_red`: define a strong upper threshold
- `min_red`: define a strong lower threshold 
- `min_yel`: define a weaker lower threshold 
- `max_yel`: define a weaker upper threshold 


**Category 3: Growth Rate**

Check yearly growth rates of variables.

**Category 4: Target Year**

Check whether a region reaches Net-Zero before or after a defined period.
