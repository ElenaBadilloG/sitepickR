# <img src="man/figures/logo.png" align="left" height="119" />
# sitepickR: A Generalizability R package
Elizabeth A. Stuart, Robert Olsen, Elena Badillo-Goicoechea (2022)

## Introduction

sitepickR is designed to select a representative sample of sites for a prospective impact evaluation, such as a randomized controlled trial (RCT). 

Like [generalizeR](https://nustat.github.io/generalizeR/), this package is designed for selecting schools but can be used to select any type of site defined by geography or administrative responsibility (e.g., county, job training center, health clinic). Unlike generalizeR, sitepicker is designed to select sites in two stages—first “units” that contain multiple sites located nearby and/or under the same administration and then “subunits” containing individual sites. Two-stage sampling may be necessary when the cost of the study depend on the number of units (e.g., school districts), and a one-stage sample of subunits (e.g., schools) would likely contain more units than the study can afford.

The main function in this package, selectMatch() lets the user carry out a two-level sample selection where the possibility of an initially selected participant not wanting to participate is anticipated. The procedure aims to reduce the bias (and/or loss of generalizability to the population) this could introduce.

In selecting units and subunits, sitepickR uses the cube method (e.g., Deville & Tillé, 2004; Tillé 2011). The cube method is a probability sampling method that is designed to satisfy criteria for balance between the sample and the population. Recent research has shown that this method performs well in simulations for studies of educational programs (Fay & Olsen, under review). To implement the cube method, sitepickerR uses the [sampling](https://cran.r-project.org/web/packages/sampling/index.html) R package. Users have the option to select units with equal probabilities or with probabilities proportional to their “size” measured in terms of the number of subunits nested within units.



In addition, sitepickR uses statistical matching to select possible replacement units. In education RCTs, the share of selected districts that agrees to participate tends to be low. To address this challenge, sitepickR selects and ranks up to 10 replacement districts for each districts selected using the cube method. Replacement districts are selected using statistical matching based on propensity score methods. To implement statistical matching, sitepickR uses the [MatchIt](https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html) R package

## References

Deville, J. C., & Tillé, Y. (2004). Efficient balanced sampling: the cube method. _Biometrika_, 91, 893-912.

Tillé, Y. (2011). Ten years of balanced sampling with the cube method: An appraisal. _Survey Methodology_, 37, 215-226.


_Main Inputs_: 
- dataframe with district ID, school ID, auxiliary variables
- M: number of sampled units
- Q: number of replacement units (default 10)
- K: number of sampled sub-units per unit (default 5)

_Output: (list)_:
- dataframe of M rows and Q+1 columns, where column 1 has IDs of initially selected units and columns 2: (Q+1) have replacement unit IDs in matching order
- dataframe of M rows and Q columns, where each cell (m x q) has the balance table between district m and district q, in the same ranked order as 1.
- dataframe with M x (Q+1) rows and K+1 columns, where column 1 has a unit ID and columns 2:(K+1) have sub-unit IDs corresponding to its unit.
- Balance diagnostics (via wrappers)
