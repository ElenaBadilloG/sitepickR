# <img src="man/figures/logo.png" align="left" height="119" />
# sitepickR: A Generalizability R package

Elena Badillo-Goicoechea, Robert Olsen, and Elizabeth A. Stuart (2022)

## Introduction

sitepickR is designed to select a representative sample of sites for a prospective impact evaluation, such as a randomized controlled trial (RCT). This package is designed for selecting schools but can be used to select any type of site defined by geography or administrative responsibility (e.g., county, job training center, health clinic).

In addition, sitepicker is designed to select sites in two stages —first “units” that contain multiple sites located nearby and/or under the same administration and then “sub-units” containing individual sites. Two-stage sampling may be necessary when the cost of the study depend on the number of units (e.g., school districts), and a one-stage sample of sub-units (e.g., schools) would likely contain more units than the study can afford. Further, the main function in this package, [selectMatch()](vignettes/selectMatch.html) lets the user carry out a two-level sample selection where the possibility of an initially selected participant not wanting to participate is anticipated. The procedure aims to reduce the bias (and/or loss of generalizability to the population) this could introduce. This enhances the functionality of related packages, such as [generalizeR](https://nustat.github.io/generalizeR/).

In selecting units and sub-units, sitepickR uses the cube method (e.g., Deville & Tillé, 2004; Tillé 2011). The cube method is a probability sampling method that is designed to satisfy criteria for balance between the sample and the population. Recent research has shown that this method performs well in simulations for studies of educational programs (Fay & Olsen, under review). To implement the cube method, sitepickerR uses the [sampling](https://cran.r-project.org/web/packages/sampling/index.html) R package. Users have the option to select units with equal probabilities or with probabilities proportional to their “size” measured in terms of the number of sub-units nested within units.

In addition, sitepickR uses statistical matching to select possible replacement units. In education RCTs, the share of selected districts that agrees to participate tends to be low. To address this challenge, sitepickR selects and ranks up to K replacement districts for each districts selected using the cube method. Replacement districts are selected using statistical matching based on propensity score methods. To implement statistical matching, sitepickR uses the [MatchIt](https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html) R package.

## Usage

Install seamlessly using the devtools package:

```
if(!require(devtools)){
    install.packages("devtools")
    }

devtools::install_github("ElenaBadilloG/sitepickR")

```
sitepickR's core sampling + matching procedure, implemented with the [selectMatch()]() function, consists of four main steps:

- Study sample design: identify: 1) a target population that has a nested structure (e.g. [school district:school]); 2) observable covariates on interest at the unit level; 3) observable covariates of interest at the sub-unit level.
- Randomly select an initial sample of units from the target population.
- Obtain a list of 'best' matches (e.g. replacement candidates) for each initially selected unit, in terms of the covariates of interest.
- Assess balance and match quality in terms of the covariates of interest:
    - Balance between initially selected ('original') units and the target population.
    - Balance between original units and each group of matches (1 to K, from closest to further).
    - Balance between sub-units associated to each unit replacement group and the original sub-units in the population, in terms of available covariates of interest, both at the unit and sub-unit level. 
## References

Deville, J. C., & Tillé, Y. (2004). Efficient balanced sampling: the cube method. _Biometrika_, 91, 893-912.

Tillé, Y. (2011). Ten years of balanced sampling with the cube method: An appraisal. _Survey Methodology_, 37, 215-226.

## Questions/troubleshooting? 

Reach out at:
	- egoicoe1@jhu.edu
	- robolsen@gwu.edu
	- estuart@jhu.edu
	
## Acknowledgements

Noah Greifer, maintainer of the [MatchIt](https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html) package for his valuable input.



