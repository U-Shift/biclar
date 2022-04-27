
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biclar

<!-- badges: start -->
<!-- badges: end -->

The goal of `biclar` is to store code and (in the releases) data for
estimating cycling potential and influencing policy.

**biclar** is a tool for the design and assessment of different
scenarios of the cycling network models in the Lisbon metropolitan area
(LMA).

# Input data

The key datasets are as follows:

-   Trips dataset with Origin and Destination, at *Freguesia* level,
    disaggregated by transport mode, from Instituto National de
    Estatística (2018)  
-   [CAOP
    2020](https://www.dgterritorio.gov.pt/cartografia/cartografia-tematica/caop?language=en) -
    Official limits of Portuguese areas.
-   Road network from
    [OpenStreetMap](https://www.openstreetmap.org/#map=11/38.7053/-9.1585)  
-   Main public transport interfaces at Lisbon Metro Region, provided by
    [Transportes Metropolitanos de
    Lisboa](https://www.tmlmobilidade.pt/)

# Cenarios for cycling uptake

## Baseline

The baseline scenario makes use of the 2018 mobility survey data in
LMA.  
We considered all trips between *Freguesias*.

<!-- todo: include map of baseline scenario -->

See vignette [baseline scenario](articles/0_baseline_scenario.html) to
see how this was modeled.

## ENMAC targets

The National targets for cycling uptake were set to:

-   4% of all trips should be made by bicycle by 2025
-   10% of all trips should be made by bicycle by 2030

Cycling trips should replace car trips directly.

See vignette [ENMAC scenario](articles/1_emnac_scenario.html) to see how
this was modeled.

## Intermodal trips

See vignette [Intermodal scenario](articles/2_intermodal_scenario.html)
to see how this was modeled.

## E-bikes investment policy

See vignette [E-bike scenario](articles/3_ebikes_scenario.html) to see
how this was modeled.

# Methods

## PCT - Propensity to Cycle Tool

`biclar` uses the methods developed in [PCT.bike](https://pct.bike)
(Lovelace et al. 2017) for cycling uptake estimation and data
visualization.

## Jittering

For the disagregation of OD pairs at *Freguesias* level, we use [OD
Jittering](https://github.com/atumworld/odrust) (Lovelace, Félix, and
Carlino 2022) method, which better suits walking and cycling trips
modelling (shorter distances), instead of relying on centroids that
concentrate all the trips between areas.

The OD datasets, before and after jittering, are shown below.

<img src="man/figures/README-jitteredoverview-1.png" width="50%" /><img src="man/figures/README-jitteredoverview-2.png" width="50%" />

## Cycling routes

Use of [CyclingStreets.net](https://cyclinstreets.net) ([R
package](https://rpackage.cyclestreets.net/)) for fast and quiet bike
routes for baseline scenario.  
For e-bike scenario, we developed a proper algorithm, considering the
topography (and [`slopes`](https://docs.ropensci.org/slopes) package).

## Intermodal trips

We made use and developed a [methodology](https://github.com/npct/rail)
that considers replacing long trips by bike + train or ferry trips.

## Estimation of socioeconomic benefits

Health Economic Assessment Tool ([HEAT
v5.0](https://www.heatwalkingcycling.org/#how_heat_works)) for walking
and cycling by WHO.

# Results

## Cycling uptake in LMA and by Municipality

## Comparision with the cycling network plans by Municipality

Compare the modeled cycling networks (segments overlapping) with
expansion plans, by municipality.

<!-- todo: this is too heavy -->

<img src="man/figures/existingplanned.png" width="100%" />

We can view it in an [interactive map
here](https://ushift.tecnico.ulisboa.pt/content/tml/RedeExistentePrevista.html).

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-IMOB" class="csl-entry">

Instituto National de Estatística. 2018. “Mobilidade e Funcionalidade Do
Território Nas áreas Metropolitanas Do Porto e de Lisboa: 2017.” Lisboa.
<https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_publicacoes&PUBLICACOESpub_boui=349495406&PUBLICACOESmodo=2&xlang=pt>.

</div>

<div id="ref-Jittering2022" class="csl-entry">

Lovelace, Robin, Rosa Félix, and Dustin Carlino. 2022. “Jittering: A
Computationally Efficient Method for Generating Realistic Route Networks
from Origin-Destination Data.” *Findings*.
<https://doi.org/10.32866/001c.33873>.

</div>

<div id="ref-Lovelace2017" class="csl-entry">

Lovelace, Robin, Anna Goodman, Rachel Aldred, Nikolai Berkoff, Ali
Abbas, and James Woodcock. 2017. “The Propensity to Cycle Tool: An Open
Source Online System for Sustainable Transport Planning.” *Journal of
Transport and Land Use* 10 (1). <https://doi.org/gfgzf7>.

</div>

</div>
