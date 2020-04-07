COVID19 Analysis by @averissimo
================

This is an exploratory analysis of worldwide COVID-19 cases/deaths.

The full analysis can be found in one of the links below and updated 2
times a day

  - [World](https://averissimo.github.io/covid19-analysis/)
  - [Germany](https://averissimo.github.io/covid19-analysis/germany.html)
    *(by state)*
  - [Italy](https://averissimo.github.io/covid19-analysis/italy.html)
    *(by regione)*
  - [Bavaria](https://averissimo.github.io/covid19-analysis/bayer.html)
    *(Germany)*

Analysis by age group:

  - [Germany](https://averissimo.github.io/covid19.de.data/) *(by
    district)*

### Source code and tecnhology

Source code is available at
[averissimo/r-analysis-covid19](https://github.com/averissimo/r-analysis-covid19).
All the data and analysis was processed in [R programming
language](https://www.r-project.org/).

Data is retrieved from official sources and is not in real time. It is
updated once or twice a day in those sources, which reflect in the
reports in under an hour.

#### Data

World data is retrieved from [EU
CDC](https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data)
and unless it’s delayed this is the main source. When the release is
delayed we use [John
Hopkin’s](https://github.com/CSSEGISandData/COVID-19/).

Currently, the only exception is for Portugal’s data, which is updated
around noon *(local time)* and we extract that information directly from
the daily situation [PDF
report](https://covid19.min-saude.pt/relatorio-de-situacao/) and store
the updated dataset in a data package
[averissimo/covid19.pt.data](https://github.com/averissimo/covid19.pt.data).

**Notes**

Data is mainly from EU CDC with some updates from Italy, Portugal and
Germany’s institutes responsible with this pandemic.

We also use Eurostat and World Bank for relevant statistics and
populations.

Mapping for Italy codici\_regione taken from
[istat](https://www.istat.it/it/archivio/6789) *(note: two regions are
mapped to same NUTS 2, we chose one – see `download.it.data`)*
