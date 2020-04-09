The report was generated on `r format.Date(Sys.time(), '%A, %B %d, %Y at %Hh%M %Z (%z)')`

# Preface

This is a personal analysis of the cases of covid-19.

I'm not an Epidemiologist! I'm a researcher with a background in computer science and bioinformatics.

The source code is at [averissimo/r-analysis-covid19](https://github.com/averissimo/r-analysis-covid19), [averissimo/covid19.de.data](https://github.com/averissimo/covid19.de.data) and [averissimo/covid19.pt.data](https://github.com/averissimo/covid19.pt.data)

## Other covid-19 confirmed/deaths analysis

* [World](https://averissimo.github.io/covid19-analysis/)
* [Germany](https://averissimo.github.io/covid19-analysis/germany.html) *(by state)*
* [Italy](https://averissimo.github.io/covid19-analysis/italy.html) *(by region)*
* [Spain](https://averissimo.github.io/covid19-analysis/spain.html) *(by region)*

## Age group analysis

* [Germany](https://averissimo.github.io/covid19.de.data/) *(by state and district)*

# Data

Data was retrieved from oficial sources.

* [EU CDC](https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data) *(if not available it uses [Johns Hopkins](https://github.com/CSSEGISandData/COVID-19/))*
* [Italian Protezione Civil](https://github.com/pcm-dpc/COVID-19) *(IT PC)*
* [Portuguese Direcção Geral da Saúde](https://covid19.min-saude.pt/relatorio-de-situacao/) *(PT DGS)*
* German Robert Koch's Institute](https://www.arcgis.com/home/item.html?id=dd4580c810204019a7b8eb3e0b329dd6&view=list#overview) *(DE RKI)*
* [Spanish Ministerio de Sanidad](https://covid19.isciii.es/) *(ES ISCIII)*
* [COVID Tracking Project](https://covidtracking.com/) for USA
    * Not a direct official source, but it scrappes from official sources given by individual state's health departments.

It's not realtime data and data presented may have 1 or more days of difference from real-time data. Check the caption on the figures to check when was the last data point.