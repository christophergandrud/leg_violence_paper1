Two Sword Lengths Apart: Research on Violence in National Legislatures
===================

Christopher Gandrud

# Purpose

This repository contains all of the material needed to reproduce the analyses
in *Two Sword Lengths Apart: Credible Commitment Problems and Physical Violence
in Democratic National Legislatures*.  

# File Structure

The repository is organised into three folders:

- **Analysis**: R files to reproduce all of the analyses/create the plots shown
in the main article and Supplementary Materials.

- **Data**: Data files, descriptions, and R code used to clean/merge the raw data files
together.

- **PaperNamed**: TeX files used to create the main article and Supplementary
Materials.

## Analysis

The files in the *Analysis* folder are names such that:

- main\_analysis\_.\*.R are R files used in the main paper.

- supplemental\_analysis\_.\*.R are are files used in the Supplemental Materials.

- *functions* contains R functions that are used by the other files.

The files should be run in the same order in which they are numbered. You
will likely need to change the working directories in the files to one
suitable for your system.

## Data

The cleaned data set used for all of the analyses and figures is stored in
[Data/LegislativeViolenceMain.csv](Data/LegislativeViolenceMain.csv).

Variable descriptions can be found in
[Data/variable_descriptions.pdf](Data/variable_descriptions.pdf)

The R file gather.R was used to created this data set with raw data files
scrapped from the internet as well as manually downloaded and stored in the
[Data/raw](Data/raw) directory.

The legislative incidents data, including key sources, can be found in
[Data/violence_sources.csv](Data/violence_sources.csv).
You can download the data directly into R with:

```{S}
violence_incidents <- repmis::source_data('https://raw.githubusercontent.com/christophergandrud/leg_violence_paper1/master/Data/violence_sources.csv')
```

Highlights from reports about the incidents can be found in
[Data/violence_incidence_descriptions.md](Data/violence_incidence_descriptions.md).
