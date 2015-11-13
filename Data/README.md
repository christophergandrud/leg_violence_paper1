# Data For Two Sword Lengths Apart

The cleaned data set used for all of the analyses and figures is stored in
[Data/LegislativeViolenceMain.csv](Data/LegislativeViolenceMain.csv).

Variable descriptions can be found in
[Data/variable_descriptions.pdf](Data/variable_descriptions.pdf)

The R file gather.R was used to created this data set with raw data files
scrapped from the internet as well as manually downloaded and stored in the
[Data/raw](Data/raw) directory.

The legislative incidents data, including key sources, can be found in
[Data/violence_sources.csv](https://github.com/christophergandrud/leg_violence_paper1/blob/master/Data/violence_sources.csv).
You can download the data directly into R with:

```{S}
violence_incidents <- repmis::source_data('https://raw.githubusercontent.com/christophergandrud/leg_violence_paper1/master/Data/violence_sources.csv')
```

Highlights from reports about the incidents can be found in
[Data/violence_incidence_descriptions.md](Data/violence_incidence_descriptions.md).
