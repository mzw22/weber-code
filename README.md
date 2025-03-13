# How animals discriminate between stimuli: a meta-analysis
## Files provided in this dataset
### Supplementary data
- **Supplementary data 1:** Summarises the data extracted from each dataset, with the following headings:
	- Reference: Label for this study, corresponding to the Y axis of Figure 3(a)
	- Citation
	- Species, Common name
	- Stimulus: description of the stimulus for which discrimination ability was tested
	- Modality: sensory modality (electroreception, hearing, smell, taste, touch, vision)
	- Choice: choice type (whether the preference for the chosen stimulus was innate or conditioned)
	- Task: task type (whether the subject had to determine which stimulus had a higher magnitude or simply whether the stimuli differed)
	- Weber's law: whether the study mentioned Weber's law
	- k, Lower bound, Upper bound: fitted value of *k* and its 95% confidence intervals
	- Model fitted, Model test statistic, Model p value: type of linear model fitted to determine the value of *k*, and the fitted model's test statistic and p-value
	- Included: Whether the *k* estimate was included in the meta-analysis or not
	- Exclusion reason: Why the *k* estimate was excluded from the meta-analysis (if applicable), from the following reasons:
		- k failed: a value of *k* could not be fitted
		- AIC plot: the plot of AIC against *k* suggested problems with the fit
		- response plot: the plot of the response against perceived stimulus difference suggested overfitting or other problems
		- interval: the interval between the upper and lower 95% confidence intervals for the estimate of *k* was greater than 3, suggesting a low quality estimate
	- Source: whether the dataset was found in the database search or the Akre and Johnsen 2014 review
	- Data: the source of the data (either a repository reference, a figure number, or "Email" if the data were obtained by email)
	- Search date: either the original 25/11/2022 search ("2022") or the 02/12/2024 second search ("2024")
	- file prefix: prefix used to label all output files and diagnostic plots associated with this data
- **Supplementary data 2:** List of studies screened in the literature search
### Analysis
- **/code**: contains code used to perform the analysis
	- **/1_extract_data**: contains individual .R code files used to extract data from each dataset, the outputs of which are in **results/data_extraction**
	- **2_combine_results.R**: code to combine the outputs from **/1_extract_data** into a single file, **results/k_results.csv**
	- **3_meta_analysis.R**: code to perform the meta-analysis (using the output of combine_results)
	- **4_litsearch_summary.R**: code to summarise the literature search and count up the number of studies screened
	- **analysis_functions.R**: functions used during the data extraction
	- **chisqgrid.R**: function used to produce a grid of correlations between fixed effects
- **/data**: contains data used to perform the analysis
	- **study_list.csv**: contains metadata for each study (used to produce Supplementary Data 1)
	- raw data copied from original studies has been removed, but standardised versions used for analysis can be found in **/results/standard_data**
- **/litsearch**: contains the results of the literature searches conducted
	- **Supplementary Data 2 25-11-22.xlsx**: initial literature search performed on 25/11/22
	- **Supplementary Data 2 02-12-24.xlsx**: second literature search performed on 02/12/24 after review
- **/results**: contains the results of the analysis
	- **/data_extraction**: data extracted from each study to be used in the meta-analysis, sorted into several subfolders:
		- **/models**: contains models fitted to each dataset to estimate the value of *k*
		- **/plots**: contains diagnostic plots for the models fitted to each dataset to estimate the value of *k*
			- **/k_fit**: AIC value of the model plotted against *k*
			- **/k_response**: subject response against perceived difference with *k* = fitted value of *k*
			- **/k0_response**: subject response against perceived difference with *k* = 0
			- **/k1_response**: subject response against perceived difference with *k* = 1
	- **/figures**: contains figures produced during the meta-analysis
	- **/meta_analysis**: contains tables produced during the meta-analysis
		- **colinearity.csv**: colinearity between fixed effects
		- **k_species.csv**: average value of *k* calculated for each species
		- **k_table.csv**: same as Supplementary Data 1
		- **mods_table.csv**: how *k* varies with fixed effects
		- **mods_vif.csv**: Variance Inflation Factors for the fixed effects
		- **n_estimates.csv**: counts up how many datasets follow Weber's law, near miss, opposite miss etc.
		- **variance_explained.csv**: proportion of variance explained by each random effect
	- **/standard_data**: data from each study in a standardised format, with the following column headers:
		- a = magnitude of stimulus a;
		- b = magnitude of stimulus b;
		- abs_diff = |a - b|;
		- mean_ab = mean of a and b;
		- response = some measure of discrimination (varies between studies);
		- N = no. of trials contributing to the response (if applicable)
	- **k_results.csv**: all metadata from **/data/study_list.csv** combined with all models from **/results/data_extraction/models**
## Installing kber
We created the package `kber` to aid analysis, which can be found here: https://github.com/mzw22/kber/
This package can be downloaded and loaded with the following code:
```r
library(devtools) #allows downloads from github
install_github("mzw22/kber") #download package
library(kber)
```