# How animals discriminate between stimuli: a meta-analysis
## How to repeat this analysis
### Literature search
1. Search results were saved into `litsearch/searches`
2. Results were combined and duplicates removed with `litsearch/combine_results.R`
	- Outputs a raw list of all studies: `combinedresults.csv`
3. This list was exported to an excel spreadsheet (`litsearch/litsearch_25_11.xlsx`), and studies from other sources were manually appended on separate sheets
### Data analysis
#### 1. Installing kber
We created the package `kber` to aid analysis, which can be found here: https://github.com/mzw22/kber/
This package can be downloaded and loaded with the following code:
```r
library(devtools) #allows downloads from github
install_github("mzw22/kber") #download package
library(kber)
```
#### 2. Data extraction
Input data is stored in the following locations:
- Datasets are in `data` sorted by study
- Metadata about each study is in `data/study_list.csv`

Input data was analysed using the `.R` files in `code/1_extract_data` (one per dataset). The functions used for this analysis are in `code/analysis_functions.R`. These files output model summaries into `results/data_extraction/models` and diagnostic plots into `results/data_extraction/plots`.
#### 3. Combining extracted data
Extracted data was combined into a single file with metadata using `code/2_combine_results.R`. This outputs a summary of all the datasets: `results/k_results.csv`
#### 4. Meta-analysis
The meta-analysis was conducted with `code/3_meta_analysis.R`, and outputs tables and figures into `results/meta_analysis`.