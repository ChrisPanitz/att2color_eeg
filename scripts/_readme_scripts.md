### readme scripts
R scripts for data reshaping, statistical analyses, and plotting


00_installPackages.R
- installs all required R packages but only if not already installed (will not overwrite existing packages with different version)

01_createDataframes.R
- reads Hilbert time courses from "hilbertData" folder
- creates dataframes for statistical analyses and plotting time courses & topographies
- saves them in "dataframes" folder

02_performanceData.R
- computes descriptive statistics on performance data (% correct trials & # of missing responses)
- plots histograms of performance data

03_anovaHilbert.R
- computes frequentist & Bayesian ANOVAs (Attention x Chromaticity x Driving Frequency)
- computes inclusion Bayes factors

04_plotTimecourseAndTopos.R
- plots Grand Averages of within-trial time courses of Hilbert amplitudes for average of all attended conditions and average of all ignored conditions, respectively
- plots topographies for (a) all ignored conditions aggregated, (b) all ignored conditions aggregated, (c) difference [attended - ignored]
- combines all plots into one figure and saves it as pdf

05_pairwiseComparisons.R
- computes frequentist & Bayesian t-tests as well as Cohen's d for pairwise comparisons between attended & ignored pictures...
- ...for values aggregated across all attended and ignored conditions respectively
- ...for values within color/gray and driving frequency conditions

06_tTestsAgainstZero.R
- computes frequentist & Bayesian t-tests as well as Cohen's d for deviations from zero (i.e., baseline) for attended & ignored pictures...
- ...for values aggregated across all attended and ignored conditions respectively
- ...for values within color/gray and driving frequency conditions

07_plotMeansWithRainplots.R
- plots Hilbert amplitudes (% change relative to baseline) for each participant and group means for each condition
- Using raincloud plots, shows single-participant values as scatterplot and smoothed distribution 