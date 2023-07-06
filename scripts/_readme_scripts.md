### readme scripts
R scripts for data reshaping, statistical analyses, and plotting


00_installPackages.R
- installs all required R packages but only if not already installed (will not overwrite existing packages with different version)

01_createDataframes.R
- reads Hilbert time courses from "hilbertData" folder
- creates dataframes for statistical analyses and plotting time courses & topographies
- saves them in "dataframes" folder

02_performanceData.R
- computes descriptive statistics on performance data (% correct trials, # of missing responses, misses for different targets, zero ratings for trials with/without targets present)
- plots histograms of performance data

03_anovaHilbert.R
- computes frequentist & Bayesian ANOVAs (Attention x Chromaticity x Driving Frequency)
- computes inclusion Bayes factors

03b_anovaHilbert_controlEOG.R
- computes frequentist & Bayesian ANOVAs (Attention x Chromaticity x Driving Frequency)
- computes inclusion Bayes factors
- separate for participants with and without significant changes in EOG activity from baseline to cued attention phase

04_ permTests_Timecourse.R
- compute cluster-based permutation test on timecourse data (cf. Maris & Oostenveld, 2007, Journal of Neuroscience Methods)
- directed tests: uncued/ignored < baseline; cued/attended > baseline; cued/attended > uncued/ignored

05_plotTimecourseAndTopos.R
- plots Grand Averages of within-trial time courses of Hilbert amplitudes for average of all attended conditions and average of all ignored conditions, , marks time windows that are significant by means of permutation testing
- plots topographies for (a) all ignored conditions aggregated, (b) all ignored conditions aggregated, (c) difference [attended - ignored]
- combines all plots into one figure and saves it as pdf

05b_plotTimecourseAndTopos_controlEOG.R
- plots Grand Averages of within-trial time courses of Hilbert amplitudes for average of all attended conditions and average of all ignored conditions, respectively
- separate for participants with and without significant changes in EOG activity from baseline to cued attention phase

06_pairwiseComparisons.R
- computes frequentist & Bayesian t-tests as well as Cohen's d for pairwise comparisons between attended & ignored pictures...
- ...for values aggregated across all attended and ignored conditions respectively
- ...for values within color/gray and driving frequency conditions
- Rank test also available

07_tTestsAgainstZero.R
- computes frequentist & Bayesian t-tests as well as Cohen's d for deviations from zero (i.e., baseline) for attended & ignored pictures...
- ...for values aggregated across all attended and ignored conditions respectively
- ...for values within color/gray and driving frequency conditions
- Rank test also available

08_plotMeansWithRainplots.R
- plots Hilbert amplitudes (% change relative to baseline) for each participant and group means for each condition
- Using raincloud plots, shows single-participant values as scatterplot and smoothed distribution 