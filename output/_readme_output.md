# readme output

### anovaResults
Since Bayesian ANOVA with 3+ within-participant factors may take a while to compute, ANOVA output was saved into text files. Files contain the output of the anova.test function (frequentist ANOVA) and the bf_inclusion function (Inclusion Bayes Factors).

Separate files for main analyses and ANOVA excluding bad performers. The number in the file name ("excludeLowerThanXX") indicates the cutoff criterion of % correct responses to be included in the analysis.

anovaResults_EOGcontrol: separate ANOVAs (frequentist & Bayesian) for participants with and without significant increases in EOG activity from baseline to cued attention task



### indicesSigPermClusters
- text files containing sample indices (not time in ms!) of significant clusters in the of the Hilbert time course.
- rows = clusters
- 1st column = start point, 2nd column = end point

indicesSigPermClustersAtt.txt = test of cued/attended pictures against baseline
indicesSigPermClustersDiff.txt = test of cued/attended pictures against uncued/ignored pictures
(there were no significant clusters for uncued/ignored vs baseline)