# Sequential-Voting-Promotes-Collective-Discovery-in-Social-Recommendation-Systems
Data and analysis code for Celis et al. "Sequential Voting Promotes Collective Discovery in Social Recommendation Systems", ICWSM 2016.

To reproduce our analysis, plus supporting tests, cd into ./code/ and run 
Rscript analyze.R

To reproduce our plots, cd into ./code/ and run 
Rscript plot.R

Data:
ALLanswers-completed.csv      : assignment to conditions and participant responses
explanation-gradesUPDATED.csv : average grades given by the authors and our external validators to the top explanations in each condition
inequalities.csv              : Gini indices for the upvotes in each voting condition (produced by ./code/inequality.py)
scores.csv                    : parsed version of ALLanswers-completed.csv 
total-votes.csv               : total votes received for each explanation in each condition (used to calculate Gini indices)
