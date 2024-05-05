# biostatsTools
"biostatsTools" contains a small collection of custom functions designed for efficient data processing and statistical analysis. Built as part of the final project requirement for BMI510: Biostatistics for Machine Learning.

Functions include:
logLikBernoulli = function(data): Computes the parameter (p) that maximizes the log-likelihood for a given input of a Bernoulli-distributed vector (data).

survCurv = function (status, time): Calculates and plots survival curves S(t) based on given numerical vector (status) and time vector (time); includes demo with real-world dataset examples.

unscale = function(x): Reverses the centering/scaling of a standardized vector (x). 

pcApprox = function(x, npc): Provides an approximation of input data vector (x) based on a specified number of principal components (npc).

standardizeNames = function(data): Creates new tibble that standardizes variable names in input (data) to "small_camel" case, integrating functionalities from dplyr and janitor packages.

minimumN = function(x1, x2): Determines the minimum sample size required for t-tests of the mnll hypotheses that either mean(x1) = 0 or mean(x1) = mean(x2) with 80% power at alpha = 0.05. 

downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId): Retrieves and returns Redcap Report data securely as a tibble using RedCap API token (redcapTokenName) from a users' .REnviron file, queries the input url (redcapUrl) to return the RedcapReportId. 
