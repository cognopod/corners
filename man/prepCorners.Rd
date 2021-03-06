% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepCorners.R
\name{prepCorners}
\alias{prepCorners}
\title{prepCorners}
\usage{
prepCorners(data, solution, outcomeCol, ruleOut = "0=0;1=1")
}
\arguments{
\item{data}{is the raw data}

\item{solution}{is the QCApro output list from eQMC() function}

\item{outcomeCol}{is the original outcome column}

\item{ruleOut}{is the rule for recoding the target outcome in car::recode format (so if you looked at avoiding an outcome you need to use "0=1;1=0")}
}
\value{
A list of componments
   \item{pimChart}{The prime implicants table}
   \item{pimCorners}{The five numbers needed to draw the fourCorners plot for each of the paths. Counts are the number of cases with each outcome.}
\describe{
 \item{PATHOUT}{The number of cases on the path and with the outcome. When this is proportionally large, most of the cases can be explained by this configuration.}
 \item{OTHOUT}{The number of cases not on the path, but with the outcome. Cases not covered by this configuration.}
 \item{PATHNOT}{The number of cases on the path, but without the outcome. Counterfactuals.}
 \item{OTHNOT}{The number of cases not on the path, but without the outcome. Irrelevant.}
 \item{UNIQOUT}{The number of cases with the outcome that are uniquely covered by this path.}
}
}
\description{
prepCorners makes a dataframe containing the five numbers needed for each path in order to draw a deadNettle plot
}
