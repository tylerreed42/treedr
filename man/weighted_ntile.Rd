% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted_ntile.r
\name{weighted_ntile}
\alias{weighted_ntile}
\title{Weighted ntile}
\usage{
weighted_ntile(x, n, weight = 1, solo_levels = NULL)
}
\arguments{
\item{x}{Numeric vector}

\item{n}{Number of ntiles}

\item{weight}{Numeric vector containing weights of each record.  Default 1.}

\item{solo_levels}{Character vector of individual levels of x which should be binned individually.  These are "special levels" within x such as 0, or levels that otherwise do not make sense to bin with other levels.}
}
\value{
Ordered factor representing x binned into ntiles.  The number of ntiles is n plus the number of solo levels specified.  Levels are formatted as whole numbers.
}
\description{
An ntile function that respects weighting of observations.  Useful when creating binned variables for predictive models that will use a vector of weights.  This ensures an equal number of observations will be in each bucket with respect to how the model is created.
}
\examples{
df = data.frame(earned_premium = rexp(1000, 0.01), credit_score = sample(c(rep(997,50),sample(300:850, size = 950, replace = TRUE, prob = 850:300))))
df \%>\% mutate(credit_score_ntile = weighted_ntile(credit_score, 5, weight = earned_premium, solo_levels = c(997))) \%>\% group_by(credit_score_ntile) \%>\% summarize(across(earned_premium,sum))
}
