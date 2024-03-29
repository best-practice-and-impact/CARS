% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency-tables.R
\name{summarise_coding_tools}
\alias{summarise_coding_tools}
\title{Summarise coding tools}
\usage{
summarise_coding_tools(
  data,
  type = list("knowledge", "access"),
  prop = TRUE,
  sample = FALSE
)
}
\arguments{
\item{data}{full CARS dataset after pre-processing}

\item{type}{type of table (knowledge or access)}

\item{prop}{whether to return proportion data (0-1). TRUE by default. Assumes mutually exclusive response options.}

\item{sample}{additionally returns count and sample size. FALSE by default}
}
\value{
frequency table (data.frame)
}
\description{
calculate frequency table coding tools (knowledge or access)
}
