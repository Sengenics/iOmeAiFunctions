% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC_Function.R
\name{BG_count_function}
\alias{BG_count_function}
\title{Background QC}
\usage{
BG_count_function(plot_data, flag_df, col_name, spot_total_number)
}
\arguments{
\item{plot_data}{A data.frame in long format with at least columns:
\code{Sample}, \code{value} (BG values), and \code{key = 'BG'}.}

\item{flag_df}{A data.frame with one row per sample and a column
named according to \code{col_name}, containing the threshold for that sample.}

\item{col_name}{A string specifying the column in \code{flag_df} that holds
the per-sample BG threshold to compare against (e.g., "BG_overlap").}

\item{spot_total_number}{The total number of array spots per sample
(used to normalize the count into a percentage).}
}
\value{
The original \code{flag_df}, augmented with a new column
\code{<col_name>_BG_Percentage}, containing the percentage of
BG values above the specified threshold for each sample.
}
\description{
Calculate the percentage of background (BG) values exceeding a threshold per sample
}
\details{
This function compares background intensity values (BG) for each sample
against a user-specified threshold column in the summary \code{flag_df}. It computes
the percentage of BG values that exceed this threshold and adds the result as
a new column to \code{flag_df}, named \code{<col_name>_BG_Percentage}.
}
\note{
Version 1.0.0 from
QC_Function.R
}
\examples{
BG_count_function(plot_data, flag_df, col_name = "BG_overlap", spot_total_number = 200)
}
