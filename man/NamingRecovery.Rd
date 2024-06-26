% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_documentation.R
\docType{data}
\name{NamingRecovery}
\alias{NamingRecovery}
\title{Picture naming during aphasia treatment}
\format{A data frame with 115 rows and 9 variables:
\describe{
\item{SubjectID}{subject ID, factor}
\item{Diagnosis}{participant's aphasia subtype: Anomic, Conduction, or Wernicke's aphasia}
\item{TestTime}{test iteration, 0 is the baseline at the start of treatment}
\item{Correct}{proportion correct naming responses}
\item{Semantic.error}{proportion semantic errors (cat - "dog")}
\item{Mixed.error}{proportion mixed errors (cat - "rat")}
\item{Formal.error}{proportion formal errors (cat - "mat")}
\item{Unrelated.error}{proportion unrelated errors (cat - "log")}
\item{Nonword.error}{proportion nonword errors (cat - "lat")}
}}
\source{
Moss Aphasia Psycholinguistics Database <mappd.org>
}
\usage{
NamingRecovery
}
\description{
Picture naming data from individuals with aphasia undergoing a treatment trial. Each participant was tested on a 175-item picture naming test (Philadelphia Naming Test) multiple times over the course of the treatment and responses were coded.
}
\keyword{datasets}
