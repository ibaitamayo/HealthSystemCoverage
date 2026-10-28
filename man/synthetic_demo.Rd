\name{run_synthetic_demo}
\alias{run_synthetic_demo}
\alias{example_care_events}
\title{Fictional care events and a database-free demonstration}
\usage{
example_care_events()
run_synthetic_demo(path, process_maps = FALSE)
}
\arguments{
\item{path}{A new output directory. Existing directories are not overwritten.}
\item{process_maps}{Also generate selection and process maps. Requires optional process packages and Pandoc.}
}
\description{Creates explicitly fictional data, not sampled or transformed patient records. The default demonstration needs no database, credentials or optional process dependencies. It creates the architectural view and hierarchical table.}
\details{Small-cell masking is disabled for this fictional example only. Demonstration values must not be interpreted as empirical findings.}
\value{A fictional event data frame, or invisibly the demonstration output directory.}
\examples{
events <- example_care_events()
head(care_day_blocks(events))
}
