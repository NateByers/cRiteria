\name{criteria}
\alias{criteria}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructor for \code{criteria} object
}
\description{
This function creates an object with the class "\code{criteria}" that can be used to summarize the status of ambient air monitors in reference to the U.S. EPA's National Ambient Air Quality Standards (http://www.epa.gov/air/criteria.html).
}
\usage{
criteria(pollutant, state, county, monitor.id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pollutant}{
An abbreviation for a criteria pollutant ("co", "pb", "no2", "o3", "PM10", "PM2.5", or "so2" corresponding to carbon monoxide, lead, nitrogen dioxiode, ozone, particulate matter, fine particles, and sulfur dioxide, respectively).
}
  \item{monitor.id}{
Identification numbers in text with the following format: "XX-XXX-XXXX-X". The numbers correspond to state, county, site and POC codes respectively (see the US EPA's AQS code descriptions: http://www.epa.gov/ttn/airs/airsaqs/manuals/codedescs.htm). 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
