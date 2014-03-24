\name{criteria}
\alias{criteria}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generic criteria pollutant function
}
\description{
This function takes a data frame of criteria pollutant data and creates an object with the class "\code{criteria}" (or a list of objects) that can be used to summarize and analyze the data in reference to the U.S. EPA's National Ambient Air Quality Standards (http://www.epa.gov/air/criteria.html).
}
\usage{
criteria(data, parameter, monitor.id, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame with a column labeled "date" with the text format "YYYYMMDD", or "YYYYMMDDHH" for hourly data. The data frame may instead be formatted by the \code{import} function from the \code{openair} package. If the columns for the pollutant values are labeled appropriately, then the \code{parameter} argument does not have to be given. The data frame must not contain extra columns beside "date" and criteria pollutant(s). 
}
  \item{parameter}{
A character or vector giving the pollutant abbreviation ("co", "pb", "no2", "o3", "PM2.5", "PM10", "so2") or parameter code (see https://aqs.epa.gov/aqsweb/codes/data/Parameters-CRITERIA.html). The total number of columns in the data frame must equal the number of parameters in the character vector plus the "date" column.
}
  \item{monitor.id}{
%%     ~~Describe \code{conditional} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
