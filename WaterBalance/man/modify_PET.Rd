% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WB_Functions.R
\name{modify_PET}
\alias{modify_PET}
\title{Modify PET}
\usage{
modify_PET(pet, slope, aspect, lat, freeze, shade.coeff = NULL)
}
\arguments{
\item{pet}{A time series vector of PET values.}

\item{slope}{Slope of the site (in degrees).}

\item{aspect}{Aspect of the site (in degrees).}

\item{lat}{Latitude of the site (in degrees).}

\item{freeze}{A vector of freeze factor values, calculated from Tmean. Values are 0-1.}

\item{shade.coeff}{(optional) A shade coefficient from 0-1. Default is 1.}
}
\description{
Modifies PET by heat load according to method by Lutz et al. (2010).
}
