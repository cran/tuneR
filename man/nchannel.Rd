\name{nchannel}
\docType{methods}
\alias{nchannel}
\alias{nchannel,Wave-method}
\alias{nchannel,WaveMC-method}
\title{Number of channels}
\description{Get the number of channels from a Wave or WaveMC object}
\usage{
nchannel(object)
\S4method{nchannel}{Wave}(object)
\S4method{nchannel}{WaveMC}(object)
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}} or class \code{\link{WaveMC}}.}
}
\value{An integer, the number of channels given in the object.}
\seealso{\link{Wave-class}, \link{WaveMC-class}}

