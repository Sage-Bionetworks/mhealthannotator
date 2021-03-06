% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stored_annotation.R
\name{get_stored_annotation}
\alias{get_stored_annotation}
\title{Get Stored Annotation}
\usage{
get_stored_annotation(
  syn,
  parent_id,
  stored_filename,
  uid,
  keep_metadata,
  survey_colnames
)
}
\arguments{
\item{syn}{synapseclient}

\item{parent_id}{synapse parent Id}

\item{stored_filename}{filename of the annotation}

\item{uid}{unique identifier of the files needed to be annotate}

\item{keep_metadata}{what metadata to keep from the table}

\item{survey_colnames}{column for the survey}
}
\value{
a dataframe containing several columns
of the annotation information (survey input, metadata, annotation timestamp)
}
\description{
Get stored annotation in synapse,
it will search based on synapse parentId, the filename, and
the annotator (filename prefix). If not exist,
build an empty dataframe
}
