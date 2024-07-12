#' @title Returns memory used in R session
#' @description Wrapper around \code{pryr::mem_used}
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link{datashield.connections_default}}.
#' @importFrom DSI datashield.aggregate datashield.connections_find
#' @return \code{ds.abs} Prints current memory used
#' @author Tim Cadman
#' @export
ds.memUsed <- function(x=NULL, newobj=NULL, datasources=NULL){

  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  cally <- call("memUsedDS")
  DSI::datashield.aggregate(datasources, cally)
}
