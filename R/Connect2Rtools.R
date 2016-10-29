
#' @export
Connect2Rtools<-function(path="C:\\Rtools")
{
  rtools <- paste(path,"\\bin",sep="")
  gcc <- paste(path,"\\gcc-4.6.3\\bin",sep="")
  path <- strsplit(Sys.getenv("PATH"), ";")[[1]]
  new_path <- c(rtools, gcc, path)
  new_path <- new_path[!duplicated(tolower(new_path))]
  Sys.setenv(PATH = paste(new_path, collapse = ";"))
}