rm_dirs <- function(dir_nm) {
  if(dir.exists(dir_nm) == FALSE){
    unlink(dir_nm, recursive = TRUE)
    print(paste("removing file path",dir_nm))

  } else {
    print(paste(dir_nm,"file path not present"))
  }
}
