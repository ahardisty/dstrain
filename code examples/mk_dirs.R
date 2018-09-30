#' mk_dir
#'
#' @param dir_nm
#'
#' @return
#' @export
#'
#' @examples
mk_dirs <- function(dir_nm) {
  if(dir.exists(dir_nm) == FALSE){
    dir.create(dir_nm, recursive = TRUE)
    print(paste("creating required file path",dir_nm))

  } else {
    print(paste(dir_nm,"file path already present"))
  }
}
