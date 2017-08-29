#' findTodo
#'
#' @return todo data.frame
#' @export
#'
#' @examples
findTodos <- function(){
  DIR <- rstudioapi::getActiveProject()

  rFiles <- list.files(path = DIR, pattern = "(?i)\\.r$", recursive = TRUE)
  rmdFiles <- list.files(path = DIR, pattern = "(?i)\\.rmd$", recursive = TRUE)
  filenames <- c(rFiles, rmdFiles) %>%
    as.data.frame(stringsAsFactors = F)
  colnames(filenames) <- c("path")
  ldf <- filenames %>% dplyr::rowwise() %>%
    dplyr::do(filename=.$path, code=readDplyr(.$path)) %>%
    tidyr::unnest(filename) %>% tidyr::unnest(code) %>% as.data.frame()
  return(ldf)
}

readDplyr <- function(path){
  readLines(path) %>% as.data.frame(stringsAsFactors=F) %>%
    tibble::rownames_to_column() %>% dplyr::rename(text=".") %>%
    dplyr::filter(grepl("# ?\\bTODO\\b", text, ignore.case=T)) -> code # TODO better regex
  return(code)
}
