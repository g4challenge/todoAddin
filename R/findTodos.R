#' findTodo
#'
#' @return todo data.frame
#' @export
#'
#' @examples
findTodos <- function(){
  # Todo better pattern
  filenames <- list.files(".", pattern="*.R", full.names=TRUE, recursive=T) %>%
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
