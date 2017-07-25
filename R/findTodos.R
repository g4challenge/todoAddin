#' findTodo
#'
#' @return todo data.frame
#' @export
#'
#' @examples
findTodos <- function(){
  # Todo better pattern
  filenames <- list.files(".", pattern="*.R", full.names=TRUE, recursive=T) %>% as.data.frame(stringsAsFactors = F)
  colnames(filenames) <- c("path")
  ldf <- filenames %>% rowwise() %>% do(filename=.$path, code=readDplyr(.$path))
  return(ldf)
}

readDplyr <- function(path){
  readLines(path) %>% as.data.frame() %>%
    tibble::rownames_to_column() %>% rename(text=".") %>%
    filter(grepl("# ?\\bTODO\\b", text, ignore.case=T)) -> code # TODO better regex
  return(code)
}
