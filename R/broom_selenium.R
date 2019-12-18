#' get_attribute
#' @export
get_attribute <- function(elements, attr){
  if(class(elements)[[1]] != "list"){elements <- list(elements)}
  purrr::map_chr(elements, ~{
    out <- .x$getElementAttribute(attr)
    out <- ifelse(length(out) == 0, NA_character_, out[[1]])
    return(out)
  })
}


#' get_all_attribute
#' @export

get_all_attribute <- function(element, text = F){
  out <- element %>%
    tidyweb::get_attribute("outerHTML") %>%
    stringr::str_extract("<.*?>") %>%
    stringr::str_extract_all('\\w+=\\".*?\\"') %>% .[[1]] %>%
    stringr::str_split("\\=", n = 2) %>%
    purrr::map_dfc(~{
      tibble::tibble(stringr::str_remove_all(.x[2], '"')) %>%
        purrr::set_names(.x[1])
    }) %>%
    dplyr::mutate(element = list(element))

  if(nrow(out) == 0){
    out <- tibble(element = list(element))
  }

  if(text){
    out <- out %>% mutate(text = element %>% map_chr(get_text))
  }
  return(out)
}


#' check_element
#' @export
check_element <- function(chrome, value, using = "css selector"){
  element <- silently(try(chrome$findElement(using, value), silent = T))
  if(class(element)[1] == "try-error"){
    return(F)
  } else {
    return(T)
  }
}
