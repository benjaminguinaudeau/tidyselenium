#' new_window
#' @export
new_window <- function(browser, ...){
  trash <- browser$open()
  return(invisible(browser))
}

#' go
#' @export
go <- function(browser, ...){
  browser$navigate(...)
  return(invisible(browser))
}

#' element
#' @export
element <- function(browser,  value, using = "css selector"){
  browser$findElement(using, value)
}

#' elements
#' @export
elements <- function(browser,  value, using = "css selector"){
  browser$findElements(using, value)
}

#' find_child
#' @export
find_child <- function(element, value = "", using = "css selector"){
  element$findChildElement(value = value, using = using)
  }

#' find_children
#' @export
find_children <- function(element, value = "", using = "css selector"){
  element$findChildElements(value = value, using = using)
  }

#' switch_to_window
#' @export
switch_to_window <- function (chrome, window_handle){
  qpath <- sprintf("%s/session/%s/window", chrome$serverURL, chrome$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))

  return(invisible(chrome))
}

#' switch_to_frame
#' @export
switch_to_frame <- function(chrome, div_value = "", div_using = "css selector",
                            frame_value = "", frame_using = "name"){
  elem <- chrome %>%
    element(div_value, div_using) %>%
    find_child(frame_value, frame_using)

  chrome$switchToFrame(elem)
  return(invisible(chrome))
}


#' set_attribute
#' @export
set_attribute <- function(elements, attr, value){
  if(class(elements)[[1]] != "list"){elements <- list(elements)}
  elements %>%
    purrr::map_chr(~{
      out <- .x$setElementAttribute(attributeName = attr, value = value)
      out <- ifelse(length(out) == 0, NA_character_, out[[1]])
      return(out)
    })
}

#' get_class
#' @export
get_class <- function(elems){
  elems %>% get_attribute("class")
}

#' get_text
#' @export
get_text <- function(element){element$getElementText()[[1]]}



#' highlight
#' @export
highlight <- function(element, wait = .1){
  element$highlightElement(wait = wait)
  return(invisible(element))
}

## execute
#' @export
execute <- function(browser, script){
  browser$executeScript(script)
}

## get_location
#' @export
get_location <- function(element){
  element$getElementLocation()
}







