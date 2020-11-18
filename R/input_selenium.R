#' send_keys
#' @export
send_keys <- function(browser, keys = list(), return = "browser"){
  browser$sendKeysToElement(keys)
  if(return == "element") return(invisible(elem))
  if(return == "browser") return(invisible(browser))
}

#' keys
#' @export

keys <- RSelenium::selKeys

#' clear
#' @export

clear <- function(element){
  element$clearElement()
  return(invisible(element))
}

#' click
#' @export
click <- function(browser, value, using = "css selector", return = "browser"){
  if("remoteDriver" %in% class(browser)){
    elem <- browser$findElement(using, value)
    elem$clickElement()
    if(return == "element") return(elem)
    if(return == "browser") return(browser)
  } else {
    browser$clickElement()
  }
  Sys.sleep(sample(1:500, 1)/1000)
}
