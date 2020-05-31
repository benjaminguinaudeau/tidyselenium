#' chrome_init
#' @export
chrome_init <- function(name = "", view = T, ua = NULL, cache = NULL){
  browser_init(browser = "chrome", name = name, view = view, ua = ua, cache = cache)
}

#' firefox_init
#' @export
firefox_init <- function(name = "", view = T, ua = NULL, cache = NULL){
  browser_init(browser = "firefox", name = name, view = view, ua = ua, cache = cache)
}

#' browser_init
#' @export
browser_init <- function(browser = "chrome", name = "", view = T, ua = NULL, cache = NULL){
  
  if(!dockeR::is_docker_running()){stop("Docker daemon is not running, please start it and try again")}
  
  name <- ifelse(name == "", browser, name)
  
  if(!name %in% dockeR::existing_containers()){
    dockeR::create_container(glue::glue("selenium/standalone-{browser}-debug"), name, other_arguments = "--shm-size=2g")
    bashR::wait(4, .5)
  }
  
  if(name %in% dockeR::stopped_containers()){
    dockeR::start_container(name)
    bashR::wait(4, .5)
  }
  if(name %in% dockeR::running_containers()){
    browser <- dockeR::quiet(get_driver(port = dockeR::get_port(name, 4444), ua = ua, cache_id = cache, browser = browser))
  }
  
  if(view == T){dockeR::view_container(name)}
  return(browser)
}


#' get_driver
#' @export
get_driver <- function(port, ua = NULL, browser = "chrome", cache_id = NULL){
  
  if(!is.null(ua)){
    if(is.character(ua)){
      ua <- glue::glue('--user-agent="{ua}"')
    } else {
      ua <- glue::glue('--user-agent="{stringr::str_subset(tidyselenium::user_agents$user_agent, "hrome")[ua]}"')
    }
  }
  
  if(browser == "chrome"){
    ecaps <- list(
      chromeOptions =
        list(
          prefs = list(
            "profile.default_content_settings.popups" = 0L
            # "download.prompt_for_download" = F
            # #"download.default_directory" = "~/extract_temp"
          ),
          args = purrr::discard(
            c(
              '--disable-dev-shm-usage',
              '--disable-gpu',
              "--no-sandbox",
              "--disable-setuid-sandbox",
              "--disable-infobars",
              "--window-position=0,0",
              "--ignore-certifcate-errors",
              "--ignore-certifcate-errors-spki-list",
              ifelse(is.null(ua), "", ua), 
              ifelse(is.null(cache_id), "", glue::glue('--user-data-dir=tmp/cache/{cache_id}') )
            ), # '--no-sandbox', '--headless') #  '--window-size=1200,1800' , ,
            ~.x == ""
          )
        )
    )
  } else if (browser == "firefox"){
    ecaps <- RSelenium::makeFirefoxProfile(
      purrr::discard(
        list(
          "general.useragent.override" = ifelse(!is.null(ua), stringr::str_subset(tidyselenium::user_agents$user_agent, "irefox")[ua], ""),
          "browser.cache.disk.enable" = TRUE,
          "browser.cache.memory.enable" = TRUE,
          "browser.cache.offline.enable" = TRUE,
          "network.http.use-cache" = TRUE,
          "browser.cache.disk.parent_directory" = ifelse(!is.null(cache_id), glue::glue("/home/seluser/{cache_id}"), ""),
          "browser.download.dir" = "/tmp/"
        ), 
        ~.x == ""
      )
    )
  } else {
    ecaps <- list()
  }
  
  driver <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = port,
    browserName = browser,
    extraCapabilities = ecaps
  )
  
  return(driver)
}

#' renew_window
#' @export
refresh_window <- function(chrome, max = T){
  if(!max){max_size <- function(x) return(x)}
  chrome %>%
    close_all %>% 
    open %>% 
    max_size
  return(invisible(chrome))
}


#' open.remoteDriver
#' @export
open.remoteDriver <- function(chrome){
  dockeR::quiet(chrome$open())
  return(invisible(chrome))
}

#' close_all
#' @export
close_all <- function(chrome){
  chrome$closeall()
  return(invisible(chrome))
}

#' max_size
#' @export
max_size <- function(chrome){
  chrome$maxWindowSize()
  return(invisible(chrome))
}


#' screenshot
#' @export
screenshot <- function(browser, file = NULL, display = T, useViewer = T){
  browser$screenshot(file = file, display = display, useViewer = useViewer)
  return(invisible(browser))
}

#' new_window
#' @export
new_window <- function(port = 4444, prune = T, browser = "chrome"){
  tmp <- get_driver(port = as.integer(port), browser = browser)
  if(prune) tmp$closeall()
  tmp$open()
  return(tmp)
}

#' get_source_code
#' @export
get_source_code <- function(browser, filepath = NULL){
  
  tmp <- browser$executeScript("return window.document.getElementsByTagName('html')[0].outerHTML")
  
  page <- tmp[[1]] %>%
    xml2::read_html(.)
  
  if(is.null(filepath)){
    return(page)
  } else {
    page %>% xml2::write_html(., file = filepath)
    message(glue::glue("Source code was saved under { filepath }"))
  }
  
}

#' get_real_source_code
#' @export
get_real_source_code <- function(browser, filepath = NULL){
  
  tmp <- browser$executeScript("return window.document.getElementsByTagName('html')[0].innerHTML")
  
  page <- tmp[[1]] %>%
    xml2::read_html(.)
  
  if(is.null(filepath)){
    return(page)
  } else {
    page %>% xml2::write_html(., file = filepath)
    message(glue::glue("Source code was saved under { filepath }"))
  }
  
}
