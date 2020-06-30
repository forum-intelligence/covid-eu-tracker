#' Crawl data
#' 
#' A wrapper functions for all \code{crawlers}.
#' 
#' @param date_time Date time of last update (dislpayed on dashboard).
#' @param puppet_ch,puppet_uk Paths to puppet js crawling files for UK and Switzerland.
#' @param state Where to push data to, dev of prod database.
#' 
#' @importFrom utils download.file unzip
#' 
#' @export
crawl_covid <- function(puppet_ch = system.file("crawl/crawl_ch.js", package = "covid.crawler"), puppet_uk = system.file("crawl/crawl_uk.js", package = "covid.crawler"), date_time = Sys.time(), state = c("dev", "prod")){
  
  add_norway(state)
  add_scotland(state)
  add_germany(state)
  add_italy(state)
  add_spain(state)
  add_ukraine(state)
  add_switzerland(puppet_ch, state)
  add_uk(puppet_uk, state)
  add_poland(state)
  add_netherlands(state)
  add_belgium(state)
  add_europe(state)
  add_sweden(state)
  add_update_time(date_time, state)

}

#' Individual Crawlers
#' 
#' Push data on individual countries and Europe via the ECDC.
#' 
#' @inheritParams crawl_covid
#' @param puppet Paths to puppet js crawling files for UK and Switzerland.
#' 
#' @note These functions can be all called at once with \code{\link{crawl_covid}}.
#' 
#' @rdname crawlers
#' @export
add_norway <- function(state = c("dev", "prod")){
  has_config() # check config present

  # get pdf link
  news_pdf_url <- read_html(url_no) %>% 
    rvest::html_node(".textual-block") %>% 
    rvest::html_nodes("a") %>% 
    .[1] %>% 
    rvest::html_attr("href") %>% 
    paste0("https://www.fhi.no", .)
  
  # download pdf
  tmp <- tempfile(fileext = ".pdf")
  utils::download.file(news_pdf_url, tmp)

  # read table
  col_names <- function(.) c("province", "cases", "per_capita")
  no_data <- tabulizer::extract_text(tmp, pages = 11) %>% 
    clean_norway()

  unlink(tmp) # delete pdf

  con <- connect(state)
  write_table(con, "norway", no_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export 
add_scotland <- function(state = c("dev", "prod")){
  # scotland
  col_names <- function(.) c("county", "cases", "hospital", "icu")
  sct_data <- read_html(url_sco) %>% 
    rvest::html_node(".publication-body") %>% 
    rvest::html_node("table") %>% 
    rvest::html_table() %>% 
    dplyr::rename_all(col_names) %>% 
    dplyr::mutate(
      icu = gsub("\\*", "", icu),
      cases = gsub("\\*", "", cases),
      cases = gsub(",", "", cases),
      hospital = gsub("\\*", "", hospital),
      icu = as.integer(icu),
      cases = as.integer(cases),
      hospital = as.integer(hospital),
      county = enc2native(county),
      county = gsub("\\(.*\\)", "", county),
      county = trimws(county)
    ) %>% 
    dplyr::mutate(
      county = dplyr::case_when(
        county == "Eileanan Siar" ~ "Eilean Siar",
        TRUE ~ county
      )
    ) %>% 
    dplyr::slice(2:dplyr::n())
  sct_data$county[1] <- "Ayrshire and Arran"

  con <- connect(state)
  write_table(con, "scotland", sct_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export 
add_germany <- function(state = c("dev", "prod")){
  ger_data <- jsonlite::fromJSON(url_de)$features$attributes
  names(ger_data) <- c("province", "cases")

  con <- connect(state)
  write_table(con, "germany", ger_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_italy <- function(state = c("dev", "prod")){
  ita_data <- utils::read.csv(url_ita, stringsAsFactors = FALSE)
  names(ita_data) <- c(
    "date", "country", "region_code", "region_name",
    "province_code", "province_name", "province_initials",
    "lat", "long", "cases", "comment"
  )
  ita_data$date <- as.POSIXct(ita_data$date)

  con <- connect(state)
  write_table(con, "italy", ita_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_spain <- function(state = c("dev", "prod")){
  # Spain
  col_names <- function(.) c("province", "cases")
  sp_data <- read_html(url_es) %>% 
    rvest::html_node(".imagen_texto") %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    .[[2]] %>% 
    gsub("\\.\\./\\.\\./\\.\\./\\.\\./\\.\\.", "", .) %>% 
    paste0("https://www.mscbs.gob.es", .)

  tmp <- tempfile(fileext = ".pdf")
  utils::download.file(sp_data, tmp)
  sp_data <- tabulizer::extract_tables(tmp, pages = 1, output = "data.frame") %>% 
    .[[2]] %>% 
    dplyr::select(X, X.1) %>% 
    dplyr::rename_all(col_names) %>% 
    dplyr::slice(2:dplyr::n()-1) %>% 
    dplyr::mutate(
      cases = gsub("[[:space:]].*$", "", cases),
      cases = gsub("\\.", "", cases),
      province = gsub("\\*", "", province),
      province = gsub("\\*\\*", "", province),
      cases = gsub("\\*\\*", "", cases),
      cases = gsub("\\.000$", "", cases),
      cases = gsub("\\.", "", cases),
      cases = as.integer(cases),
      province = dplyr::case_when(
        province == "separando estos casos. Madrid" ~ "Madrid",
        province == "un Ceuta" ~ "Ceuta",
        province == "corresponden con nuevos contagios. País Vasco" ~ "País Vasco",
        province == "de Baleares" ~ "Baleares",
        province == "diarias Asturias" ~ "Asturias",
        TRUE ~ province
      )
    ) %>% 
    dplyr::filter(!is.na(cases)) %>% 
    dplyr::select(province, cases)

  unlink(tmp)

  con <- connect(state)
  write_table(con, "spain", sp_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_ukraine <- function(state = c("dev", "prod")){
  # ukraine
  ua_data <- read_html(url_ua) %>% 
    rvest::html_node(".editor") %>% 
    rvest::html_nodes("li") %>% 
    .[1:25] %>% 
    rvest::html_text() %>% 
    clean_ukraine()

  con <- connect(state)
  write_table(con, "ukraine", ua_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_switzerland <- function(puppet = system.file("crawl/crawl_ch.js", package = "covid.crawler"), state = c("dev", "prod")){

  tmp <- tempfile(fileext = ".json")
  args <- paste(puppet, tmp)
  system2("node", args)

  ch_list <- jsonlite::fromJSON(tmp)

  ch_data <- ch_list %>% 
    purrr::map_dfr(function(html){
      base <- read_html(html)

      canton <- rvest::html_node(base, ".name") %>% rvest::html_text()
      cases <- rvest::html_node(base, ".cases") %>% rvest::html_text()
      deaths <- rvest::html_node(base, ".death") %>% rvest::html_text()

      tibble::tibble(
        code = canton,
        confirmed_cases = cases,
        deaths = deaths
      )
    }) %>% 
    dplyr::filter(!is.na(code)) %>% 
    dplyr::filter(code != "") %>% 
    dplyr::mutate(
      confirmed_cases = as.integer(confirmed_cases),
      deaths = as.integer(deaths)
    ) %>% 
    dplyr::left_join(cantons, by = "code")

  unlink(tmp)

  con <- connect(state)
  write_table(con, "switzerland", ch_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_poland <- function(state = c("dev", "prod")){
  col_names <- function(.) c("province", "cases", "deaths", "id")
  pl_data <- read_html(url_pl) %>% 
    rvest::html_node("#registerData") %>% 
    rvest::html_text() %>% 
    gsub('.*parsedData\":\"', "", .) %>% 
    gsub("\",\"fileName\".*$", "", .) %>% 
    gsub("\\\\", "", .) %>% 
    jsonlite::fromJSON() %>% 
    dplyr::rename_all(col_names) %>% 
    dplyr::select(-id) %>% 
    dplyr::mutate(
      cases = gsub(" ", "", cases),
      cases = as.integer(cases),
      deaths = as.integer(deaths)
    ) %>% 
    dplyr::slice(2:dplyr::n())

  con <- connect(state)
  write_table(con, "poland", pl_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_netherlands <- function(state = c("dev", "prod")){
  # netherlands
  nl_data <- read.delim(url_nl, sep = ";") %>% 
    dplyr::mutate(Date_of_report = as.Date(Date_of_report)) %>% 
    dplyr::filter(Date_of_report == max(Date_of_report)) %>% 
    dplyr::select(province = Province, cases = Total_reported) %>% 
    dplyr::group_by(province) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::ungroup()

  con <- connect(state)
  write_table(con, "netherlands", nl_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_belgium <- function(state = c("dev", "prod")){
  # Belgium
  be_data <- readr::read_csv(url_be, col_types = readr::cols()) %>% 
    dplyr::select(province = PROVINCE, cases = CASES) %>% 
    dplyr::group_by(province) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(!is.na(province)) %>% 
    dplyr::mutate(
      province = gsub('([[:upper:]])', ' \\1', province),
      province = trimws(province)
    )  

  con <- connect(state)
  write_table(con, "belgium", be_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_europe <- function(state = c("dev", "prod")){
  fl <- tempfile(fileext = "csv")
  curl::curl_download(url_eu, fl)
  eu_data <- readr::read_csv(fl, col_types = readr::cols())
  unlink(fl)
  names(eu_data) <- c("date_time", "day", "month", "year", "cases", "deaths", "country", "country_code", "country_code_iso", "population", "continent")
  eu_data$continent <- countrycode::countrycode(eu_data$country_code, "iso2c", "continent")   

  eu_data <- eu_data %>% 
    dplyr::mutate(
      date_time = as.Date(date_time, "%d/%m/%Y"),
      country = dplyr::case_when(
        country == "Czechia" ~ "Czech Rep.",
        TRUE ~ country
      )
    ) %>% 
    tidyr::replace_na(list(population = 0))

  con <- connect(state)
  write_table(con, "europe", eu_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_update_time <- function(date_time = Sys.time(), state = c("dev", "prod")){
  # updated time
  updated <- tibble::tibble(
    date_time = date_time 
  )

  con <- connect(state)
  write_table(con, "update", updated)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_sweden <- function(state = c("dev", "prod")){

  url <- "https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/bekraftade-fall-i-sverige"

  url_data <- read_html(url) %>% 
    rvest::html_node('[title="Excel-fil"]') %>% 
    rvest::html_attr("href")

  tmp <- tempfile(fileext = ".xlsx")
  download.file(url_data, destfile = tmp)

  data <- readxl::read_xlsx(tmp, sheet = 4)
  unlink(tmp)
  
  names(data) <- c("province", "cases", "per_capita", "icu", "deaths")

  data <- data %>% 
    dplyr::mutate(
      province = dplyr::case_when(
        province == "Jämtland Härjedalen" ~ "Jämtland",
        province == "Sörmland" ~ "Södermanland",
        TRUE ~ province
      )
    )

  con <- connect(state)
  write_table(con, "sweden", data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_austria <- function(state = c("dev", "prod")){

  # austria
  zip <- tempfile()
  dir <- tempdir()
  download.file("https://info.gesundheitsministerium.at/data/data.zip", zip)
  unzip(zip, exdir = dir)

  bundesland_path <- dir %>% 
    paste0("/Bundesland.csv") %>% 
    normalizePath()
  genesen_path <- dir %>% 
    paste0("/GenesenTodesFaelleBL.csv") %>% 
    normalizePath()
  aut_data <- readr::read_delim(bundesland_path, delim = ";", col_types = readr::cols())
  rec <- readr::read_delim(genesen_path, delim = ";", col_types = readr::cols())

  unlink(dir, recursive = TRUE, force = TRUE)

  aut_data <- aut_data %>% dplyr::group_by(Bundesland) %>% dplyr::summarise(current_cases = sum(Anzahl, na.rm = TRUE))
  rec <- rec %>% dplyr::group_by(Bundesland) %>% dplyr::summarise(recovered = sum(Genesen, na.rm = TRUE))

  aut_data <- dplyr::left_join(aut_data, rec, by = "Bundesland") %>% 
    dplyr::mutate(
      cases = current_cases + recovered
    )

  names(aut_data)[1] <- "province"

  con <- connect(state)
  write_table(con, "austria", aut_data)
  disconnect(con)
}

#' @rdname crawlers
#' @export
add_uk <- function(puppet = system.file("crawl/crawl_uk.js", package = "covid.crawler"), state = c("dev", "prod")){

  path <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"

  data <- readr::read_csv(path, col_types = readr::cols())

  data <- data %>% 
    dplyr::select(county = `Area name`, type = `Area type`, cases = `Cumulative lab-confirmed cases`, date = `Specimen date`) %>% 
    dplyr::filter(type == "Upper tier local authority") %>% 
    dplyr::filter(date == max(date)) %>% 
    dplyr::select(county, cases)

  tmp <- tempfile(fileext = ".json")
  args <- paste(puppet, tmp)
  system2("node", args)

  uk_list <- jsonlite::fromJSON(tmp)

  unlink(tmp)

  miss <- tibble::tibble(
    county = c("Northern Ireland", "Scotland", "Wales"),
    cases = c(uk_list[5], uk_list[8], uk_list[11])
  ) %>% 
    dplyr::mutate(
      cases = gsub(",", "", cases),
      cases = as.integer(cases)
    )

  data <- dplyr::bind_rows(data, miss)

  data$county <- gsub(", County of", "", data$county)
  data$county <- gsub(", City of", "", data$county)

  data <- data %>% 
    dplyr::mutate(
      county = dplyr::case_when(
        county == "Hackney and City of London" ~ "Greater London",
        county == "Hammersmith and Fulham" ~ "Greater London",
        county == "Haringey" ~ "Greater London",
        county == "Enfield" ~ "Greater London",
        county == "Barking and Dagenham" ~ "Greater London",
        county == "Barnet" ~ "Greater London",
        county == "Bexley" ~ "Greater London",
        county == "Brent" ~ "Greater London",
        county == "Bromley" ~ "Greater London",
        county == "Croydon" ~ "Greater London",
        county == "Ealing" ~ "Greater London",
        county == "Haringey" ~ "Greater London",
        county == "Harrow" ~ "Greater London",
        county == "Havering" ~ "Greater London",
        county == "Hillingdon" ~ "Greater London",
        county == "Hounslow" ~ "Greater London",
        county == "Kingston upon Thames" ~ "Greater London",
        county == "Merton" ~ "Greater London",
        county == "Newham" ~ "Greater London",
        county == "Romford" ~ "Greater London",
        county == "Redbridge" ~ "Greater London",
        county == "Richmond upon Thames" ~ "Greater London",
        county == "Sutton" ~ "Greater London",
        county == "Waltham Forest" ~ "Greater London",
        TRUE ~ county
      )
    ) %>% 
    dplyr::group_by(county) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::ungroup()

  con <- connect(state)
  write_table(con, "england", data)
  disconnect(con)
}

