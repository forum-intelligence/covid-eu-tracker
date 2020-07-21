globalVariables(
  c(
    "cases", "cases_electronic", "country", ".", "last_two_weeks", "province", "X", "X.2",
    "additional_cases", "deaths", "per_pop", "added", "hospitalized", "icu",
    "code", "confirmed_cases", "id", "CASES", "PROVINCE", "county", "healed",
    "per_capita", "previous1", "previous2", "previous3", "previous4",
    "Anzahl", "Area name", "Area type", "Bundesland", "Cumulative lab-confirmed cases",
    "Genesen", "Provincie", "Specimen date", "Totaal.gemeld", "X.1", "current_cases",
    "date_time", "hospital", "recovered", "type", "Date_of_report", "Province",
    "Total_reported"
  )
)

# read html for scraping
read_html <- function(url){
  xml2::read_html(url)
}

clean_aut_data <- function(aut_data){
  aut_data <- gsub(".*: ", "", aut_data)
  aut_data <- gsub("[[:space:]]und", ",", aut_data) # remove and
  aut_data <- gsub("[()]", "", aut_data) # remove brackets

  # split by province
  aut_data <- strsplit(aut_data, ",")[[1]] 
  aut_data <- gsub("-.*", "", aut_data)
  aut_data <- trimws(aut_data)
  aut_data <- gsub("^[[:space:]]", "", aut_data)

  # build data.frame
  provinces <- gsub("[0-9]*|[[:punct:]]", "", aut_data)
  provinces <- trimws(provinces)

  cases <- gsub("[A-z]*|ä|ö|ü|[[:punct:]]", "", aut_data)
  cases <- trimws(cases)
  cases <- as.integer(cases)

  data.frame(
    province = provinces,
    cases = cases,
    stringsAsFactors = FALSE
  )
}

cantons <- data.frame(
  canton = c("Geneva", "Vaud", "Basel City", "Aargau", "Zurich", "Ticino", 
    "Graubünden", "Valais", "Basel Country", "Bern", "Fribourg", 
    "Zug", "Schwyz", "Neuchâtel", "St. Gallen", "Lucerne", "Appenzell Ausserrhoden", 
    "Jura", "Thurgau", "Solothurn", "Appenzell Innerrhoden", "Glarus", 
    "Schaffhausen", "Nidwalden", "Obwalden", "Uri"),
  code = c(
    "GE", "VD", "BS", "AG", "ZH", "TI", "GR", "VS", "BL", "BE",
    "FR", "ZG", "SZ", "NE", "SG", "LU", "AR", "JU", "TG", "SO",
    "AI", "GL", "SH", "NW", "OW", "UR"
  ),
  stringsAsFactors = FALSE
)

#' Rename
#' 
#' Rename first few columns
#' 
#' @param df Sheet.
#' 
#' @keywords internal
rename_sheets <- function(df){
  names(df)[1:4] <- c(
    "state",
    "country",
    "lat", 
    "lon"
  )
  return(df)
}

#' Pivot
#' 
#' Change data from wide to long.
#' 
#' @param df Sheet.
#' 
#' @keywords internal
pivot <- function(df){
  tidyr::pivot_longer(
    df, 
    tidyselect::contains("/"),
    names_to = c("date"),
    values_to = c("cases"),
    values_ptypes = list(cases = "character")
  )
}

ukraine <- data.frame(
  ukrainian = c("Вінницька область", "Волинська область", 
    "Дніпропетровська область", "Донецька область", 
    "Житомирська область", "Закарпатська область", 
    "Запорізька область", "Івано-Франківська область", 
    "Кіровоградська область", "м. Київ", 
    "Київська область", "Львівська область", 
    "Луганська область", "Одеська область", 
    "Полтавська область", "Рівненська область", 
    "Сумська область", "Тернопільська область", 
    "Харківська область", "Херсонська область", 
    "Хмельницька область", "Чернівецька область", 
    "Черкаська область", "Чернігівська область"
  ),
  province = c(
    "Vinnytsia",
    "Volyn",
    "nipropetrovs'k",
    "Donets'k",
    "Zhytomyr",
    "Transcarpathia",
    "Zaporizhzhya",
    "Ivano-Frankivs'k",
    "Kirovohrad",
    "Kiev City",
    "Kiev",
    "L'viv",
    "Luhans'k",
    "Odessa",
    "Poltava",
    "Rivne",
    "Sumy",
    "Ternopil'",
    "Kharkiv",
    "Kherson",
    "Khmel'nyts'kyy",
    "Chernivtsi",
    "Cherkasy",
    "Chernihiv"
  ),
  stringsAsFactors = FALSE
)

clean_ukraine <- function(x){
  spl <- strsplit(x, ";\\n")[[1]]

  ukrainian <- purrr::map(x, function(x){
    gsub("—.*$", "", x) %>% 
      gsub("▪️", "", .) %>% 
      trimws()
  }) %>% 
    unlist()

  cases <- purrr::map(x, function(x){
    gsub("^.*—", "", x) %>% 
      gsub("\\(.*\\)", "", .) %>% 
      gsub("випадки|випадків|випадок|\\.", "", .) %>% 
      gsub(";", "", .) %>% 
      trimws() %>% 
      as.integer()
  }) %>% 
    unlist()

  tibble::tibble(
    ukrainian = ukrainian,
    cases = cases
  ) %>% 
    dplyr::left_join(ukraine, by = "ukrainian")

}

clean_netherlands <- function(x){
  x <- x[[1]] %>% 
    dplyr::slice(3:dplyr::n())

  purrr::transpose(x) %>% 
    purrr::map(function(row){
      strsplit(row[[1]], " ")
    }) %>% 
    purrr::map_dfr(function(r){
      tibble::tibble(
        province = r[[1]][1],
        cases = as.integer(r[[1]][2])
      )
    })
}

clean_norway <- function(x){
  gsub("^.*\\nFylke", "\nFylke", x)
}

# clean_portugal <- function(txt){
#   spl <- strsplit(txt, "\\n")
  
#   cases <- stringr::str_extract_all(spl, "[0-9]+") %>% 
#     as.integer()
# }
