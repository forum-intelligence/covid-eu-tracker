globalVariables(
  c(
    "canton", "cases", "confirmed_cases", "country", "county",
    "name", "province", "province_name",
    "cases_log", "continent", "date_time", "region_name",
    "positive_test", "county", "population", "since_first_case",
    "deaths"
  )
)

pkg_file <- function(path){
  system.file(path, package = "covid.tracker")
}

disconnected <- sever::sever_default(
  title = "Disconnected", 
  subtitle = "Your session ended", 
  button = "Reconnect"
)

loader <- tagList(
  waiter::spin_1(),
  br(),
  "Loading data"
)

logo_url <- "https://www.weforum.org/assets/logo-b66768797e3f785791fd157ffc33c27eeca6d5100b7f34d418f50d206f1a8004.svg"

add_logo <- function(e, top = 30, left = 50){
  e %>% 
    echarts4r::e_image_g(
      left = left,
      top = top,
      z = 999,
      style = list(
        image = logo_url,
        width = 72,
        height = 45,
        opacity = .95
      )
    )
}

#' Database
#' 
#' Manage database connections for covid.
#' 
#' @param con Connection, as returned by \code{connect}.
#' 
#' @name connection
#' 
#' @keywords internal
connect <- function(){
  conf <- read_config()
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = conf$dbname,
    host = conf$host,
    port = 5432, user = conf$user,
    password = conf$password
  )
}

#' @rdname connection
#' @keywords internal
disconnect <- function(con){
  DBI::dbDisconnect(con)
}

#' Config
#' 
#' Create config file.
#' 
#' @export 
create_config <- function(){
  fl <- system.file("covid.yml", package = "covid.crawler")
  
  file.copy(fl, config_file)

  cat("Copied _covid.yml file, fill it in")
}

config_file <- "_covid.yml"

# read config
read_config <- function(){
  has_config()
  yaml::read_yaml(config_file)
}

# check config exsist
has_config <- function(){
  has_it <- file.exists(config_file)

  if(!has_it)
    stop("Missing _config.yml, see `create_config`")

  invisible()
}

# europe <- eu %>% 
#   dplyr::filter(continent == "Europe") %>% 
#   dplyr::mutate(country = "Europe") %>% 
#   dplyr::group_by(country, date_time) %>% 
#   dplyr::summarise(
#     cases = sum(cases, na.rm = TRUE),
#     population = sum(population)
#   ) %>% 
#   dplyr::ungroup()

# eu <- dplyr::bind_rows(eu, europe)

# first_case <- eu %>% 
#   dplyr::mutate(date_time = as.Date(date_time)) %>% 
#   dplyr::arrange(country, date_time) %>% 
#   dplyr::group_by(country) %>% 
#   dplyr::mutate(cases = cumsum(cases)) %>% 
#   dplyr::filter(cases > 0) %>% 
#   dplyr::summarise(first_case = min(date_time)) %>% 
#   dput()

# first_case <- structure(list(country = c("Afghanistan", "Albania", "Algeria", 
# "Andorra", "Angola", "Anguilla", "Antigua and Barbuda", "Argentina", 
# "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", 
# "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", 
# "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herz.", 
# "Brazil", "British Virgin Islands", "Brunei Darussalam", "Bulgaria", 
# "Burkina Faso", "Cambodia", "Cameroon", "Canada", "Cape Verde", 
# "Cases on an international conveyance Japan", "Cayman Islands", 
# "Central African Republic", "Chad", "Chile", "China", "Colombia", 
# "Congo", "Costa Rica", "Cote dIvoire", "Croatia", "Cuba", "Curaçao", 
# "Cyprus", "Czech Rep.", "Democratic Republic of the Congo", "Denmark", 
# "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", 
# "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", 
# "Ethiopia", "Europe", "Faroe Islands", "Fiji", "Finland", "France", 
# "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany", 
# "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guam", 
# "Guatemala", "Guernsey", "Guinea", "Guinea Bissau", "Guyana", 
# "Haiti", "Holy See", "Honduras", "Hungary", "Iceland", "India", 
# "Indonesia", "Iran", "Iraq", "Ireland", "Isle of Man", "Israel", 
# "Italy", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", 
# "Kenya", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", 
# "Lebanon", "Liberia", "Libya", "Liechtenstein", "Lithuania", 
# "Luxembourg", "Macedonia", "Madagascar", "Malaysia", "Maldives", 
# "Mali", "Malta", "Mauritania", "Mauritius", "Mexico", "Moldova", 
# "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", 
# "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands", "New Caledonia", 
# "New Zealand", "Nicaragua", "Niger", "Nigeria", "Norway", "Oman", 
# "Pakistan", "Palestine", "Panama", "Papua New Guinea", "Paraguay", 
# "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", 
# "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
# "San Marino", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", 
# "Singapore", "Sint Maarten", "Slovakia", "Slovenia", "Somalia", 
# "South Africa", "South Korea", "Spain", "Sri Lanka", "Sudan", 
# "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan", "Thailand", 
# "Timor Leste", "Togo", "Trinidad and Tobago", "Tunisia", "Turkey", 
# "Turks and Caicos islands", "Uganda", "Ukraine", "United Arab Emirates", 
# "United Kingdom", "United Republic of Tanzania", "United States of America", 
# "United States Virgin Islands", "Uruguay", "Uzbekistan", "Venezuela", 
# "Vietnam", "Zambia", "Zimbabwe"), first_case = structure(c(18317, 
# 18330, 18318, 18324, 18343, 18348, 18336, 18325, 18322, 18334, 
# 18286, 18318, 18321, 18337, 18316, 18330, 18339, 18320, 18296, 
# 18345, 18338, 18341, 18327, 18333, 18327, 18318, 18348, 18331, 
# 18329, 18332, 18289, 18328, 18287, 18342, 18297, 18341, 18337, 
# 18341, 18325, 18261, 18328, 18337, 18328, 18333, 18318, 18333, 
# 18334, 18331, 18323, 18332, 18319, 18340, 18344, 18323, 18322, 
# 18307, 18340, 18336, 18343, 18320, 18336, 18335, 18286, 18341, 
# 18341, 18291, 18286, 18340, 18334, 18339, 18319, 18289, 18334, 
# 18341, 18319, 18341, 18344, 18340, 18336, 18341, 18335, 18348, 
# 18334, 18341, 18328, 18333, 18326, 18321, 18291, 18323, 18312, 
# 18317, 18322, 18342, 18314, 18292, 18333, 18276, 18341, 18324, 
# 18336, 18335, 18337, 18316, 18340, 18346, 18324, 18314, 18338, 
# 18346, 18326, 18320, 18322, 18319, 18342, 18286, 18329, 18347, 
# 18329, 18336, 18341, 18321, 18329, 18321, 18331, 18339, 18342, 
# 18324, 18344, 18345, 18336, 18286, 18320, 18342, 18320, 18340, 
# 18342, 18320, 18319, 18317, 18319, 18327, 18331, 18342, 18329, 
# 18328, 18291, 18325, 18324, 18322, 18319, 18293, 18336, 18347, 
# 18336, 18334, 18320, 18324, 18324, 18328, 18336, 18285, 18324, 
# 18328, 18326, 18338, 18327, 18281, 18293, 18289, 18335, 18336, 
# 18293, 18318, 18344, 18282, 18274, 18343, 18328, 18334, 18324, 
# 18333, 18346, 18343, 18325, 18288, 18292, 18338, 18282, 18345, 
# 18336, 18337, 18336, 18285, 18340, 18342), class = "Date")), class = c("tbl_df", 
# "tbl", "data.frame"), row.names = c(NA, -196L))


# europe <- eu %>% 
#   dplyr::filter(continent == "Europe") %>% 
#   dplyr::mutate(country = "Europe") %>% 
#   dplyr::group_by(country, date_time) %>% 
#   dplyr::summarise(
#     cases = sum(cases, na.rm = TRUE),
#     population = sum(population)
#   ) %>% 
#   dplyr::ungroup()

# eu <- dplyr::bind_rows(eu, europe)

# first_case <- eu %>% 
#   dplyr::mutate(date_time = as.Date(date_time)) %>% 
#   dplyr::arrange(country, date_time) %>% 
#   dplyr::group_by(country) %>% 
#   dplyr::mutate(cases = cumsum(cases)) %>% 
#   dplyr::filter(cases > 100) %>% 
#   dplyr::summarise(first_case = min(date_time)) %>% 
#   dput()

# first day starts at 100th confirmed death

first_case <- structure(list(country = c("Afghanistan", "Albania", "Algeria", 
"Andorra", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", 
"Bahrain", "Bangladesh", "Belarus", "Belgium", "Bolivia", "Bosnia and Herz.", 
"Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Cambodia", 
"Cameroon", "Canada", "Cases on an international conveyance Japan", 
"Chile", "China", "Colombia", "Congo", "Costa Rica", "Cote dIvoire", 
"Croatia", "Cuba", "Cyprus", "Czech Rep.", "Democratic Republic of the Congo", 
"Denmark", "Djibouti", "Dominican Republic", "Ecuador", "Egypt", 
"El Salvador", "Estonia", "Ethiopia", "Europe", "Faroe Islands", 
"Finland", "France", "Gabon", "Georgia", "Germany", "Ghana", 
"Gibraltar", "Greece", "Guam", "Guatemala", "Guernsey", "Guinea", 
"Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", 
"Iraq", "Ireland", "Isle of Man", "Israel", "Italy", "Jamaica", 
"Japan", "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kosovo", 
"Kuwait", "Kyrgyzstan", "Latvia", "Lebanon", "Lithuania", "Luxembourg", 
"Macedonia", "Madagascar", "Malaysia", "Mali", "Malta", "Mauritius", 
"Mexico", "Moldova", "Montenegro", "Morocco", "Myanmar", "Netherlands", 
"New Zealand", "Niger", "Nigeria", "Norway", "Oman", "Pakistan", 
"Palestine", "Panama", "Paraguay", "Peru", "Philippines", "Poland", 
"Portugal", "Puerto Rico", "Qatar", "Romania", "Russia", "Rwanda", 
"San Marino", "Saudi Arabia", "Senegal", "Serbia", "Singapore", 
"Slovakia", "Slovenia", "Somalia", "South Africa", "South Korea", 
"Spain", "Sri Lanka", "Sweden", "Switzerland", "Taiwan", "Thailand", 
"Trinidad and Tobago", "Tunisia", "Turkey", "Ukraine", "United Arab Emirates", 
"United Kingdom", "United Republic of Tanzania", "United States of America", 
"Uruguay", "Uzbekistan", "Venezuela", "Vietnam"), first_case = structure(c(18350, 
18346, 18342, 18344, 18341, 18340, 18332, 18330, 18348, 18331, 
18359, 18352, 18328, 18352, 18344, 18336, 18346, 18341, 18347, 
18349, 18352, 18333, 18303, 18338, 18280, 18340, 18368, 18342, 
18349, 18341, 18350, 18345, 18334, 18354, 18331, 18360, 18343, 
18339, 18337, 18361, 18336, 18371, 18314, 18344, 18334, 18323, 
18370, 18353, 18322, 18348, 18358, 18334, 18358, 18362, 18356, 
18357, 18350, 18342, 18334, 18338, 18337, 18319, 18337, 18336, 
18356, 18335, 18316, 18367, 18314, 18356, 18345, 18348, 18355, 
18352, 18336, 18353, 18342, 18338, 18343, 18339, 18344, 18363, 
18331, 18364, 18345, 18350, 18340, 18345, 18353, 18344, 18371, 
18328, 18344, 18357, 18352, 18328, 18348, 18338, 18351, 18340, 
18357, 18339, 18336, 18336, 18335, 18351, 18333, 18336, 18339, 
18357, 18338, 18337, 18348, 18341, 18322, 18340, 18335, 18370, 
18340, 18313, 18324, 18346, 18328, 18328, 18340, 18337, 18357, 
18346, 18340, 18347, 18339, 18327, 18370, 18324, 18342, 18349, 
18347, 18344), class = "Date")), class = c("tbl_df", "tbl", "data.frame"
), row.names = c(NA, -138L))