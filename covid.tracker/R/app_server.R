#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  echarts4r::e_common(
    font_family = "Raleway"
  )

  sever::sever(html = disconnected, bg_color = "#005c9c")

  # get data
  con <- golem::get_golem_options("con")
  eu <- DBI::dbReadTable(con, "europe")
  it <- DBI::dbReadTable(con, "italy")
  de <- DBI::dbReadTable(con, "germany")
  at <- DBI::dbReadTable(con, "austria")
  ch <- DBI::dbReadTable(con, "switzerland")
  #fr <- DBI::dbReadTable(con, "france")
  sp <- DBI::dbReadTable(con, "spain")
  en <- DBI::dbReadTable(con, "england")
  no <- DBI::dbReadTable(con, "norway")
  pl <- DBI::dbReadTable(con, "poland")
  sc <- DBI::dbReadTable(con, "scotland")
  se <- DBI::dbReadTable(con, "sweden")
  nl <- DBI::dbReadTable(con, "netherlands")
  ua <- DBI::dbReadTable(con, "ukraine")
  be <- DBI::dbReadTable(con, "belgium") %>% 
    dplyr::mutate(
      province = dplyr::case_when(
        province == "Limburg" ~ "Limburg ",
        province == "Luxembourg" ~ "Luxembourg ",
        TRUE ~ province
      )
    )
  
  updated <- DBI::dbReadTable(con, "update")

  output$last_updated <- renderUI({
    d <- format(max(updated$date), "%d %b at %H:%M %Z")
    span("Last updated on", d, class = "pull-right", style="padding:25px;")
  })

  # bind provinces
  it <- it %>% 
    dplyr::filter(date == max(date)) %>%
    dplyr::mutate(
      region_name = dplyr::case_when(
        region_name == "P.A. Bolzano" ~ "Trentino-Alto Adige",
        region_name == "P.A. Trento" ~ "Trentino-Alto Adige",
        TRUE ~ region_name
      )
    ) %>% 
    dplyr::group_by(region_name) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(name = region_name, cases) %>% 
    dplyr::mutate(country = "Italy")
  
  eu <- dplyr::mutate(
      eu,
      country = dplyr::case_when(
        country == "Czech_Republic" ~ "Czech Rep.",
        country == "Bosnia_and_Herzegovina" ~ "Bosnia and Herz.",
        country == "North_Macedonia" ~ "Macedonia",
        TRUE ~ country
      ),
      country = gsub("_", " ", country)
    )

  ch <- ch %>% 
    dplyr::mutate(
      canton = dplyr::case_when(
        canton == "Basel" ~ "Basel-Landschaft",
        canton == "Basel City" ~ "Basel-Landschaft",
        TRUE ~ canton
      )
    ) %>% 
    dplyr::group_by(canton, code) %>% 
    dplyr::summarise(
      confirmed_cases = sum(confirmed_cases)
    ) %>% 
    dplyr::ungroup()

  be <- be %>% 
    dplyr::mutate(
      province = dplyr::case_when(
        province == "Brussels" ~ "Bruxelles-Capitale - Brussel-Hoofdstad",
        province == "West Vlaanderen" ~ "West-Vlaanderen",
        province == "Oost Vlaanderen" ~ "Oost-Vlaanderen",
        province == "Vlaams Brabant" ~ "Vlaams-Brabant",
        TRUE ~ province
      )
    )
  
  de <- dplyr::select(de, name = province, cases) %>% dplyr::mutate(country = "Germany")
  at <- dplyr::select(at, name = province, cases) %>% dplyr::mutate(country = "Austria")
  ch <- dplyr::select(ch, name = canton, cases = confirmed_cases) %>% dplyr::mutate(country = "Switzerland")
  #fr <- dplyr::select(fr, name = province, cases) %>% dplyr::mutate(country = "France")
  sp <- dplyr::select(sp, name = province, cases) %>% dplyr::mutate(country = "Spain")
  en <- dplyr::select(en, name = county, cases) %>% dplyr::mutate(country = "England")
  no <- dplyr::select(no, name = province, cases) %>% dplyr::mutate(country = "Norway")
  sc <- dplyr::select(sc, name = county, cases) %>% dplyr::mutate(country = "Scotland", continent = "Europe")
  pl <- dplyr::select(pl, name = province, cases) %>% dplyr::mutate(country = "Poland", name = tools::toTitleCase(name))
  se <- dplyr::select(se, name = province, cases) %>% dplyr::mutate(country = "Sweden")
  nl <- dplyr::select(nl, name = province, cases) %>% dplyr::mutate(country = "Netherlands")
  ua <- dplyr::select(ua, name = province, cases) %>% dplyr::mutate(country = "Ukraine")
  be <- dplyr::select(be, name = province, cases) %>% dplyr::mutate(country = "Belgium")

  # totals
  #total_fr <- fr %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_es <- sp %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_de <- de %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_it <- it %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_no <- no %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_pl <- pl %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_ch <- ch %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_se <- se %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_nl <- nl %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_ua <- ua %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)
  total_be <- be %>% dplyr::pull(cases) %>% sum(na.rm = TRUE)

  correct_countries <- c("Spain", "Germany", "Italy", "Norway", "Poland", "Switzerland", "Sweden", "Netherlands", "Ukraine", "Belgium")
  correct_ecdc <- data.frame(
    name = correct_countries,
    cases = c(total_es, total_de, total_it, total_no, total_pl, total_ch, total_se, total_nl, total_ua, total_be),
    continent = rep("Europe", length(correct_countries)),
    stringsAsFactors = FALSE
  )

  eu_agg <- eu %>% 
    dplyr::group_by(country, continent) %>% 
    dplyr::summarise(cases = sum(cases, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(name = country, cases, continent) %>% 
    dplyr::filter(!name %in% correct_countries) %>% 
    dplyr::bind_rows(correct_ecdc) %>% 
    dplyr::mutate(
      continent = dplyr::case_when(
        name == "Russia" ~ "Europe",
        name == "Turkey" ~ "Europe",
        TRUE ~ continent
      )
    )

  data <- dplyr::bind_rows(de, it, eu_agg, at, ch, sp, en, no, pl, sc, se, nl, ua, be) %>% 
    dplyr::mutate(
      name = dplyr::case_when(
        name == "Ile-de-France" ~ "Île-de-France",
        name == "Emilia Romagna" ~ "Emilia-Romagna",
        name == "Sicilia" ~ "Sicily",
        name == "Puglia" ~ "Apulia",
        name == "Friuli Venezia Giulia" ~ "Friuli-Venezia Giulia",
        name == "Hauts-de-France" ~ "Hauts-de-France",
        name == "Pays De La Loire" ~ "Pays de la Loire",
        name == "Centre-Val De Loire" ~ "Centre-Val de Loire",
        name == "Geneva" ~ "Genève",
        name == "Zurich" ~ "Zürich",
        name == "Navarra" ~ "Comunidad Foral de Navarra",
        name == "St. Gallen" ~ "Sankt Gallen",
        name == "Asturias" ~ "Principado de Asturias",
        name == "Madrid" ~ "Comunidad de Madrid",
        name == "C. Valenciana" ~ "Comunidad Valenciana",
        name == "Murcia" ~ "Región de Murcia",
        name == "Hackney and City of London" ~ "Greater London",
        name == "Cornwall and Isles of Scilly" ~ "Cornwall",
        name == "Bedford" ~ "Bedfordshire",
        name == "Baleares" ~ "Islas Baleares",
        name == "Castilla La Mancha" ~ "Castilla-La Mancha",
        name == "Bournemouth, Christchurch and Poole" ~ "Bournemouth",
        name == "County Durham" ~ "Durham",
        name == "St. Helens" ~ "Saint Helens",
        name == "Örebro" ~ "Orebro",
        TRUE ~ name
      )
    )

  callModule(mod_europe_server, "europe", eu = eu_agg, ch = ch, data = data)
  callModule(mod_world_server, "world", eu = eu)

  total_eu <- eu %>% 
    dplyr::filter(continent == "Europe" | country %in% c("United Kingdom", "Italy", "Turkey")) %>% 
    dplyr::mutate(date_time = as.Date(date_time)) %>% 
    dplyr::group_by(date_time) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::pull(cases) %>% 
    sum()

  trend_fr <- eu %>% 
    dplyr::filter(country == "France") %>% 
    dplyr::group_by(date_time) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::pull(cases)

  trend_es <- eu %>% 
    dplyr::filter(country == "Spain") %>% 
    dplyr::group_by(date_time) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::pull(cases)

  trend_de <- eu %>% 
    dplyr::filter(country == "Germany") %>% 
    dplyr::group_by(date_time) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::pull(cases)

  trend_it <- eu %>% 
    dplyr::filter(country == "Italy") %>% 
    dplyr::group_by(date_time) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::pull(cases)

  trend_ch <- eu %>% 
    dplyr::filter(country == "Switzerland") %>% 
    dplyr::group_by(date_time) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::pull(cases)

  total_uk <- eu %>% 
    dplyr::filter(country == "United Kingdom") %>% 
    dplyr::group_by(date_time) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::pull(cases)

  trend_eu <- eu %>% 
    dplyr::group_by(date_time) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::pull(cases)

  callModule(mod_count_server, "count_eu", n = trend_eu, title = "Europe", total = total_eu)
  callModule(mod_count_server, "count_spain", n = trend_es, title = "Spain", total = total_es)
  callModule(mod_count_server, "count_france", n = trend_fr, title = "France")
  callModule(mod_count_server, "count_germany", n = trend_de, title = "Germany", total = total_de)
  callModule(mod_count_server, "count_italy", n = trend_it, title = "Italy", total = total_it)
  callModule(mod_count_server, "count_switzerland", n = trend_ch, title = "Switzerland", total = total_ch)
  callModule(mod_count_server, "count_uk", n = total_uk, title = "United Kingdom")
}
