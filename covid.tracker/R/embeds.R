#' run Embeds
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_embeds <- function(){
  con <- connect()

  add_resource_path(
    'www', app_sys('app/www')
  )

  ui <- fluidPage(
    title = "COVID-19 - Strategic Intelligence",
    tags$head(
      favicon(),
      bundle_resources(
        path = app_sys('app/www'),
        app_title = 'COVID-19 - Strategic Intelligence'
      ),
      tags$meta(name="twitter:image", content="https://tracker.weforum.org/www/covid.jpg"),
      HTML(
        "<!-- Global site tag (gtag.js) - Google Analytics -->
        <script async src='https://www.googletagmanager.com/gtag/js?id=UA-15704185-67'></script>
        <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());

          gtag('config', 'UA-15704185-67');
        </script>"
      )
    ),
    echarts4r::e_map_register_ui("counties", "www/counties.json"),
    echarts4r::e_map_register_ui("europe", "www/europe.json"),
    echarts4r::e_map_register_ui("switzerland", "www/switzerland.json"),
    echarts4r::e_map_register_ui("germany", "www/germany.json"),
    echarts4r::e_map_register_ui("italy", "www/italy.json"),
    echarts4r::e_map_register_ui("austria", "www/austria.json"),
    #echarts4r::e_map_register_ui("france", "www/france.json"),
    echarts4r::e_map_register_ui("spain", "www/spain.json"),
    echarts4r::e_map_register_ui("norway", "www/norway.json"),
    echarts4r::e_map_register_ui("poland", "www/poland.json"),
    echarts4r::e_map_register_ui("sweden", "www/sweden.json"),
    echarts4r::e_map_register_ui("england", "www/england.json"),
    echarts4r::e_map_register_ui("netherlands", "www/netherlands.json", async = TRUE),
    echarts4r::e_map_register_ui("ukraine", "www/ukraine.json", async = TRUE),
    echarts4r::e_map_register_ui("belgium", "www/belgium.json", async = TRUE),
    waiter::use_waiter(include_js = FALSE),
    waiter::waiter_show_on_load(loader, color = "#005c9c"),
    echarts4r::echarts4rOutput("chart", height = "100vh"),
    waiter::waiter_hide_on_render("chart")
  )

  server <- function(input, output, session){

    echarts4r::e_common(
      font_family = "Raleway",
      theme = "weforum"
    )

    rv <- reactiveValues(
      chart = "",
      map = "",
      scale = "",
      regions = "",
      metric = "",
      country = "",
      date = "",
      capita = ""
    )

    observe({
      query <- parseQueryString(session$clientData$url_search)
      rv$chart <- get_query(query, "chart")
      rv$map <- get_query(query, "map")
      rv$scale <- get_query(query, "scale")
      rv$regions <- get_query(query, "regions")
      rv$metric <- get_query(query, "metric")
      rv$country <- get_query(query, "country")
      rv$date <- get_query(query, "date")
      rv$capita <- get_query(query, "capita")
    })

 # get data
  eu <- DBI::dbReadTable(con, "europe")
  it <- DBI::dbReadTable(con, "italy")
  de <- DBI::dbReadTable(con, "germany")
  at <- DBI::dbReadTable(con, "austria")
  ch <- DBI::dbReadTable(con, "switzerland")
  fr <- DBI::dbReadTable(con, "france")
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
  fr <- dplyr::select(fr, name = province, cases) %>% dplyr::mutate(country = "France")
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

    output$chart <- echarts4r::renderEcharts4r({

      if(rv$chart == "")
        return(NULL)

      is_log <- ifelse(rv$scale == "linear", FALSE, TRUE)

      if(rv$chart == "eu"){
        
        if(rv$map == "country"){
          e <- eu_countries_map(data, embed = TRUE)
        }

        if(rv$map == "province"){
          e <- eu_counties_map(data, embed = TRUE)
        }
      } else if(rv$chart == "world") {
        data <- DBI::dbReadTable(con, "world") %>% 
          dplyr::arrange(date)
        e <- world_map(data, is_log, embed = TRUE)
      } else if(rv$chart == "country"){

        eu <- DBI::dbReadTable(con, "europe")

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
        
        cumul <- ifelse(grepl("cumulative", rv$metric), TRUE, FALSE)

        countries <- strsplit(rv$regions, "\\|")[[1]]
        countries <- tools::toTitleCase(countries)

        europe <- eu %>% 
          dplyr::filter(continent == "Europe" | country %in% c("United Kingdom", "Italy", "Turkey")) %>% 
          dplyr::mutate(country = "Europe") %>% 
          dplyr::group_by(country, date_time) %>% 
          dplyr::summarise(
            cases = sum(cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)
          ) %>% 
          dplyr::ungroup()
        
        eu <- dplyr::bind_rows(eu, europe)

        v <- gsub("cumulative-|new-", "", rv$metric)
        v <- trimws(v)

        e <- country_trend(eu, countries, is_log, cumul, embed = TRUE, per_capita = rv$capita, date = rv$date, var = v)
      } else if(rv$chart == "country-chart" || rv$chart == "country-map"){
        
        if(rv$chart == "country-map")
          e <- echarts_detail_map(data, tools::toTitleCase(rv$country), embed = TRUE)
        else if(rv$chart == "country-chart")
          e <- echarts_counties_bar(data, tools::toTitleCase(rv$country), embed = TRUE)
      }

      return(e)

    })

  }

  shinyApp(
    ui, 
    server,
    onStart = function(){
      onStop(function(){
        disconnect(con)
      })
    }
  )
}

#' Get Query
#' 
#' @param query URL query.
#' @param param Parameter to extract.
#' 
#' @keywords internal
get_query <- function(query, param){
  p <- query[[param]]

  if(is.null(p))
    p <- ""

  if(p == "true" || p == "false"){
    p <- toupper(p)
    p <- as.logical(p)
  }

  return(p)
}
