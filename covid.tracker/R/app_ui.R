#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      waiter::waiter_show_on_load(loader, color = "#005c9c"),
      echarts4r::e_map_register_ui("counties", "www/counties.json", async = TRUE),
      echarts4r::e_map_register_ui("europe", "www/europe-turkey.json", async = FALSE),
      echarts4r::e_map_register_ui("switzerland", "www/switzerland.json", async = TRUE),
      echarts4r::e_map_register_ui("germany", "www/germany.json", async = TRUE),
      echarts4r::e_map_register_ui("italy", "www/italy.json", async = TRUE),
      echarts4r::e_map_register_ui("austria", "www/austria.json", async = TRUE),
      #echarts4r::e_map_register_ui("france", "www/france.json", async = TRUE),
      echarts4r::e_map_register_ui("spain", "www/spain.json", async = TRUE),
      echarts4r::e_map_register_ui("norway", "www/norway.json", async = TRUE),
      echarts4r::e_map_register_ui("poland", "www/poland.json", async = TRUE),
      echarts4r::e_map_register_ui("sweden", "www/sweden.json", async = TRUE),
      echarts4r::e_map_register_ui("england", "www/england.json", async = TRUE),
      echarts4r::e_map_register_ui("netherlands", "www/netherlands.json", async = TRUE),
      echarts4r::e_map_register_ui("ukraine", "www/ukraine.json", async = TRUE),
      echarts4r::e_map_register_ui("belgium", "www/belgium.json", async = TRUE),
      sever::use_sever(),
      div(
        class = "container",
        fluidRow(
          column(
            8,
            h1("COVID-19 Tracker: Europe", id = "header"),
            h5("The World Economic Forum created this tool to track the spread of COVID-19 in Europe, in many cases down to the provincial, regional, and state level.")
          ),
          column(
            4, uiOutput("last_updated")
          )
        ),
        img(src = logo_url, id = "logo"),
        br(),
        fluidRow(
          column(2, mod_count_ui("count_eu", cl = "emphasize")),
          column(2, mod_count_ui("count_italy")),
          column(2, mod_count_ui("count_spain")),
          column(2, mod_count_ui("count_germany")),
          column(2, mod_count_ui("count_france")),
          column(2, mod_count_ui("count_uk"))
        ),
        hr(),
        mod_europe_ui("europe"),
        hr(),
        mod_world_ui("world"),
        div(
          class = "panel panel-default",
          div(
            class = "panel-body",
            h3("About this project"),
            p(
              "While many existing visualizations track the virus’ spread to European countries and provide country-level statistics, the focus of this project is to provide both country-level information for all European states in addition to provincial and regional figures where available. Currently, we cover Austria, Belgium, France, Germany, Italy, Norway, Poland, Spain, Sweden, Switzerland, Ukraine, and the United Kingdom (England and Scotland) at this more granular level."
            ),
            p(
              "For country-level data, we use daily updates on confirmed cases from the European Centre of Disease Prevention and Control (ECDC). Provincial-level data are collected from various governmental websites. Because we rely on these disparate data sources, there may be observable differences between the country-level data and the aggregated, more granular statistics. We make no interpolation or extrapolation of the data, and all data sources are referenced at the bottom of the tracker."
            ),
            p(
              "For country level statistics, we aggregate sub-country level figures where available. Otherwise, we use the ECDC source. On the timeline visualisation, we however use only ECDC data to allow for a fairer comparison between countries."
            ),
            p(
              "A programmatic pipeline has been built to extract and process the data in a fully automated way, such as to ensure scalability as we add new data sources, full reproducibility, and minimize the risk for manual errors. This also allows us to provide the data back to the community in a structured format, accompanied with the source code shared through an open source GitHub repository (coming out soon)."
            ),
            HTML(
              "<p>Feel free to contact us at <a href='mailto:tracker@weforum.org'>tracker@weforum.org</a>.</p>"
            )
          )
        ),
        div(
          class = "panel panel-default",
          div(
            class = "panel-body",
            h3("Data Sources"),
            p("Below are is the list of data sources used for this project."),
            div(
              class = "list-group",
              tags$a(
                href = "https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Austria"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for provinces in Austria are taken from the Ministry of Social Affairs of Austria."
                )
              ),
              tags$a(
                href = "https://epistat.wiv-isp.be/covid/",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Belgium"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for the provinces of Belgium are taken from Sciensano which monitors the situation at the request of Belgian health authorities."
                )
              ),
              tags$a(
                href = "https://coronavirus.data.gov.uk/#local-authorities",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "England"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for the counties of England are taken from the United Kingdom Ministry of Public Health."
                )
              ),
              tags$a(
                href = "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Germany"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for provinces in Germany are taken from the Federal Robert Koch Institue."
                )
              ),
              tags$a(
                href = "https://github.com/pcm-dpc/COVID-19",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Italy"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for provinces in Italy are taken from the Department of Civil Protection."
                )
              ),
              tags$a(
                href = "https://www.rivm.nl/coronavirus-covid-19/grafieken",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Netherlands"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for the regions of the Netherlands are taken from the Ministry of Health."
                )
              ),
              tags$a(
                href = "https://www.fhi.no/sv/smittsomme-sykdommer/corona/",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Norway"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for provinces of Norway are taken from the Norwegian Institute of Public Health."
                )
              ),
              tags$a(
                href = "https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Poland"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for the provinces of Poland are taken from the official website of the Republic of Poland."
                )
              ),
              tags$a(
                href = "https://www.gov.scot/coronavirus-covid-19/",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Scotland"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for the NHS Health Boards of Scotland are taken from the Department of Health of Scotland."
                )
              ),
              tags$a(
                href = "https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Spain"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for the regions of Spain are taken from the Ministry of Health of Spain."
                )
              ),
              tags$a(
                href = "https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/aktuellt-epidemiologiskt-lage/",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Sweden"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for provinces of Sweden are taken from the Public Health Agency of Sweden."
                )
              ),
              tags$a(
                href = "https://interactif.tdg.ch/2020/covid-19-carte-suisse/",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Switzerland"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for the cantons of Switzerland are taken from the Federal Office of Public Health via Tribune de Genève."
                )
              ),
              tags$a(
                href = "https://moz.gov.ua/article/news/operativna-informacija-pro-poshirennja-koronavirusnoi-infekcii-2019-ncov-",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Ukraine"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases for the provinces of Ukraine are taken from the Ministry of Healthcare of Ukraine."
                )
              ),
              tags$a(
                href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                target = "_blank", class = "list-group-item",
                h4(class = "list-group-item-heading", "Other European Countries"),
                p(
                  class = "list-group-item-text",
                  "Confirmed cases at the country level for all European countries are taken from the European Center for Disease Prevention and Control (ECDC)."
                )
              )
            )
          )
        )
      ),
      tags$script(src="www/collapsible.js"),
      waiter::waiter_hide_on_render("count_eu-count"),
      br(),
      tags$footer(
        class = "footer align-middle",
        div(
          class = "container",
          fluidRow(
            column(6, p("© 2020 World Economic Forum Strategic Intelligence")),
            column(6, tags$a("World Economic Forum Strategic Intelligence", href = "https://intelligence.weforum.org/", class = "pull-right", target = "_blank"))
          ) 
        ),
        br()
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'COVID-19 - Strategic Intelligence"'
    ),
    HTML(
    "<!-- Global site tag (gtag.js) - Google Analytics -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=UA-15704185-67'></script>
    <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());

      gtag('config', 'UA-15704185-67');
    </script>
"
    ),
    tags$meta(property="og:title", content="COVID-19 - Strategic Intelligence"),
    tags$meta(property="og:description", content="A focus on the COVID-19 spread throughout Europe."),
    tags$meta(property="og:image", content="https://tracker.weforum.org/www/covid.jpg"),
    tags$meta(name="twitter:card", content="summary_large_image"),
    tags$meta(property="og:site_name", content="COVID-19 - Strategic Intelligence"),
    tags$meta(name="twitter:image:alt", content="COVID-19"),
    tags$meta(name="twitter:site", content="@wef"),
    tags$meta(name="twitter:image:src", content="https://tracker.weforum.org/www/covid.jpg"),
    waiter::use_waiter(spinners = 3, include_js = FALSE)
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

