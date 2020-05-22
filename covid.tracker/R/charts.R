format_tp <- htmlwidgets::JS(
  "function(params){
    return params.name + '<br/>' + Math.round(Math.pow(10, params.value) - 1) + ' confirmed cases'
  }"
)

format_vm <- htmlwidgets::JS(
  "function(value){
    return Math.round(Math.pow(10, value) - 1)
  }"
)

eu_countries_map <- function(eu, log = TRUE, embed = FALSE){
  eu$cases_log <- log10(eu$cases + 1)

  e <- eu %>% 
    dplyr::filter(continent == "Europe" | name == "Kosovo" | name == "Russia" | name == "Turkey" | name == "Greece" | name == "United Kingdom") %>% 
    echarts4r::e_charts(name) %>% 
    echarts4r::e_map(
      cases_log, map = "europe", name = "Confirmed cases",
      boundingCoords = list(
        c(-20.181351, 67.864860),
        c(25.839844, 37.857507)
      ),
      roam = FALSE,
      itemStyle = list(
        borderColor = "#eee",
        emphasis = list(
          areaColor = "#eee"
        )
      ),
      label = list(
        emphasis = list(
          color = "#005c9c"
        )
      ),
      zoom = 1
    ) %>% 
    echarts4r::e_visual_map(
      cases_log,
      top = "middle",
      text = list(
        "Max confirmed cases",
        "Min confirmed cases"
      ),
      inRange = list(
        color = sequential_palette
      ),
      formatter = format_vm
    ) %>%
    echarts4r::e_tooltip(formatter = format_tp) %>% 
    echarts4r::e_show_loading(color = "#a63603", text = "")
  
  if(embed){
    e <- add_logo(e)
  }

  return(e)
}

eu_counties_map <- function(data, log = FALSE, embed = FALSE){
  data$cases <- log10(data$cases + 1)

  e <- data %>% 
    dplyr::filter(!continent %in% c("Asia", "Africa", "Americas", "Oceania")) %>% 
    dplyr::filter(!name %in% c("United Kingdom", "Spain", "Italy", "Netherlands", "Germany", "Austria", "Switzerland")) %>% 
    echarts4r::e_charts(name) %>%
    echarts4r::e_map(
      cases, map = "counties", name = "Confirmed cases",
      boundingCoords = list(
        c(-20.181351, 67.864860),
        c(25.839844, 37.857507)
      ),
      roam = FALSE,
      itemStyle = list(
        borderColor = "#eee",
        emphasis = list(
          areaColor = "#eee"
        )
      ),
      label = list(
        emphasis = list(
          color = "#005c9c"
        )
      ),
      zoom = 1
    ) %>% 
    echarts4r::e_visual_map(
      cases,
      top = "middle",
      text = list(
        "Max confirmed cases",
        "Min confirmed cases"
      ),
      inRange = list(
        color = sequential_palette
      ),
      formatter = format_vm
    ) %>%
    echarts4r::e_tooltip(formatter = format_tp) %>% 
    echarts4r::e_show_loading(color = "#a63603")

  if(embed){
    e <- add_logo(e)
  }

  return(e)
}

world_map <- function(data, log = FALSE, embed = FALSE){
  titles <- unique(data$date) %>% 
    purrr::map(function(x){
      list(text = format(x, "%d %B"))
    })

  if(log) data$cases <- log10(data$cases + 1)
  
  mn <- min(data$cases, na.rm = TRUE)
  mx <- max(data$cases, na.rm = TRUE)

  e <- data %>% 
    dplyr::mutate(
      country = dplyr::case_when(
        country == "United States of America" ~ "United States",
        country == "Czechia" ~ "Czech Rep.",
        TRUE ~ country
      )
    ) %>% 
    dplyr::group_by(date) %>% 
    echarts4r::e_charts(country, timeline = TRUE) %>% 
    echarts4r::e_map(cases, name = "Confirmed cases") %>% 
    echarts4r::e_visual_map(
      min = mn,
      max = mx,
      top = "middle",
      text = list(
        "Max confirmed cases",
        "Min confirmed cases"
      ),
      inRange = list(
        color = sequential_palette
      )
    ) %>%
    echarts4r::e_timeline_serie(
      title = titles
    ) %>% 
    echarts4r::e_timeline_opts(
      currentIndex = length(titles) - 1,
      playInterval = 600, 
      symbolSize = 4, 
      axis_type = "time",
      label = list(
        show = FALSE,
        interval = 5
      ),
      checkpointStyle = list(
        symbol = "pin",
        symbolSize = 20
      ),
      itemStyle = list(
        color = "#005c9c",
        emphasis = list(
          color = "#005c9c"
        )
      )
    ) %>% 
    echarts4r::e_show_loading(color = "#a63603") %>% 
    echarts4r::e_tooltip()

  if(embed){
    e <- add_logo(e)
  }

  return(e)
}

country_trend <- function(data, pattern = NULL, log = FALSE, cumul = FALSE, embed = FALSE, per_capita = FALSE, date = TRUE, var = "cases"){
  ls <- list(
    shadowColor = "rgba(0, 0, 0, 0.8)",
    shadowBlur = 5,
    shadowOffsetY = 3
  )

  axis_title <- paste("Confirmed", var)
  x_axis_title <- "Date"
  if(!date)
    x_axis_title <- "Days since 100th case"

  # replace cases
  data$cases <- data[[var]]

  if(per_capita){
    axis_title <- paste(axis_title, "per 1 million inhabitant")
    data$cases <- data$cases / (data$population / 10^6)
    data$cases <- round(data$cases, 3)
  }

  if(log)
    axis_title <- paste("log10(", axis_title, ")")

  dat <- data %>% 
    dplyr::mutate(date = as.Date(date_time)) %>% 
    dplyr::filter(country %in% pattern) %>% 
    dplyr::group_by(date, country) %>% 
    dplyr::summarise(cases = sum(cases, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(
      cases_cumul = cumsum(cases)
    ) %>% 
    dplyr::inner_join(first_case, by = "country") %>% 
    dplyr::mutate(
      since_first_case = date - first_case
    )
    
  if(log){
    dat$cases <- log10(dat$cases + 1)
    dat$cases_cumul <- log10(dat$cases_cumul + 1)
  }

  cases_to_plot <- "cases"
  if(cumul)
    cases_to_plot <- "cases_cumul"

  log_fm <- htmlwidgets::JS("function(params){
    var str = params[0].value[0];
    for(var cn = 0; cn < params.length; cn++){
      str = str + '<br/>' + '<i class = \"fa fa-circle\" style = \"color:' + params[cn].color + '\"></i> ' + params[cn].seriesName + ': ' + Math.round(Math.pow(10, params[cn].value[1]) - 1)
    }

    return str;
  }")

  if(!date){
    dat <- dplyr::filter(dat, since_first_case >= 0)
    dat$date <- as.integer(dat$since_first_case)
  }

  e <- dat %>% 
    echarts4r::e_charts(date, dispose = TRUE) %>% 
    echarts4r::e_line_(cases_to_plot, lineStyle = ls, symbol = "circle") %>% 
    echarts4r::e_hide_grid_lines("x") %>% 
    echarts4r::e_legend(TRUE) %>% 
    echarts4r::e_theme("weforum") %>% 
    echarts4r::e_axis_labels(x = x_axis_title, y = axis_title) %>% 
    echarts4r::e_show_loading(color = "#a63603")

  if(log){
    e <- echarts4r::e_tooltip(e, trigger = "axis", axisPointer = list(type = "shadow"), formatter = log_fm)
  } else {
    e <- echarts4r::e_tooltip(e, trigger = "axis", axisPointer = list(type = "shadow"))
  }

  if(embed){
    e <- add_logo(e, left = "90%")
  }

  echarts4r::e_datazoom(e, toolbox = FALSE)
}

#' Round up
#' 
#' Nicely round up numbers for pieces in maps.
#' 
#' @param x Number.
#' @param nice Vector of ending.
#' 
#' @keywords internal
round_up <- function(x, nice=1:10) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

diverging_palette <- rev(
  c(
  "#d73027",
  "#fc8d59",
  "#fee08b",
  "#d9ef8b",
  "#91cf60",
  "#1a9850"
  )
)

sequential_palette <- c(
  "#feedde",
  "#fdd0a2",
  "#fdae6b",
  "#fd8d3c",
  "#e6550d",
  "#a63603"
)

compute_pieces <- function(data, col, n = 7){
  x <- data %>% 
    dplyr::select(cases = {{ col }}) %>% 
    dplyr::filter(!is.na(cases)) %>% 
    dplyr::distinct(cases) %>% 
    dplyr::arrange(cases) %>% 
    dplyr::pull(cases)

  pieces <- split(x, sort(x%%n)) %>% 
    unname() %>% 
    purrr::map(range) %>% 
    purrr::map(function(x){
      list(
        gt = round_up(x[1]),
        lte = round_up(x[2])
      )
    })

  for(i in 1:(length(pieces) - 1)){
    pieces[[i]]$lte <- pieces[[i + 1]]$gt
  }

  return(pieces)
}

echarts_detail_map <- function(data, selected_country = "Switzerland", embed = FALSE){
  data$cases_log <- log10(data$cases + 1)
  map <- tolower(selected_country)

  # correct for uk
  if(map == "united kingdom"){
    map <- "england"
    selected_country <- c("England", "Scotland")
  }

  e <- data %>% 
    dplyr::filter(country %in% selected_country) %>% 
    echarts4r::e_charts(name) %>% 
    echarts4r::e_map(
      cases_log, 
      map = map, 
      name = "Confirmed cases",
      roam = FALSE,
      itemStyle = list(
        borderColor = "#eee",
        emphasis = list(
          areaColor = "#eee"
        )
      ),
      label = list(
        emphasis = list(
          color = "#005c9c"
        )
      ),
      zoom = 1
    ) %>% 
    echarts4r::e_visual_map(
      cases_log,
      top = "top",
      left = "center",
      text = list(
        "Max confirmed cases",
        "Min confirmed cases"
      ),
      inRange = list(
        color = sequential_palette
      ),
      formatter = format_vm,
      orient = "horizontal"
    ) %>%
    echarts4r::e_tooltip(formatter = format_tp) %>% 
    echarts4r::e_show_loading(color = "#a63603", text = "")

  if(embed){
    e <- add_logo(e)
  }

  return(e)
}

echarts_counties_bar <- function(data, selected_country = "Switzerland", embed = FALSE){
  # correct for uk
  if(selected_country == "United Kingdom"){
    selected_country <- c("England", "Scotland")
  }

  e <- data %>% 
    dplyr::filter(country %in% selected_country) %>% 
    dplyr::filter(!is.na(name)) %>% 
    dplyr::arrange(-cases) %>% 
    echarts4r::e_charts(name) %>% 
    echarts4r::e_bar(cases, legend = FALSE, color="#e6550d") %>% 
    echarts4r::e_tooltip(
      trigger = "axis",
      axisPointer = list(
        type = "shadow"
      )
    ) %>% 
    echarts4r::e_axis_labels(y = "Confirmed Cases") %>% 
    echarts4r::e_theme("weforum")

  if(embed){
    e <- add_logo(e, left = "90%")
  }

  return(e)
}