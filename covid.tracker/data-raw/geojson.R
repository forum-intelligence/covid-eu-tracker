library(purrr)

# EU raster
eu_json <- jsonlite::read_json("./inst/app/www/europe.json")
eu_json$features[[25]] <- NULL # remove Italy
eu_json$features[[11]] <- NULL # remove Germany
eu_json$features[[2]] <- NULL # remove Switzerland
eu_json$features[[3]] <- NULL # remove Austria
#eu_json$features[[13]] <- NULL # remove France
eu_json$features[[11]] <- NULL # remove Spain
eu_json$features[[14]] <- NULL # remove UK
eu_json$features[[35]] <- NULL # remove Norway
eu_json$features[[33]] <- NULL # remove Poland
eu_json$features[[40]] <- NULL # remove Sweden
eu_json$features[[41]] <- NULL # remove Ukraine
eu_json$features[[6]] <- NULL # remove Belgium
eu_json$features[[31]] <- NULL # remove Netherlands

ni <- function(nm, data){
  data <- purrr::map(data$features, "properties") %>% 
    purrr::map("name") %>% 
    unlist()
  print(grep(nm, data))
  data[grepl(nm, data)]
}

# get and simplify geosjon
get_geojson <- function(country, level = 1, keep = .01){
  sp <- raster::getData('GADM', country = toupper(country), level = level)
  sp <- rmapshaper::ms_simplify(sp, keep = keep) 
  geojsonio::geojson_list(sp)
}

germany_json <- get_geojson("germany", level = 1)
germany_json$features <- germany_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

italy_json <- get_geojson("italy", level = 1)
italy_json$features <- italy_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

italy_counties_json <- get_geojson("italy", level = 2)
italy_counties_json$features <- italy_counties_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_2
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

switzerland_json <- get_geojson("switzerland", level = 1)
switzerland_json$features <- switzerland_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

austria_json <- get_geojson("austria", level = 1)
austria_json$features <- austria_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

france_json <- get_geojson("france", level = 1)
france_json$features <- france_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

spain_json <- get_geojson("spain", level = 1)
spain_json$features <- spain_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })
spain_json$features[[6]] <- NULL

england_json <- get_geojson("united kingdom", level = 2)
england_json$features <- england_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_2
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })
england_json$features[[143]]$properties$name <- "Lanarkshire"
england_json$features[[150]]$properties$name <- "Lanarkshire"
england_json$features[[142]]$properties$name <- "Ayrshire and Arran"
england_json$features[[149]]$properties$name <- "Ayrshire and Arran"
england_json$features[[159]]$properties$name <- "Ayrshire and Arran"
england_json$features[[156]]$properties$name <- "Forth Valley"
england_json$features[[134]]$properties$name <- "Forth Valley"
england_json$features[[152]]$properties$name <- "Forth Valley"
england_json$features[[129]]$properties$name <- "Grampian"
england_json$features[[140]]$properties$name <- "Grampian"
england_json$features[[141]]$properties$name <- "Grampian"
england_json$features[[147]]$properties$name <- "Borders"
england_json$features[[136]]$properties$name <- "Greater Glasgow and Clyde"
england_json$features[[153]]$properties$name <- "Greater Glasgow and Clyde"
england_json$features[[160]]$properties$name <- "Greater Glasgow and Clyde"
england_json$features[[131]]$properties$name <- "Greater Glasgow and Clyde"
england_json$features[[146]]$properties$name <- "Greater Glasgow and Clyde"
england_json$features[[138]]$properties$name <- "Greater Glasgow and Clyde"
england_json$features[[155]]$properties$name <- "Highland"

england_json$features[[132]]$properties$name <- "Lothian"
england_json$features[[130]]$properties$name <- "Lothian"
england_json$features[[154]]$properties$name <- "Lothian"
england_json$features[[139]]$properties$name <- "Lothian"

england_json$features[[144]]$properties$name <- "Orkney"
england_json$features[[148]]$properties$name <- "Shetland"
england_json$features[[151]]$properties$name <- "Angus"
england_json$features[[158]]$properties$name <- "Angus"
england_json$features[[145]]$properties$name <- "Angus"

# remove irish 
england_json$features[[125]] <- NULL
england_json$features[[124]] <- NULL
england_json$features[[123]] <- NULL
england_json$features[[125]] <- NULL
england_json$features[[121]] <- NULL
england_json$features[[119]] <- NULL
england_json$features[[121]] <- NULL
england_json$features[[119]] <- NULL
england_json$features[[119]] <- NULL
england_json$features[[118]] <- NULL
england_json$features[[118]] <- NULL

# remove welsh
england_json$features[[150]] <- NULL
england_json$features[[151]] <- NULL
england_json$features[[168]] <- NULL
england_json$features[[150]] <- NULL
england_json$features[[167]] <- NULL
england_json$features[[161]] <- NULL
england_json$features[[155]] <- NULL
england_json$features[[151]] <- NULL
england_json$features[[152]] <- NULL
england_json$features[[156]] <- NULL
england_json$features[[159]] <- NULL
england_json$features[[158]] <- NULL
england_json$features[[155]] <- NULL
england_json$features[[150]] <- NULL
england_json$features[[152]] <- NULL
england_json$features[[153]] <- NULL
england_json$features[[153]] <- NULL
england_json$features[[150]] <- NULL
england_json$features[[151]] <- NULL
england_json$features[[151]] <- NULL
england_json$features[[150]] <- NULL
england_json$features[[150]] <- NULL

uk_json <- get_geojson("united kingdom", level = 1)
uk_json$features <- uk_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

# add ireland and wales to england json
england_json$features <- append(england_json$features, list(uk_json$features[[2]]))
england_json$features <- append(england_json$features, list(uk_json$features[[4]]))

uk_json$features[[1]] <- NULL # remove england
uk_json$features[[2]] <- NULL # remove scotland

norway_json <- get_geojson("norway", level = 1)
norway_json$features <- norway_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })
norway_json$features[[10]]$properties$name <- "Agder"
norway_json$features[[13]]$properties$name <- "Agder"
norway_json$features[[19]]$properties$name <- "Trøndelag"
norway_json$features[[7]]$properties$name <- "Trøndelag"
norway_json$features[[8]]$properties$name <- "Vestfold og Telemark"
norway_json$features[[11]]$properties$name <- "Vestfold og Telemark"
norway_json$features[[14]]$properties$name <- "Viken"
norway_json$features[[1]]$properties$name <- "Viken"
norway_json$features[[12]]$properties$name <- "Viken"
norway_json$features[[17]]$properties$name <- "Vestland"
norway_json$features[[6]]$properties$name <- "Vestland"
norway_json$features[[9]]$properties$name <- "Troms og Finnmark"
norway_json$features[[15]]$properties$name <- "Troms og Finnmark"
norway_json$features[[16]]$properties$name <- "Innlandet"
norway_json$features[[3]]$properties$name <- "Innlandet"

# poland
poland_json <- get_geojson("poland", level = 1)
poland_json$features <- poland_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

# turkey
turkey_json <- get_geojson("turkey", level = 0)
turkey_json$features <- turkey_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_0
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

# sweden
sweden_json <- get_geojson("sweden", level = 1)
sweden_json$features <- sweden_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

# netherlands
netherlands_json <- get_geojson("netherlands", level = 1)
netherlands_json$features <- netherlands_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

# netherlands
ukraine_json <- get_geojson("ukraine", level = 1)
ukraine_json$features <- ukraine_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

# Belgium
download.file(
  "https://raw.githubusercontent.com/thomaspepio/belgium-geojson/master/expected.geojson", 
  destfile = "./inst/app/www/belgium.json"
)

belgium_sp <- geojsonio::geojson_read("./inst/app/www/belgium.json", what = "sp")
sp <- rmapshaper::ms_simplify(belgium_sp, keep = .01)
belgium_json <- geojsonio::geojson_list(sp)

belgium_json$features <- belgium_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$name
    if(nm == "Limburg") 
      nm <- "Limburg "

    if(nm == "Luxembourg") 
      nm <- "Luxembourg "
    nm <- tools::toTitleCase(nm)
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

eu_json$features <- append(eu_json$features, germany_json$features)
eu_json$features <- append(eu_json$features, italy_json$features)
eu_json$features <- append(eu_json$features, austria_json$features)
eu_json$features <- append(eu_json$features, switzerland_json$features)
#eu_json$features <- append(eu_json$features, france_json$features)
eu_json$features <- append(eu_json$features, spain_json$features)
eu_json$features <- append(eu_json$features, england_json$features)
eu_json$features <- append(eu_json$features, uk_json$features)
eu_json$features <- append(eu_json$features, norway_json$features)
eu_json$features <- append(eu_json$features, poland_json$features)
eu_json$features <- append(eu_json$features, turkey_json$features)
eu_json$features <- append(eu_json$features, sweden_json$features)
eu_json$features <- append(eu_json$features, netherlands_json$features)
eu_json$features <- append(eu_json$features, ukraine_json$features)
eu_json$features <- append(eu_json$features, belgium_json$features)

eu_turkey_json <- jsonlite::read_json("./inst/app/www/europe.json")
eu_turkey_json$features <- purrr::map(eu_turkey_json$features, function(feat){
  nm <- feat$properties$name
  feat$properties <- NULL
  feat$properties <- list(name = nm)
  return(feat)
})
eu_turkey_json$features <- append(eu_turkey_json$features, turkey_json$features)
jsonlite::write_json(eu_turkey_json, path = "./inst/app/www/europe-turkey.json", auto_unbox = TRUE)

# remove useless properties
eu_json$features <- purrr::map(eu_json$features, function(feat){
  nm <- feat$properties$name
  feat$properties <- NULL
  feat$properties <- list(name = nm)
  return(feat)
})

# test
echarts4r::e_charts() %>%
  echarts4r::e_map_register("eu", belgium_json) %>%
  echarts4r::e_map(map = "eu", roam = TRUE)

jsonlite::write_json(eu_json, path = "./inst/app/www/counties.json", auto_unbox = TRUE)

# replace switzerland and austria with more detailed maps
switzerland_json <- get_geojson("switzerland", level = 1, keep = 0.1)
switzerland_json$features <- switzerland_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

austria_json <- get_geojson("austria", level = 1, keep = 0.1)
austria_json$features <- austria_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

netherlands_json <- get_geojson("netherlands", level = 1, keep = .1)
netherlands_json$features <- netherlands_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$NAME_1 
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

belgium_sp <- geojsonio::geojson_read("./inst/app/www/belgium.json", what = "sp")
sp <- rmapshaper::ms_simplify(belgium_sp, keep = .1)
belgium_json <- geojsonio::geojson_list(sp)

belgium_json$features <- belgium_json$features %>% 
  purrr::map(function(x){ 
    nm <- x$properties$name
    if(nm == "Limburg") 
      nm <- "Limburg "

    if(nm == "Luxembourg") 
      nm <- "Luxembourg "
    nm <- tools::toTitleCase(nm)
    x$properties <- NULL
    x$properties <- list(name = nm)
    return(x)
  })

geojsonio::geojson_write(switzerland_json, file = "./inst/app/www/switzerland.json", geometry = "polygon")
geojsonio::geojson_write(italy_json, file = "./inst/app/www/italy.json", geometry = "polygon")
geojsonio::geojson_write(austria_json, file = "./inst/app/www/austria.json", geometry = "polygon")
geojsonio::geojson_write(germany_json, file = "./inst/app/www/germany.json", geometry = "polygon")
geojsonio::geojson_write(france_json, file = "./inst/app/www/france.json", geometry = "polygon")
geojsonio::geojson_write(spain_json, file = "./inst/app/www/spain.json", geometry = "polygon")
geojsonio::geojson_write(england_json, file = "./inst/app/www/england.json", geometry = "polygon")
geojsonio::geojson_write(italy_counties_json, file = "./inst/app/www/italy-counties.json", geometry = "polygon")
geojsonio::geojson_write(norway_json, file = "./inst/app/www/norway.json", geometry = "polygon")
geojsonio::geojson_write(poland_json, file = "./inst/app/www/poland.json", geometry = "polygon")
geojsonio::geojson_write(sweden_json, file = "./inst/app/www/sweden.json", geometry = "polygon")
geojsonio::geojson_write(netherlands_json, file = "./inst/app/www/netherlands.json", geometry = "polygon")
geojsonio::geojson_write(ukraine_json, file = "./inst/app/www/ukraine.json", geometry = "polygon")
geojsonio::geojson_write(belgium_json, file = "./inst/app/www/belgium.json", geometry = "polygon")
