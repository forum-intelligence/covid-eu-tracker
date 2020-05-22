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