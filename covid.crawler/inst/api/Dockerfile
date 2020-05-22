FROM rocker/verse

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libpq-dev \
  libssl-dev \
  libxml2-dev \
  libcurl4-gnutls-dev

RUN install2.r plumber DBI pool RPostgres

COPY plumb.R plumber.R

EXPOSE 8000
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$registerHooks(list('exit' = function(){pool::poolClose(con)})); pr$run(host='0.0.0.0', port=8000, swagger = TRUE)"]