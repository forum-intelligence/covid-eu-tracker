FROM rocker/r-ver:3.6.0
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libpq-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'install.packages("shinyWidgets")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "2.1.3")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'install.packages("golem")'
RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "1.5")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_github("JohnCoene/countup", upgrade="never")'
RUN Rscript -e 'remotes::install_github("JohnCoene/waiter@b6fe892902deed6796a1b8ad0748baeb7dd64410")'
RUN Rscript -e 'remotes::install_github("JohnCoene/sever@85f45c650edaa70532ebee68dbf3df94b8d7a510")'
RUN Rscript -e 'remotes::install_github("JohnCoene/reactrend@06daa5869c9cd94187db3fafb252c6f0a486b90e")'
RUN Rscript -e 'remotes::install_github("JohnCoene/echarts4r")'
RUN Rscript -e 'remotes::install_github("r-lib/later@0fb877adcfdf8166eb5b505fe99bcf2f9bedacd3")'
RUN Rscript -e 'remotes::install_github("r-lib/usethis@01dbd8f40f35d08956a919e3d2680810da32deb6")'
RUN Rscript -e 'install.packages("yaml")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');covid.tracker::run_app(embed_url='https://tracker.weforum.org/app/embeds')"]