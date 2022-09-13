FROM {{{base_image}}} AS sysreqs

ENV CRAN_REPO https://packagemanager.rstudio.com/cran/__linux__/focal/latest

ARG R_CONFIG_ACTIVE={{{app_config}}}

# System Requirements
{{{sysreqs}}}

FROM sysreqs As pkgdeps

#COPY ./deps.R /tmp/deps.R
#RUN Rscript /tmp/deps.R && rm -rf /tmp/*

RUN R -e "install.packages('remotes', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))"

{{{ cran_installs }}}

{{{ gh_installs }}}

FROM pkgdeps AS shinyapp

# Copy app into `shiny_app` directory
COPY . /srv/shiny-server/shiny-app

# Update permissions (recursively) to App directory for `shiny` user
RUN chown -R shiny:shiny /srv/shiny-server/shiny_app

# Set the R_CONFIG_ACTIVE environment variable for Shiny.  For some reason shiny-server
# can't read in regular environment variables, so we have to pass the environment variable
# as a build argument to this Docker image, and then set it as an R environment variable. We
# set it in .Rprofile rather than .Renviron, because if there is a .Renviron supplied with the
# shiny app, the .Renviron from the shiny user's home folder will not be read in.
RUN echo "Sys.setenv(R_CONFIG_ACTIVE='$R_CONFIG_ACTIVE')" >> /home/shiny/.Rprofile

USER shiny

{{{additional_r_commands}}}

EXPOSE {{{port}}}

CMD ["Rscript","-e","shiny::runApp(appDir='/srv/shiny-server/shiny_app',port={{{port}}},launch.browser=FALSE,host='0.0.0.0')"]
