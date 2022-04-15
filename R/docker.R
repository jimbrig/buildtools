#' Find the Docker Executable
#'
#' @return character string that is location of the docker executable
#' @export
find_docker_cmd <- function() {
  docker_cmd <- Sys.which('docker')
  if (length(docker_cmd) == 0) stop(paste('\n','Docker command not found. ','\n',
                                          'Please install docker: ','\n',
                                          'https://www.docker.com/products/overview#/install_the_platform'))
  docker_cmd
}

# start_docker_service <- function() {
#   if (check_docker_running()) stop("Docker service is already running!")
# }

#' Check that the Docker Daemon/Service is Running
#'
#' @return Logical TRUE/FALSE
#' @export
check_docker_running <- function() {
  check_cmd <- system2("powershell", "-noprofile (get-service -name 'com.docker.service').Status", stdout = TRUE)
  return(check_cmd == "Running")
}

#' Build Docker Shiny Application
#'
#' @param app_name Name of application to make (defaults to working directory)
#'
#' @export
build_docker_app <- function(app.name=basename(getwd())) {
  docker_cmd <- find_docker_cmd()
  system2(docker_cmd,c('build','-t',app.name,'.'))
}

