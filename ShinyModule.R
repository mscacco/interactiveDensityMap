library("shiny")

## to display messages to the user in the log file of the App in MoveApps one can use the function from the logger.R file: logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace() ##

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()

  tagList(
    titlePanel("Add your user interface")
  )
}

shinyModuleConfiguration <- function(id, input) { ## inclusion of this function is optional. To be used if one would like the user to set values of parameters before executing the shiny app. See user manual.
  ns <- NS(id)
  configuration <- list()
  configuration
}

shinyModule <- function(input, output, session, data) { ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()
  current <- reactiveVal(data)

  return(reactive({ current() }))
}
