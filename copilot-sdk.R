library("jsonlite")
source("logger.R")
source("ShinyModule.R")

Sys.setenv(tz="UTC")

inputFileName = "input1_pigeons.rds" ## Provided testing datasets: "input1_pigeons.rds", "input2_geese.rds", "input3_stork.rds", "input4_goat.rds"  ## for own data: file saved as a .rds containing a object of class MoveStack
outputFileName = "output.rds" ## optionally change the output file name

if(file.exists("configuration.json")) {
  args <- read_json("configuration.json")
} else {
  args <- list()
}

#################################################################
########################### Arguments ###########################
# If the function "shinyModuleConfiguration()" has been used, state here the arguments that are listed in the function
# The data parameter will be added automatically if input data is available
# The name of the field in the vector must be exactly the same as in the shiny module signature
# Example:
# shinyModule <- function(input, output, session, username, password)
# The parameter must look like:
#    args[["username"]] = "any-username"
#    args[["password"]] = "any-password"

# Add your arguments of your r shiny function here
#args[["year"]] = 2014

#####################################################################
#####################################################################
## DO NOT MODIFY THE CODE BELOW!  
## All the code below simulates the MoveApps environment to enable ##
## testing an App locally. Hit "Run App" (top right of this panel) ##
## to run this script. The Shiny App will be executed.             ##
## DO NOT MODIFY THE CODE BELOW!   
#####################################################################
#####################################################################

storeConfiguration <- function(configuration) {
  write_json(configuration, "configuration.json", auto_unbox = TRUE)
  logger.info("Stored configuration of shinyModule to 'configuration.json'")
}

ui <- fluidPage(
  do.call(shinyModuleUserInterface, c("shinyModule", "shinyModule", args)),
  dataTableOutput("table"), #Is necessary for storing result

  if (exists("shinyModuleConfiguration")) {
    actionButton("storeConfiguration", "Store current configuration")
  },
)

readInput <- function(sourceFile) {
  input <- NULL
  if(!is.null(sourceFile) && sourceFile != "") {
    if (file.info(sourceFile)$size == 0) {
      # handle the special `null`-input
        logger.warn("The App has received invalid input! It cannot process NULL-input. Aborting..")
        stop("The App has received invalid input! It cannot process NULL-input. Check the output of the preceding App or adjust the datasource configuration.")
    }
    logger.debug("Loading file from %s", sourceFile)
    input <- tryCatch({
        # 1: try to read input as move RDS file
        readRDS(file = sourceFile)
      },
      error = function(readRdsError) {
        tryCatch({
          # 2 (fallback): try to read input as move CSV file
          move(sourceFile, removeDuplicatedTimestamps=TRUE)
        },
        error = function(readCsvError) {
          # collect errors for report and throw custom error
          stop(paste(sourceFile, " -> readRDS(sourceFile): ", readRdsError, "move(sourceFile): ", readCsvError, sep = ""))
        })
      })
  } else {
    logger.debug("Skip loading: no source File")
  }

  input
}

server <- function(input, output, session) {
  tryCatch(
  {
    inputData <- readInput(inputFileName)

    shinyModuleArgs <- c(shinyModule, "shinyModule", args)
    if (!is.null(inputData)) {
      shinyModuleArgs[["data"]] <- inputData
    }

    result <- do.call(callModule, shinyModuleArgs)

    observeEvent(input$storeConfiguration, {
      logger.info("Start reading configuration from shinyModule")
      storeConfiguration(shinyModuleConfiguration("shinyModule", input))
    })

    output$table <- renderDataTable({
      if (!is.null(outputFileName) &&
        outputFileName != "" &&
        !is.null(result())) {
        logger.info(paste("Storing file to '", outputFileName, "'", sep = ""))
        saveRDS(result(), file = outputFileName)
      } else {
        logger.warn("Skip store result: no output File or result is missing.")
      }
    })
  },
  error = function(e) {
    logger.error(paste("ERROR:", e))
    stop(e) # re-throw the exception
  })
}

shinyApp(ui, server)