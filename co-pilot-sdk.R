## Provided testing datasets in `./data/raw`: 
## "input1_pigeons.rds", "input2_geese.rds", "input3_stork.rds", "input4_goat.rds"  
## for own data: file saved as a .rds containing a object of class MoveStack
inputFileName = "./data/raw/input1_greylgeese.rds" 
## optionally change the output file name
dir.create("./data/output/")
outputFileName = "./data/output/output.rds" 

# this file is the home of your app code and will be bundled into the final app on MoveApps
source("ShinyModule.R")

# setup your environment
Sys.setenv(
    SOURCE_FILE = inputFileName, 
    OUTPUT_FILE = outputFileName, 
    ERROR_FILE="./data/output/error.log", 
    APP_ARTIFACTS_DIR ="./data/output/artifacts"
)

# simulate running your app on MoveApps
source("src/moveapps.R")
shinyApp(ui, server, enableBookmarking="server")
