# Rasterize Variable On Inteactive Map

MoveApps

Github repository: *github.com/movestore/rasterizeOnLeaflet*

## Description
Raster containing the number of instances of the specified variable overlaid on a interactive map. The user can choose between displaying the number of GPS locations per pixel, the number of individuals, the number of species or the number of Movebank studies (this last one yet to implement). The variable is rasterized on a grid of user-defined resolution (pixel size) and overlaid on a interactive map. The map can be interactively zoomed, and the background openstreetmap can be selected as `TopoMap`or `Aerial`. The map interactively updates when the user selects a different variable to display or raster pixel size. *Suggestion: if the chosen dataset covers a large area and at first you do not see the raster on the map, try increasing the pixel size.*

## Documentation
This App creates a Shiny UI that allows the interactive visualization of the number of GPS locations, number of individuals and species in different regions of the map. This App extracts from the MoveStack the variable `entity` selected by the user (by default the number of GPS locations) and count the number of instances falling in each raster cell. For further analyses the input data set is also returned.

### Input data
MoveStack in Movebank format.

### Output data
Shiny user interface (UI) and MoveStack in Movebank format.

### Artefacts
`DensityMap.png`: The visualization produced by the App can be saved (via "Save Plot") in *.png* format to the "Output" folder in Moveapps

### Parameters 
`entity`: variable that the user wants to be rasterized as number of occurrences per raster cell. The user can choose one of `n_locations` (default), `n_individuals`, `n_species` or `n_studies`.
`pxSize`: desired resolution (pixel size) of the grid used to rasterize the chosen variable. Unit is in degree with a possible range of *0.01* to *5* degrees (corresponding to about 1 to 500 km). Default value is *0.1*.

### Null or error handling
If one of the user parameters are not defined the App will use the default values (`entity` = `n_locations` and `pxSize` = *0.1*)
