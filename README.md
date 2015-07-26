# OEFinder

OEFinder has two Graphical user interface (GUI) impelmentations, using R/shiny package and R/RGtk2 package, respectively. Details of the RGtk2 implementation may be found in the OEFinderGtk folder.

To run OEFinder shiny implementation, open R and type:
```
install.packages('shiny')
library(shiny)
runGitHub("OEFinder","lengning")
```

Or download the zip file, open R and type:
```
install.packages('shiny')
library(shiny)
runApp("YOUR_PATH/OEFindershiny_0.0.1")
```
YOUR_PATH is the directory that contains the unzipped OEFindershiny_0.0.1 folder.
