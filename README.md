# OEFinder

OEFinder has two Graphical user interface (GUI) impelmentations, using R/shiny package and R/RGtk2 package, respectively. Details of the RGtk2 implementation may be found in the OEFinderGtk folder.

OEFinder shiny impelemention depends on packages shiny, shinyFiles, gdata, and EBSeq. To install these packages, open R and run:
```
install.packages('shiny')
install.packages('gdata')
install.packages('shinyFiles')
source("http://bioconductor.org/biocLite.R")
biocLite("EBSeq")
```

To run OEFinder shiny implementation, open R and type:
```
runGitHub("OEFinder","lengning")
```

Or download the zip file, open R and type:
```
runApp("YOUR_PATH/OEFinder_Shiny")
```
YOUR_PATH is the directory that contains the unzipped OEFindershiny_0.0.2 folder.


![Screenshot](https://github.com/lengning/OEFinder/blob/master/figs/OEFinder_screenshot.png)
