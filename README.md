# OEFinder

OEFinder has two Graphical user interface (GUI) impelmentations, using R/shiny package and R/RGtk2 package, respectively. Details of the RGtk2 implementation may be found in the OEFinderGtk folder.

OEFinder shiny impelemention depends on packages shiny, gdata and EBSeq. To install these packages, open R and run:
```
install.packages('shiny')
install.packages('gdata')
source("http://bioconductor.org/biocLite.R")
biocLite("EBSeq")
```

To run OEFinder shiny implementation, open R and type:
```
runGitHub("OEFinder","lengning")
```

Or download the zip file, open R and type:
```
runApp("YOUR_PATH/OEFindershiny_0.0.1")
```
YOUR_PATH is the directory that contains the unzipped OEFindershiny_0.0.1 folder.
