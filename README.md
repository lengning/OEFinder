# OEFinder
OEFinder requires R version >= 3.1.3




##Trouble Shooting
For mac users, if you encounter problems in running RGtk2, I found the following link to be useful:
https://gist.github.com/sebkopf/9405675

My R kept crashing when calling RGtk2 after upgrading my mac OS X to Yosemite. It was fixed after following the these steps:
```
Reinstall XQuartz
(Re)install Macports and XCode
In terminal, run:
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
sudo port selfupdate
sudo port install pkgconfig
sudo port install gtk2 +x11
In R,
install.packages("RGtk2")
or (if binary version of RGtk2 is not available for your R version)
install.packages("RGtk2", type="source")
```
