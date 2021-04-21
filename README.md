# dwd-cmsaf-r

## R and RStudio
* [R-Basics](https://www.informatik-aktuell.de/entwicklung/programmiersprachen/was-ist-r.html)
* https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-RStudio

## Initiate Environment
### General
* Installation of RStudio and R
    * https://rstudio.com/products/rstudio/download/

### Packages
* The package [devtools](https://cran.r-project.org/web/packages/devtools/index.html) will be used for the developement of R-Packages. 
``` R 
install.packages("devtools", dependencies = TRUE)
```
* The package [rhub](https://cran.r-project.org/web/packages/rhub/index.html) will be used to check compatibility of different platforms.
``` R
install.packages("rhub", dependencies = TRUE)
```
* The Package [roxygen2](https://cran.r-project.org/web/packages/roxygen2/index.html) will be used to create documentations for CRAN. It will be installed with devtools. 

## CM SAF
* macOS:
    * The R-Shiny application (CM SAF R Toolbox) requires [XQuartz](https://www.xquartz.org). 

* Loading CM SAF Developer Packages
```R
devtools::load_all("./cmsafops")
devtools::load_all("./cmsafvis")
devtools::load_all("./cmsafs")
```
* Start the CM SAF R Toolbox
```R
run_toolbox(launch.browser = TRUE)
```

## Build and Installation 
### Build Package
``` R 
devtools::build()
```

### Installation of Packages
* (Windows) Installation of RTools
* Navigate to R package directory
```bash
cd\
dir Users\ ...
```
* command for installation
```bash
R CMD INSTALL cmsafops.tar.gz
```

## Other important commands
* Start Unit-tests: 
``` R 
devtools::test() 
```

* Documention dev-package: 
```R
devtools::document()
```

* Load dev-Package: 
```R
devtools::load_all()
```

* Add a packages (e.g., trend package):
```R
usethis::use_package("trend")
```