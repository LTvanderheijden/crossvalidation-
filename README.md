# crossvalidation
This repository contains the code of a shiny app to perform bland-altman analysis and passing-nablock regression. It contains the following files: 
1. App.R - The file containing the R code of the shiny app performing cross-validation analysis of bioanalytical methods. 
2. crossvalidation.Rproj - The R projects file
3. report.Rmd - The R markdown file used to print a report of the cross-validation.

# Docker app

https://juanitorduz.github.io/dockerize-a-shinyapp/

To build the container, on a machine with Docker installed, in a clone of this repo, 

```
docker build -t crossvalidation .
```

To run the container,

```
docker run --rm -p 80:3838 crossvalidation
```

Then browse to http://localhost
