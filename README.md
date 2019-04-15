# nlpcam

An R package to access Brazilian Câmara dos Deputados transcripts in a structured tidy format.

### Instalation

```
install.packages( "devtools" , repos = "http://cran.rstudio.com/" )
library(devtools)
install_github( "brunoritter/nlpcam" , dependencies = TRUE )
```

### Usage


```
df <- get_data('01/04/2019','15/04/2019')
```

This is a beta version. Tested only with 2019 data.
