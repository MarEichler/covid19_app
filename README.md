# COVID-19 App 

Updated version of the [covid19](https://github.com/MarEichler/covid19) repository that was started in Spring 2020.  


## Static Data 

There are 5 'static' data files that do NOT need to be updated daily.  If running the app from scratch, only need to create these files one time.    

| File | Description | 
|:----|:--------------|
|`shp_cnty.RDS` | Shape file of United States at the county level (not currently used in app, may use later)<br>Source: [`{urbnmapr}`](https://urbaninstitute.github.io/urbnmapr/) package | 
|`pop_state.RDS` | Population count at the state level<br>Source: [US Census file](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html) | 
|`pop_cnty.RDS` | Population count at the county level (not currently used in app, may use later)<br>Source: [US Census file](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html)  | 
|`shp_hex.RDS` | Hex map shape file of United States<br>Source: [MarEichler/us_hex_map](https://github.com/MarEichler/us_hex_map), which used NPR code to create hex map points | 
|`shp_hex_centers.RDS` | Center points for each hex item<br>Source: [MarEichler/us_hex_map](https://github.com/MarEichler/us_hex_map) | 

```r
box::use(prep/static_data)
static_data$save_static_data() 
#> SUCCESS [2022-03-03 14:39:30] Saved shp_cnty.RDS
#> SUCCESS [2022-03-03 14:39:30] Saved pop_state.RDS
#> SUCCESS [2022-03-03 14:39:30] Saved pop_cnty.RDS
#> SUCCESS [2022-03-03 14:39:30] Saved shp_hex.RDS
#> SUCCESS [2022-03-03 14:39:30] Saved shp_hex_centers.RDS
```


## R and Package Information  

R version 4.1.1 (2021-08-10) -- "Kick Things" 

| Package | Version | Information | 
|:--------|:----|:----------------|
| [`{box}`](https://klmr.me/box/) | 1.1.0 | Create box modules | 
| [`{data.table}`](https://rdatatable.gitlab.io/data.table/) | 1.14.0| Manage/Manipulate data (much faster than base R or tribbles) | 
| [`{DescTools}`](https://andrisignorell.github.io/DescTools/) | 0.99.43 | Use `RoundTo()` to create axis breaks for trend chart | 
| [`{DT}`](https://rstudio.github.io/DT/)  | 0.19  | Display data tables | 
| [`{shinycssloaders}`](https://daattali.com/shiny/shinycssloaders-demo/)| 1.0.0 | Loading animations for plots | 
| [`{shinyWidgets}`](https://dreamrs.github.io/shinyWidgets/index.html) | 0.6.2 | Additional widgets, `pickerInput()` | 
| [`{logger}`](https://daroczig.github.io/logger/index.html) | 0.2.0 | Build-in log messages | 
| [`{scales}`](https://scales.r-lib.org/) | 1.1.1 | Scaling for visualizations, `percent()`/`comma()` | 
| [`{shiny}`](https://shiny.rstudio.com/) | 1.7.0 | R Shiny Applications | 
| [`{urbnmapr}`](https://urbaninstitute.github.io/urbnmapr/) | 0.0.0.9002 | Pull county shape files (not currently used in app) |
| [`{vroom}`](https://vroom.r-lib.org/) | 1.5.4 | Read in large data quickly | 
| **base R**[^1] | | | 
| `{stats}` | 4.1.1 | Create quantile breaks with `quantile()` | 
| `{datasets}` | 4.1.1 | `state.abb`-  list of 50 state abbreviations | 
| **Core tidyverse**[^2] | | | 
| [`{tidyverse}`](https://www.tidyverse.org/) [`{dplyr}`](https://dplyr.tidyverse.org/)| 1.0.7 | Data manipulation | 
| [`{tidyverse}`](https://www.tidyverse.org/) [`{ggplot2}`](https://ggplot2.tidyverse.org/) | 3.3.5 | Create hex map and trends chart | 
| [`{tidyverse}`](https://www.tidyverse.org/) [`{readr}`](https://readr.tidyverse.org/)  | 2.0.1 | Read in smaller csv files |
| [`{tidyverse}`](https://www.tidyverse.org/) [`{tibble}`](https://tibble.tidyverse.org/) | 3.1.4 | Create tibbles for meta data (like variables and their names) | 
| [`{tidyverse}`](https://www.tidyverse.org/) [`{tidyr}`](https://tidyr.tidyverse.org/)  | 1.1.3 | Use `drop_na()` for creating data frame to plot | 
| **tidyverse** | | | 
| [`{tidyverse}`](https://www.tidyverse.org/) [`{lubridate}`](https://lubridate.tidyverse.org/) | 1.7.10| Create/Manage date variables |
| [`{tidyverse}`](https://www.tidyverse.org/) [`{glue}`](https://glue.tidyverse.org/) | 1.5.1 | Combine text and R code to crate string of text |
| [`{tidyverse}`](https://www.tidyverse.org/) [`{magrittr}`](https://magrittr.tidyverse.org/) | 1.5.1 | Pipe Operator, `%>%` |


[^1]: Package is automatically loaded in each R session but since it's not technically included in base R functions/datasets, it has to be specified within the box modules. 

[^2]: All tidyverse packages can be installed using installed using `install.packages("tidyverse")` but core tidyverse packages can also be imported together using `library("tidyverse")`.  For box modules refer to each specific package in the `box::use()` statement instead.  



