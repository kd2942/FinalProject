Final Project Code
================
Kaylin De Silva, Alishah Khan, Cristina Varela
12-06-2024

\#loading libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
```

\#loading datasets

``` r
garden_info = 
  GET("http://data.cityofnewyork.us/resource/p78i-pat6.csv") |> 
  content("parsed") |>
  janitor::clean_names() |>
  drop_na() |>
   mutate(
    borough = 
      recode(
        borough,
        "B" = "Brooklyn",
        "M" = "Manhattan",
        "X" = "Bronx",
        "R" = "Staten Island",
        "Q" = "Queens"
      )
   )
```

    ## Rows: 629 Columns: 27
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): address, borough, gardenname, juris, multipolygon, openhrsf, openh...
    ## dbl (11): assemblydist, communityboard, congressionaldist, coundist, policep...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
site_visits = 
  GET("http://data.cityofnewyork.us/resource/xqbk-beh5.csv") |>
  content("parsed") |>
  janitor::clean_names()
```

    ## Rows: 357 Columns: 35
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): parksid
    ## dbl  (5): inspectionid, totalfencelength, totalsidewalkarea, totalsidewalkle...
    ## lgl (29): nexttoanothergarden, onsiteservice, hydrantw_in15ft, hydrantongard...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

**Flora/Fauna Tab** \#merging the data sets for flora and fauna analysis

``` r
#cleaning site visit data set so it only includes variables involved in flora and fauna 
site_visits_flora_fauna = site_visits |>
  select(parksid, inspectionid, treesingarden, fruittrees, streettrees, chickens, pond, fishinpond, turtles, totalsidewalkarea) |>
 mutate_at(c('treesingarden', 'fruittrees', 'streettrees', 'chickens', 'pond', 'fishinpond', 'turtles'), as.numeric)

flora_fauna_df= 
  inner_join(garden_info, site_visits_flora_fauna, by = "parksid") 
```

``` r
flora_fauna_df = flora_fauna_df |>
  group_by(borough) |>
  mutate(
  "Trees in Garden" = sum(treesingarden),
  "Fruit Trees" = sum(fruittrees),
  "Street Trees" = sum(streettrees),
  "Chickens" = sum(chickens),
  "Pond" = sum(pond),
  "Fish in Pond" = sum(fishinpond),
  "Turtles" = sum(turtles)
  )
```

``` r
flora_fauna_tidy = 
   pivot_longer(
    flora_fauna_df, 
    "Trees in Garden":"Turtles",
    names_to = "item", 
    values_to = "total")

ggplot(flora_fauna_tidy, aes(x = borough, y= total, fill=item)) + 
    geom_bar(position="dodge", stat="identity") +
  labs(title = "Distribution of Flaura + Fauna Features in NYC Gardens",
    x = "Borough",
    y = "Number of Gardens with Each Feature by Borough",
    color = "Flora/Fauna Feature",
    caption = "Data from NYC Open Data"
  ) +
    viridis::scale_fill_viridis(
    name = "Flora/Fauna Feature", 
    discrete = TRUE
  )
```

    ## Warning: Removed 236 rows containing missing values or values outside the scale range
    ## (`geom_bar()`).

![](FinalProjectCode_files/figure-gfm/multi-series%20bar%20chart%20reflecting%20distribution%20of%20flora%20and%20fauna%20features%20by%20borough-1.png)<!-- -->

``` r
fit_logistic_df =
  flora_fauna_df |>
  select('treesingarden', 'fruittrees', 'streettrees', 'borough', 'totalsidewalkarea') |>
  drop_na() |>
  mutate(
    borough = fct_relevel(borough, "Manhattan")
  )
```

    ## Warning: There were 4 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `borough = fct_relevel(borough, "Manhattan")`.
    ## ℹ In group 1: `borough = "Bronx"`.
    ## Caused by warning:
    ## ! 1 unknown level in `f`: Manhattan
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

``` r
fit_logistic_treesingarden = 
  fit_logistic_df |>
  glm(treesingarden~ borough + totalsidewalkarea, data = _, family = binomial()) 

fit_logistic_fruittrees = 
  fit_logistic_df |>
  glm(fruittrees ~ borough + totalsidewalkarea, data = _, family = binomial())

fit_logistic_streettrees = 
  fit_logistic_df |>
  glm(streettrees ~ borough + totalsidewalkarea, data = _, family = binomial())

fit_logistic_treesingarden |> 
  broom::tidy() |> 
  mutate(OR = exp(estimate)) |>
  select(term, log_OR = estimate, OR, p.value) |> 
  knitr::kable(digits = 3)
```

| term                 | log_OR |         OR | p.value |
|:---------------------|-------:|-----------:|--------:|
| (Intercept)          |  2.284 |      9.812 |   0.007 |
| boroughBrooklyn      | -0.165 |      0.848 |   0.843 |
| boroughManhattan     |  0.679 |      1.972 |   0.512 |
| boroughQueens        | -1.314 |      0.269 |   0.186 |
| boroughStaten Island | 12.262 | 211582.430 |   0.993 |
| totalsidewalkarea    |  0.001 |      1.001 |   0.165 |

``` r
fit_logistic_fruittrees |> 
  broom::tidy() |> 
  mutate(OR = exp(estimate)) |>
  select(term, log_OR = estimate, OR, p.value) |> 
  knitr::kable(digits = 3)
```

| term                 | log_OR |         OR | p.value |
|:---------------------|-------:|-----------:|--------:|
| (Intercept)          |  1.739 |      5.691 |   0.001 |
| boroughBrooklyn      | -0.983 |      0.374 |   0.061 |
| boroughManhattan     | -1.138 |      0.321 |   0.039 |
| boroughQueens        | -2.584 |      0.075 |   0.000 |
| boroughStaten Island | 12.518 | 273168.830 |   0.989 |
| totalsidewalkarea    |  0.000 |      1.000 |   0.194 |

``` r
fit_logistic_streettrees |> 
  broom::tidy() |> 
  mutate(OR = exp(estimate)) |>
  select(term, log_OR = estimate, OR, p.value) |> 
  knitr::kable(digits = 3)
```

| term                 |  log_OR |    OR | p.value |
|:---------------------|--------:|------:|--------:|
| (Intercept)          |  -0.126 | 0.882 |   0.735 |
| boroughBrooklyn      |   0.307 | 1.359 |   0.422 |
| boroughManhattan     |   0.159 | 1.172 |   0.703 |
| boroughQueens        |   0.252 | 1.286 |   0.688 |
| boroughStaten Island | -15.047 | 0.000 |   0.986 |
| totalsidewalkarea    |   0.000 | 1.000 |   0.016 |

**Eco-Friendly Practices Tab ** \#merging and cleaning data for analysis
of eco-friendly practices

``` r
site_visits_eco_friendly = site_visits |>
  select(parksid, inspectionid, rainharvesting, composting, aquaponics, solarpanels) |>
 mutate_at(c('rainharvesting', 'composting', 'aquaponics', 'solarpanels'), as.numeric) 

eco_friendly_df= 
  inner_join(garden_info, site_visits_eco_friendly, by = "parksid") 
```
