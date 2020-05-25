President Donald Trump
================
Ojaswi Malik
2020-05-26

## Part 1: President Donald Trump and 2005 video

``` r
#Import the data
trump_data <- here("data", "trump.csv") %>%
  read.csv()

#Calculating predicted values below:
#Using lm() to find the actual value of estimated parameters
lm_mod <- lm(trump ~ video, trump_data)

#using data_grid() to  create grid of data points covering the region where observed data is 
grid <- trump_data %>%
  data_grid(video)

#using augment() to use lm_mod to generate predictions for each observation in the data frame
grid <- augment(lm_mod, newdata = grid)

#Graphing using geom_line() building from predicted values
ggplot(trump_data, mapping = aes(x = video, y = trump)) + 
  geom_point(mapping = aes(alpha = .1)) +
  geom_line(mapping = aes(y = .fitted), data = grid, color = "red", size = 1) +
  labs(
    title = "President Donald Trump and video from 2005",
    x = "Response to question concerning importance of the video",
    y = "Feeling thermometer rating of Trump",
    caption = "Source: 2012 American National Election Studies survey",
    alpha = NULL
  ) +
  theme(legend.position = "none")
```

![](trump_files/figure-gfm/linear%20trump-1.png)<!-- -->

**Relationship**: From the linear regression we can see an indirect
relationship between the importance of the video and feelings towards
Donald Trump. This mean that those who consider that the information
from the video should have mattered a great deal to people when deciding
to vote, have a colder attitude towards Trump. Vice versa, those who
consider that the information from the video should not matter a lot to
people when deciding to vote, have a are more supportive of Trump.

## Part 2: Coefficient Plot

In my analysis, first I will use all the variables (except video) to
generate a coefficient plot to visualize the OLS estimates and
confidence intervals. I will estimate a linear regression model to
estimate how different variables affect attitudes towards Trump.

``` r
## estimate ols model using lm()
trump_mod <- lm(trump ~ female + pid + age + educ,
                data = trump_data)


## extract coefficients using tidy() and include confidence intervals
trump_mod_coeff <- tidy(trump_mod, conf.int = TRUE) 

#Presenting as tidy table
trump_mod_coeff %>%
  mutate(
    term = recode(
      term,
      female = "Female",
      pid = "Party Identification",
      educ = "Educational Attainment",
      age = "Age"
    )
  ) %>%
  kable()
```

| term                   |    estimate | std.error |  statistic |   p.value |    conf.low |   conf.high |
| :--------------------- | ----------: | --------: | ---------: | --------: | ----------: | ----------: |
| (Intercept)            |  17.8436342 | 2.5236762 |   7.070493 | 0.0000000 |  12.8954633 |  22.7918051 |
| Female                 | \-2.3518673 | 0.8934365 | \-2.632383 | 0.0085192 | \-4.1036280 | \-0.6001067 |
| Party Identification   |  10.6595508 | 0.2053129 |  51.918553 | 0.0000000 |  10.2569938 |  11.0621078 |
| Age                    |   0.1734423 | 0.0255144 |   6.797813 | 0.0000000 |   0.1234162 |   0.2234684 |
| Educational Attainment | \-1.8566949 | 0.1956125 | \-9.491697 | 0.0000000 | \-2.2402323 | \-1.4731575 |

``` r
## cleaning up the term names to be  readable
trump_mod_coeff %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # fix variable labels
    term = recode(
      term,
      female = "Female",
      pid = "Party Identification",
      educ = "Educational Attainment",
      age = "Age"
    )
  ) %>%
  # creating the plot
  ggplot(mapping = aes(x = reorder(term, estimate),
                       y = estimate,
                       ymin = conf.low,
                       ymax = conf.high)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Coefficient",
       y = "Value",
       title = "Coefficient plot visualizing OLS estimates",
       subtitle = "Sentiments towards Trump",
       caption = "Source: 2012 American National Election Studies survey")
```

![](trump_files/figure-gfm/my%20model-1.png)<!-- -->

**Observations**: From the above graph we can observe:

  - As party affiliation move towards the Right, one becomes more
    supportive of Trump ie Republicans are more more supportive of Trump
    than Democrats. Hence, for every increase in the 7-level party
    identification scale, the approval rating for Trump increases by
    10.65. As Trump belongs to the Republican party, this strong postive
    relationship is implied and confirmed.

  - More older people are supportive of Trump than the youth. Looking at
    the estimate from the table, with increase in age, the approval
    rating for Trump increases by 0.173.

  - As people become more educated, their approval of Trump falls. This
    is shown by the negative coefficient for Educational Attainment.
    More education, leads to a fall of 1.85 in Trump’s approval ratings.

  - Females are less supportive of Trump. The negative coefficient of
    -2.35 indicates that on average, female sentiments towards Trump are
    2.35 times lower than
    non-females.

## Session info

``` r
devtools::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 3.6.3 (2020-02-29)
    ##  os       macOS Catalina 10.15.4      
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       Asia/Kolkata                
    ##  date     2020-05-26                  
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.0)
    ##  backports     1.1.6   2020-04-05 [1] CRAN (R 3.6.2)
    ##  broom       * 0.5.5   2020-02-29 [1] CRAN (R 3.6.0)
    ##  callr         3.4.3   2020-03-28 [1] CRAN (R 3.6.2)
    ##  cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.6.0)
    ##  cli           2.0.2   2020-02-28 [1] CRAN (R 3.6.0)
    ##  colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.6.0)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.0)
    ##  DBI           1.1.0   2019-12-15 [1] CRAN (R 3.6.0)
    ##  dbplyr        1.4.3   2020-04-19 [1] CRAN (R 3.6.3)
    ##  desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.0)
    ##  devtools      2.3.0   2020-04-10 [1] CRAN (R 3.6.3)
    ##  digest        0.6.25  2020-02-23 [1] CRAN (R 3.6.0)
    ##  dplyr       * 0.8.5   2020-03-07 [1] CRAN (R 3.6.0)
    ##  ellipsis      0.3.0   2019-09-20 [1] CRAN (R 3.6.0)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.0)
    ##  fansi         0.4.1   2020-01-08 [1] CRAN (R 3.6.0)
    ##  farver        2.0.3   2020-01-16 [1] CRAN (R 3.6.0)
    ##  forcats     * 0.5.0   2020-03-01 [1] CRAN (R 3.6.0)
    ##  fs            1.4.1   2020-04-04 [1] CRAN (R 3.6.2)
    ##  generics      0.0.2   2018-11-29 [1] CRAN (R 3.6.0)
    ##  ggplot2     * 3.3.0   2020-03-05 [1] CRAN (R 3.6.0)
    ##  glue          1.4.0   2020-04-03 [1] CRAN (R 3.6.2)
    ##  gtable        0.3.0   2019-03-25 [1] CRAN (R 3.6.0)
    ##  haven         2.2.0   2019-11-08 [1] CRAN (R 3.6.0)
    ##  here        * 0.1     2017-05-28 [1] CRAN (R 3.6.0)
    ##  highr         0.8     2019-03-20 [1] CRAN (R 3.6.0)
    ##  hms           0.5.3   2020-01-08 [1] CRAN (R 3.6.0)
    ##  htmltools     0.4.0   2019-10-04 [1] CRAN (R 3.6.0)
    ##  httr          1.4.1   2019-08-05 [1] CRAN (R 3.6.0)
    ##  jsonlite      1.6.1   2020-02-02 [1] CRAN (R 3.6.0)
    ##  knitr       * 1.28    2020-02-06 [1] CRAN (R 3.6.0)
    ##  labeling      0.3     2014-08-23 [1] CRAN (R 3.6.0)
    ##  lattice       0.20-38 2018-11-04 [1] CRAN (R 3.6.3)
    ##  lifecycle     0.2.0   2020-03-06 [1] CRAN (R 3.6.0)
    ##  lubridate     1.7.8   2020-04-06 [1] CRAN (R 3.6.2)
    ##  magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.0)
    ##  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.0)
    ##  modelr      * 0.1.6   2020-02-22 [1] CRAN (R 3.6.0)
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 3.6.0)
    ##  nlme          3.1-144 2020-02-06 [1] CRAN (R 3.6.3)
    ##  pillar        1.4.3   2019-12-20 [1] CRAN (R 3.6.0)
    ##  pkgbuild      1.0.6   2019-10-09 [1] CRAN (R 3.6.0)
    ##  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 3.6.0)
    ##  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.6.0)
    ##  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 3.6.0)
    ##  processx      3.4.2   2020-02-09 [1] CRAN (R 3.6.0)
    ##  ps            1.3.2   2020-02-13 [1] CRAN (R 3.6.0)
    ##  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 3.6.2)
    ##  R6            2.4.1   2019-11-12 [1] CRAN (R 3.6.0)
    ##  Rcpp          1.0.4.6 2020-04-09 [1] CRAN (R 3.6.3)
    ##  readr       * 1.3.1   2018-12-21 [1] CRAN (R 3.6.0)
    ##  readxl        1.3.1   2019-03-13 [1] CRAN (R 3.6.0)
    ##  remotes       2.1.1   2020-02-15 [1] CRAN (R 3.6.0)
    ##  reprex        0.3.0   2019-05-16 [1] CRAN (R 3.6.0)
    ##  rlang         0.4.5   2020-03-01 [1] CRAN (R 3.6.0)
    ##  rmarkdown     2.1     2020-01-20 [1] CRAN (R 3.6.0)
    ##  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.0)
    ##  rstudioapi    0.11    2020-02-07 [1] CRAN (R 3.6.0)
    ##  rvest         0.3.5   2019-11-08 [1] CRAN (R 3.6.0)
    ##  scales        1.1.0   2019-11-18 [1] CRAN (R 3.6.0)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.0)
    ##  stringi       1.4.6   2020-02-17 [1] CRAN (R 3.6.0)
    ##  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.6.0)
    ##  testthat      2.3.2   2020-03-02 [1] CRAN (R 3.6.0)
    ##  tibble      * 3.0.0   2020-03-30 [1] CRAN (R 3.6.2)
    ##  tidyr       * 1.0.2   2020-01-24 [1] CRAN (R 3.6.0)
    ##  tidyselect    1.0.0   2020-01-27 [1] CRAN (R 3.6.0)
    ##  tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 3.6.0)
    ##  usethis       1.6.0   2020-04-09 [1] CRAN (R 3.6.3)
    ##  vctrs         0.2.4   2020-03-10 [1] CRAN (R 3.6.0)
    ##  withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.0)
    ##  xfun          0.13    2020-04-13 [1] CRAN (R 3.6.2)
    ##  xml2          1.3.1   2020-04-09 [1] CRAN (R 3.6.2)
    ##  yaml          2.2.1   2020-02-01 [1] CRAN (R 3.6.0)
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
