U.S. Fish and Wildlife Imports
================

This dataset describing wildlife imports between 2000-2014 was collected
by the U.S. Fish and Wildlife Services and cleaned by Law Enforcement
Management Information Systems (LEMIS). The EDA below focused on the
fish taxa.

------------------------------------------------------------------------

Installing packages and loading libraries

``` r
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ✔ purrr   0.3.4

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(skimr)
```

Set the working directory

``` r
setwd("~/Documents/Rfor_Bio/USFWS")
```

Import Dataset

``` r
lemis<-read_csv("lemis.csv")
```

    ## Rows: 5512706 Columns: 28
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (21): species_code, taxa, class, genus, species, subspecies, specific_n...
    ## dbl   (5): control_number, quantity, value, disposition_year, shipment_year
    ## date  (2): disposition_date, shipment_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Take a quick overview of which taxa has the most observations

``` r
str(lemis)
```

    ## spec_tbl_df [5,512,706 × 28] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ control_number  : num [1:5512706] 2e+09 2e+09 2e+09 2e+09 2e+09 ...
    ##  $ species_code    : chr [1:5512706] "CY0?" "CIRA" "CORJ" "PIMX" ...
    ##  $ taxa            : chr [1:5512706] "reptile" "coral" "coral" "shell" ...
    ##  $ class           : chr [1:5512706] "Reptilia" "Anthozoa" "Anthozoa" "Bivalvia" ...
    ##  $ genus           : chr [1:5512706] "Crocodylus" "Cirrhipathes" "Corallium" "Pinctada" ...
    ##  $ species         : chr [1:5512706] "sp." "anguinas" "japonicum" "maxima" ...
    ##  $ subspecies      : chr [1:5512706] NA NA NA NA ...
    ##  $ specific_name   : chr [1:5512706] NA "BLACK" "RED" NA ...
    ##  $ generic_name    : chr [1:5512706] "CROCODILE" "CORAL" "CORAL" "OYSTER" ...
    ##  $ description     : chr [1:5512706] "SHO" "JWL" "JWL" "JWL" ...
    ##  $ quantity        : num [1:5512706] 1 110 9 289 10 25 4 2 5520 3 ...
    ##  $ unit            : chr [1:5512706] "NO" "NO" "NO" "NO" ...
    ##  $ value           : num [1:5512706] 0 425 52 321 60 ...
    ##  $ country_origin  : chr [1:5512706] "XX" "TW" "TW" "TW" ...
    ##  $ country_imp_exp : chr [1:5512706] "MX" "TW" "TW" "TW" ...
    ##  $ purpose         : chr [1:5512706] "non-standard value" "T" "T" "T" ...
    ##  $ source          : chr [1:5512706] "U" "W" "W" "W" ...
    ##  $ action          : chr [1:5512706] "R" "C" "C" "C" ...
    ##  $ disposition     : chr [1:5512706] "S" "C" "C" "C" ...
    ##  $ disposition_date: Date[1:5512706], format: "2000-01-03" "2000-01-04" ...
    ##  $ disposition_year: num [1:5512706] 2000 2000 2000 2000 2000 2000 2000 2000 2000 2000 ...
    ##  $ shipment_date   : Date[1:5512706], format: "2000-01-01" "2000-01-01" ...
    ##  $ shipment_year   : num [1:5512706] 2000 2000 2000 2000 2000 2000 2000 2000 2000 2000 ...
    ##  $ import_export   : chr [1:5512706] "I" "I" "I" "I" ...
    ##  $ port            : chr [1:5512706] "EL" "HA" "HA" "HA" ...
    ##  $ us_co           : chr [1:5512706] NA "LUCORAL" "LUCORAL" "LUCORAL" ...
    ##  $ foreign_co      : chr [1:5512706] NA "LUPERLA INTL. GROUP CORP." "LUPERLA INTL. GROUP CORP." "LUPERLA INTL. GROUP CORP." ...
    ##  $ cleaning_notes  : chr [1:5512706] "Original value in 'purpose' column: N" NA NA NA ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   control_number = col_double(),
    ##   ..   species_code = col_character(),
    ##   ..   taxa = col_character(),
    ##   ..   class = col_character(),
    ##   ..   genus = col_character(),
    ##   ..   species = col_character(),
    ##   ..   subspecies = col_character(),
    ##   ..   specific_name = col_character(),
    ##   ..   generic_name = col_character(),
    ##   ..   description = col_character(),
    ##   ..   quantity = col_double(),
    ##   ..   unit = col_character(),
    ##   ..   value = col_double(),
    ##   ..   country_origin = col_character(),
    ##   ..   country_imp_exp = col_character(),
    ##   ..   purpose = col_character(),
    ##   ..   source = col_character(),
    ##   ..   action = col_character(),
    ##   ..   disposition = col_character(),
    ##   ..   disposition_date = col_date(format = ""),
    ##   ..   disposition_year = col_double(),
    ##   ..   shipment_date = col_date(format = ""),
    ##   ..   shipment_year = col_double(),
    ##   ..   import_export = col_character(),
    ##   ..   port = col_character(),
    ##   ..   us_co = col_character(),
    ##   ..   foreign_co = col_character(),
    ##   ..   cleaning_notes = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
summary(lemis)
```

    ##  control_number      species_code           taxa              class          
    ##  Min.   :2.000e+09   Length:5512706     Length:5512706     Length:5512706    
    ##  1st Qu.:2.005e+09   Class :character   Class :character   Class :character  
    ##  Median :2.008e+09   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :2.008e+09                                                           
    ##  3rd Qu.:2.012e+09                                                           
    ##  Max.   :2.016e+09                                                           
    ##                                                                              
    ##     genus             species           subspecies        specific_name     
    ##  Length:5512706     Length:5512706     Length:5512706     Length:5512706    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  generic_name       description           quantity             unit          
    ##  Length:5512706     Length:5512706     Min.   :        0   Length:5512706    
    ##  Class :character   Class :character   1st Qu.:        2   Class :character  
    ##  Mode  :character   Mode  :character   Median :       10   Mode  :character  
    ##                                        Mean   :     2479                     
    ##                                        3rd Qu.:      141                     
    ##                                        Max.   :151331000                     
    ##                                                                              
    ##      value           country_origin     country_imp_exp      purpose         
    ##  Min.   :        0   Length:5512706     Length:5512706     Length:5512706    
    ##  1st Qu.:       29   Class :character   Class :character   Class :character  
    ##  Median :      182   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :     4773                                                           
    ##  3rd Qu.:     1244                                                           
    ##  Max.   :118000000                                                           
    ##  NA's   :1890969                                                             
    ##     source             action          disposition        disposition_date    
    ##  Length:5512706     Length:5512706     Length:5512706     Min.   :1999-11-04  
    ##  Class :character   Class :character   Class :character   1st Qu.:2004-11-12  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :2008-03-05  
    ##                                                           Mean   :2008-02-18  
    ##                                                           3rd Qu.:2011-09-09  
    ##                                                           Max.   :2015-04-10  
    ##                                                           NA's   :26          
    ##  disposition_year shipment_date        shipment_year  import_export     
    ##  Min.   :1999     Min.   :2000-01-01   Min.   :2000   Length:5512706    
    ##  1st Qu.:2004     1st Qu.:2004-11-03   1st Qu.:2004   Class :character  
    ##  Median :2008     Median :2008-02-27   Median :2008   Mode  :character  
    ##  Mean   :2008     Mean   :2008-02-11   Mean   :2008                     
    ##  3rd Qu.:2011     3rd Qu.:2011-09-04   3rd Qu.:2011                     
    ##  Max.   :2015     Max.   :2014-12-31   Max.   :2014                     
    ##  NA's   :26                                                             
    ##      port              us_co            foreign_co        cleaning_notes    
    ##  Length:5512706     Length:5512706     Length:5512706     Length:5512706    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ## 

``` r
lemis %>%
  count(taxa)
```

    ## # A tibble: 15 × 2
    ##    taxa              n
    ##    <chr>         <int>
    ##  1 amphibian     66452
    ##  2 annelid        3906
    ##  3 bird         330921
    ##  4 coral        688322
    ##  5 crustacean   135996
    ##  6 echinoderms   38033
    ##  7 fish         462527
    ##  8 insect       169620
    ##  9 mammal      1589968
    ## 10 other        138873
    ## 11 plant          2012
    ## 12 reptile      724655
    ## 13 shell       1109070
    ## 14 spider        16977
    ## 15 <NA>          35374

I am choosing fish as a taxa because it appears to have enough data to
work with.

Now I will create a new dataset only looking at the fish taxa, looking
it over before I begin any plotting or manipulation.

``` r
fish <- lemis %>%
  filter(taxa=="fish")

str(fish)
```

    ## spec_tbl_df [462,527 × 28] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ control_number  : num [1:462527] 2e+09 2e+09 2e+09 2e+09 2e+09 ...
    ##  $ species_code    : chr [1:462527] "TROP" "TROP" "TROP" "TROP" ...
    ##  $ taxa            : chr [1:462527] "fish" "fish" "fish" "fish" ...
    ##  $ class           : chr [1:462527] "Actinopterygii" "Actinopterygii" "Actinopterygii" "Actinopterygii" ...
    ##  $ genus           : chr [1:462527] "All live tropical fish" "All live tropical fish" "All live tropical fish" "All live tropical fish" ...
    ##  $ species         : chr [1:462527] "(including goldfish)" "(including goldfish)" "(including goldfish)" "(including goldfish)" ...
    ##  $ subspecies      : chr [1:462527] NA NA NA NA ...
    ##  $ specific_name   : chr [1:462527] NA NA NA NA ...
    ##  $ generic_name    : chr [1:462527] NA NA NA NA ...
    ##  $ description     : chr [1:462527] "LIV" "LIV" "LIV" "LIV" ...
    ##  $ quantity        : num [1:462527] 1560 19000 22500 1034 117757 ...
    ##  $ unit            : chr [1:462527] "NO" "NO" "NO" "NO" ...
    ##  $ value           : num [1:462527] 645 185 4350 949 11971 ...
    ##  $ country_origin  : chr [1:462527] "TT" "TT" "CN" "ID" ...
    ##  $ country_imp_exp : chr [1:462527] "TT" "TT" "CN" "ID" ...
    ##  $ purpose         : chr [1:462527] "T" "T" "T" "T" ...
    ##  $ source          : chr [1:462527] "W" "W" "W" "W" ...
    ##  $ action          : chr [1:462527] "C" "C" "C" "C" ...
    ##  $ disposition     : chr [1:462527] "C" "C" "C" "C" ...
    ##  $ disposition_date: Date[1:462527], format: "2000-01-03" "2000-01-03" ...
    ##  $ disposition_year: num [1:462527] 2000 2000 2000 2000 2000 2000 2000 2000 2000 2000 ...
    ##  $ shipment_date   : Date[1:462527], format: "2000-01-01" "2000-01-01" ...
    ##  $ shipment_year   : num [1:462527] 2000 2000 2000 2000 2000 2000 2000 2000 2000 2000 ...
    ##  $ import_export   : chr [1:462527] "I" "I" "I" "I" ...
    ##  $ port            : chr [1:462527] "NY" "NY" "LA" "LA" ...
    ##  $ us_co           : chr [1:462527] "BOZMAN IMPORTS LTD." "BOZMAN IMPORTS LTD." "Z IMPORTS INC." "GOLDEN INA INC." ...
    ##  $ foreign_co      : chr [1:462527] "CHARAN'S FISH FARM" "CHARAN'S FISH FARM" "XIAOSHAN IMPORT & EXPORT CO" "PANCA NAGA JAYA C.V." ...
    ##  $ cleaning_notes  : chr [1:462527] "Original value in 'genus' column: all live trop. fsh, 'species' column: (incl. goldfish)" "Original value in 'genus' column: all live trop. fsh, 'species' column: (incl. goldfish)" "Original value in 'genus' column: all live trop. fsh, 'species' column: (incl. goldfish)" "Original value in 'genus' column: all live trop. fsh, 'species' column: (incl. goldfish)" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   control_number = col_double(),
    ##   ..   species_code = col_character(),
    ##   ..   taxa = col_character(),
    ##   ..   class = col_character(),
    ##   ..   genus = col_character(),
    ##   ..   species = col_character(),
    ##   ..   subspecies = col_character(),
    ##   ..   specific_name = col_character(),
    ##   ..   generic_name = col_character(),
    ##   ..   description = col_character(),
    ##   ..   quantity = col_double(),
    ##   ..   unit = col_character(),
    ##   ..   value = col_double(),
    ##   ..   country_origin = col_character(),
    ##   ..   country_imp_exp = col_character(),
    ##   ..   purpose = col_character(),
    ##   ..   source = col_character(),
    ##   ..   action = col_character(),
    ##   ..   disposition = col_character(),
    ##   ..   disposition_date = col_date(format = ""),
    ##   ..   disposition_year = col_double(),
    ##   ..   shipment_date = col_date(format = ""),
    ##   ..   shipment_year = col_double(),
    ##   ..   import_export = col_character(),
    ##   ..   port = col_character(),
    ##   ..   us_co = col_character(),
    ##   ..   foreign_co = col_character(),
    ##   ..   cleaning_notes = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
summary(fish)
```

    ##  control_number      species_code           taxa              class          
    ##  Min.   :2.000e+09   Length:462527      Length:462527      Length:462527     
    ##  1st Qu.:2.004e+09   Class :character   Class :character   Class :character  
    ##  Median :2.008e+09   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :2.008e+09                                                           
    ##  3rd Qu.:2.012e+09                                                           
    ##  Max.   :2.016e+09                                                           
    ##                                                                              
    ##     genus             species           subspecies        specific_name     
    ##  Length:462527      Length:462527      Length:462527      Length:462527     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  generic_name       description           quantity            unit          
    ##  Length:462527      Length:462527      Min.   :       0   Length:462527     
    ##  Class :character   Class :character   1st Qu.:      50   Class :character  
    ##  Mode  :character   Mode  :character   Median :     550   Mode  :character  
    ##                                        Mean   :    6825                     
    ##                                        3rd Qu.:    4048                     
    ##                                        Max.   :19000000                     
    ##                                                                             
    ##      value          country_origin     country_imp_exp      purpose         
    ##  Min.   :       0   Length:462527      Length:462527      Length:462527     
    ##  1st Qu.:     233   Class :character   Class :character   Class :character  
    ##  Median :    1069   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :    3638                                                           
    ##  3rd Qu.:    2584                                                           
    ##  Max.   :85000000                                                           
    ##  NA's   :145281                                                             
    ##     source             action          disposition        disposition_date    
    ##  Length:462527      Length:462527      Length:462527      Min.   :1999-12-30  
    ##  Class :character   Class :character   Class :character   1st Qu.:2004-02-11  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :2007-07-03  
    ##                                                           Mean   :2007-08-21  
    ##                                                           3rd Qu.:2011-05-02  
    ##                                                           Max.   :2015-04-09  
    ##                                                           NA's   :2           
    ##  disposition_year shipment_date        shipment_year  import_export     
    ##  Min.   :1999     Min.   :2000-01-01   Min.   :2000   Length:462527     
    ##  1st Qu.:2004     1st Qu.:2004-02-05   1st Qu.:2004   Class :character  
    ##  Median :2007     Median :2007-06-29   Median :2007   Mode  :character  
    ##  Mean   :2007     Mean   :2007-08-16   Mean   :2007                     
    ##  3rd Qu.:2011     3rd Qu.:2011-04-28   3rd Qu.:2011                     
    ##  Max.   :2015     Max.   :2014-12-31   Max.   :2014                     
    ##  NA's   :2                                                              
    ##      port              us_co            foreign_co        cleaning_notes    
    ##  Length:462527      Length:462527      Length:462527      Length:462527     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ## 

Now I want to narrow my dataset to only observe value (of import) and
import year within the fish taxa.

``` r
smfish <-fish %>%
  select(taxa, value, shipment_year)
  
skim(smfish)
```

|                                                  |        |
|:-------------------------------------------------|:-------|
| Name                                             | smfish |
| Number of rows                                   | 462527 |
| Number of columns                                | 3      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| character                                        | 1      |
| numeric                                          | 2      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| taxa          |         0 |             1 |   4 |   4 |     0 |        1 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |        sd |   p0 |  p25 |  p50 |  p75 |     p100 | hist  |
|:--------------|----------:|--------------:|--------:|----------:|-----:|-----:|-----:|-----:|---------:|:------|
| value         |    145281 |          0.69 | 3638.09 | 155371.85 |    0 |  233 | 1069 | 2584 | 85000000 | ▇▁▁▁▁ |
| shipment_year |         0 |          1.00 | 2007.12 |      4.23 | 2000 | 2004 | 2007 | 2011 |     2014 | ▆▇▇▇▇ |

``` r
summary(smfish)
```

    ##      taxa               value          shipment_year 
    ##  Length:462527      Min.   :       0   Min.   :2000  
    ##  Class :character   1st Qu.:     233   1st Qu.:2004  
    ##  Mode  :character   Median :    1069   Median :2007  
    ##                     Mean   :    3638   Mean   :2007  
    ##                     3rd Qu.:    2584   3rd Qu.:2011  
    ##                     Max.   :85000000   Max.   :2014  
    ##                     NA's   :145281

I want to know if there were certain years when fish imports slowed or
skyrocketed, basically how has the fish trade varied over time?

Looking at the number of fish imports per year, we have the following
plot.

``` r
ggplot(smfish, aes(shipment_year)) + geom_bar() + xlab ("Year") + ylab ("Number of Fish Shipments to US") + ggtitle(label="Imported Fish Shipments Between 2000-2014", subtitle="Katy Baker")
```

![](USFWS_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Now I wanted to look at the range of declared value of the fish
shipments to the US over time. I wondered if the fish trade is dominated
by few, expensive fish products, or many, lower cost fish products.

I use the jitter to reduce overplotting.

``` r
ggplot(smfish, aes(taxa, value)) + geom_jitter(height=0, width= .5, alpha=.7, size=1, color="blue")
```

    ## Warning: Removed 145281 rows containing missing values (geom_point).

![](USFWS_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

I wanted to see what that outlier was. So I used the filter function to
look at all the observations over 10 million dollars.

``` r
outlier<-lemis %>%
  filter(value>1e7 & taxa=="fish") %>%
  arrange(value)
outlier
```

    ## # A tibble: 3 × 28
    ##   control_number species_code taxa  class genus species subspecies specific_name
    ##            <dbl> <chr>        <chr> <chr> <chr> <chr>   <chr>      <chr>        
    ## 1     2005565933 ASLT         fish  Acti… Acip… stella… <NA>       STELLATE     
    ## 2     2005565933 HUSH         fish  Acti… Huso  huso    <NA>       BELUGA       
    ## 3     2012151524 TSHA         fish  Elas… Gale… cuvieri <NA>       TIGER        
    ## # … with 20 more variables: generic_name <chr>, description <chr>,
    ## #   quantity <dbl>, unit <chr>, value <dbl>, country_origin <chr>,
    ## #   country_imp_exp <chr>, purpose <chr>, source <chr>, action <chr>,
    ## #   disposition <chr>, disposition_date <date>, disposition_year <dbl>,
    ## #   shipment_date <date>, shipment_year <dbl>, import_export <chr>, port <chr>,
    ## #   us_co <chr>, foreign_co <chr>, cleaning_notes <chr>

Turns out the most extreme value was the import of just one Tiger Shark
from Australia worth 85 million dollars. I thought it could be in error
so I did a quick google on the seller (forein_co) and apparently it was
a piece of art by Damien Hirst called The Physical Impossibility of
Death in the Mind of Someone Living (1991).

It was literally a tiger shark preserved in a glass tank of
formaldehyde. For 85 million dollars.

Not only that but according to this article,
<https://www.thegentlemansjournal.com/5-hedge-fund-billionaire-steve-cohens-extravagant-purchases/>
the buyer, a hedge-fund billionaire, purchased it in 2006 and didn’t
even have it shipped to the US until 2012, per LEMIS.

For the purpose of this data exploration we are not going to include
this ridiculous display of wealth which I am obviously jealous of and
which as made me rethink all of my life choices.

We are going to include the 10 million dollar caviar, though, since it
is actually relevant to this study, albeit ridiculous as well.

I create a new dataframe and use the filter function to only include
observations where the value is less than 20M

``` r
smsmfish<- smfish %>%
  filter(value<2e7) 
```

Then I wanted to also get rid of the ‘0’ value, so I will use the filter
function in the same way after I make sure there are no values between 0
and 1

``` r
outlier2<-lemis %>%
  filter(value<1 & taxa=="fish") %>%
  arrange(desc(value))
outlier2
```

    ## # A tibble: 12,553 × 28
    ##    control_number species_code taxa  class          genus     species subspecies
    ##             <dbl> <chr>        <chr> <chr>          <chr>     <chr>   <chr>     
    ##  1     2000826557 LEDA         fish  Actinopterygii Percina   panthe… <NA>      
    ##  2     2000698251 CCX?         fish  Elasmobranchii Carchari… sp.     <NA>      
    ##  3     2000726897 SALM         fish  Actinopterygii Salmonid… sp.     <NA>      
    ##  4     2000696823 FSCT         fish  <NA>           <NA>      <NA>    <NA>      
    ##  5     2000722212 TROP         fish  Actinopterygii All live… (inclu… <NA>      
    ##  6     2000789234 NONF         fish  <NA>           Non-CITE… sp.     <NA>      
    ##  7     2000729219 NONF         fish  <NA>           Non-CITE… sp.     <NA>      
    ##  8     2000789231 NONF         fish  <NA>           Non-CITE… sp.     <NA>      
    ##  9     2000797103 NONF         fish  <NA>           Non-CITE… sp.     <NA>      
    ## 10     2000734433 DAS?         fish  Elasmobranchii Dasyatis  sp.     <NA>      
    ## # … with 12,543 more rows, and 21 more variables: specific_name <chr>,
    ## #   generic_name <chr>, description <chr>, quantity <dbl>, unit <chr>,
    ## #   value <dbl>, country_origin <chr>, country_imp_exp <chr>, purpose <chr>,
    ## #   source <chr>, action <chr>, disposition <chr>, disposition_date <date>,
    ## #   disposition_year <dbl>, shipment_date <date>, shipment_year <dbl>,
    ## #   import_export <chr>, port <chr>, us_co <chr>, foreign_co <chr>,
    ## #   cleaning_notes <chr>

``` r
smsmfish<- smfish %>%
  filter(between(value,1, 2e7))


skim(smsmfish)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | smsmfish |
| Number of rows                                   | 304692   |
| Number of columns                                | 3        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| character                                        | 1        |
| numeric                                          | 2        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| taxa          |         0 |             1 |   4 |   4 |     0 |        1 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |       sd |   p0 |  p25 |  p50 |  p75 |     p100 | hist  |
|:--------------|----------:|--------------:|--------:|---------:|-----:|-----:|-----:|-----:|---------:|:------|
| value         |         0 |             1 | 3509.02 | 37735.58 |    1 |  323 | 1147 | 2702 | 10832295 | ▇▁▁▁▁ |
| shipment_year |         0 |             1 | 2005.49 |     3.94 | 2000 | 2002 | 2005 | 2007 |     2013 | ▇▇▆▁▆ |

Then plotting again except with more extreme jitter to reduce
overplotting

``` r
ggplot(smsmfish, aes(taxa, value)) + geom_jitter(height=0, width= .5, alpha=.7, size=1, color="blue") + xlab ("") + ylab ("Value of Fish Shipments") + ggtitle(label="Declared Value of Imported Fish Shipments Between 2000-2014", subtitle="Katy Baker")
```

![](USFWS_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
smsmfish<- smfish %>%
  filter(between(value,5, 2500))

ggplot(smsmfish, aes(taxa, value)) + geom_boxplot(color="blue") + xlab ("") + ylab ("Value of Fish Shipments") + ggtitle(label="Declared Value of Imported Fish Shipments Under $2500 Between 2000-2014", subtitle="Katy Baker")
```

![](USFWS_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

RESULTS

While the fish trade seems to be relatively stable and did not have much
volatility, there did appear to be a momentary decline in imported fish
from 2006-2010. Without looking at other variables such as species, it
is difficult to begin to ask why this might be. For instance, if the
decline overall was proportional to the decline of imports for a certain
species, say tuna, then we might be able to distinguish whether the
downward trend is fish imports overall or just one sector.

The EDA on declared value of fish shipments to the US did, however,
prove to be informative in its own right. It seems clear that the fish
import trade is dominated by smaller valued imports. There are very
expensive fish/products being imported, and although they may make up a
significant portion of the overall value of the fish trade, the average
shipment is only valued at 0.03% of the highest valued import we
considered (the 10 million dollar caviar). It was because of this
extremely right-skewed data that a histogram was not a good choice for
depicting the spread of values.

And who doesn’t love exploring data to uncover the ultimate American
dream: getting so filthy rich off an imaginary construct that you become
bored with spending money and resort to buying obscure pieces of
taxidermy someone deemed art that - no doubt - was a royal pain to ship.
