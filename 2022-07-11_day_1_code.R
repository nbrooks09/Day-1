################################################################################
# Name: Intro to Data Wrangling, Exploration, and Analysis with R Day 1
# Author: Nina Brooks 
# Date: July 11, 2022
################################################################################

################################################################################
                            # Load Packages #
################################################################################
library(readxl) # this package allows you to read .xlsx files into R
library(haven) # this package allows you to read & write stata .dta files
library(tabulator) # this is a package that is useful for making cross-tabs
library(janitor) # contains useful commands for data cleaning
library(tidycensus) # an R package that allows users to interface with a select
                    # number of the US Census Bureau’s data APIs and return 
                    # tidyverse-ready data frame
library(rvest) # a tidyverse package for webscraping
library(glue) # tidyverse-adjacent package makes working with interpreted string literals simpler
library(lubridate) # tidyverse package for working with dates
library(tidyverse) # this loads the 8 core packages of the tidyverse
library(tidylog) # this is a package that adds extra explanation of tidyverse commands


################################################################################
                            # Installing packages #
################################################################################
# Before you can load a package with library(packagename), it has to be 
# installed. You only need to do this once per package, but packages do need to
# be updated.

# For example: 
install.packages("tidycensus")
install.packages("tidyverse")
# you can comment these lines out if you already installed these packages

#install.packages(c("tidycensus", "tidyverse"))
################################################################################
                            # Working directory #
################################################################################
# Like any program, it's crucial to pay attention to your working directory in R.
# This is tells R where to source files from and where to save output to.

getwd()
setwd("~/Documents")
getwd()
setwd("/Users/nib21006/Dropbox/UConn/Teaching/2022-R-Summer-Workshop/Day-1")

################################################################################
                            # Get help within R #
################################################################################
?install.packages # pulls up the documentation for the command "install.packages"
help(install.packages) # same as above
??mean # double question marks will search all packages (including base R) that contain the search term

################################################################################
                            # Creating objects #
################################################################################
# Numbers
x <- 8
y <- 17
class(x)

x
print(x)
y
z <- x + y
z
x*y

# Character strings
string <- "Hello world"
string
class(string)

name <- "Nina"

name + string # can't add strings!

paste(name, string) # but you can paste/concatenate strings
paste(name, string, sep = "####")

# Logical
x > y
y > x
logical_ob <- TRUE
logical_ob
class(logical_ob)

# Vectors: all elements of a vector must be the SAME type
vec <- c(1,6,3,9)
vec
class(vec)
vec[1] # access elements of the vector by position
vec[4]
vec[6] # why doesn't this work?
which(vec == 6) # tells you the position of the element that equals 6
vec >5 # returns a logical vector

string_vec <- c("this", "is", "a", "vector", "of", "strings")
string_vec
string_vec[4]
class(string_vec)

sequence <- seq(1:10)
sequence
?seq
another_sequence <- seq(from = -3, to = 50, by = 4)

# Lists: can contain different types of objects
new_list <- list("a", 5, c(1,2,3,4,5,6))
new_list
class(new_list)

new_list[[1]] # access elements of a list similarly, but typically use double brackets [[]]
new_list[[3]][3]

list_of_lists <- list(new_list, new_list)
list_of_lists
class(list_of_lists)

# Matrices
mat <- matrix(1:15, nrow = 3, ncol = 5, byrow = T)
mat

mat[1, 2] # access elements of matrix by row and column position

# Data frames
df <- data.frame(
    x = c(1,2), 
    y = 1:10, 
    z = LETTERS[1:10],
    rand = runif(10)
)

df

class(df)

df$x # access variables using the $ operator

df[["x"]] # can also access by name (enclosed in quotes & with brackets)

df[ , 1] # or by position (this says give me the 1st column of the df)

class(df$x)

class(df$z)

head(df) # prints out just the first 6 rows
tail(df) # prints out the last 6 rows

df2 <- data.frame(
    fac = letters[1:10],
    stringsAsFactors = T
)

df2$fac
class(df2)
class(df2$fac)
levels(df2$fac)

# Tibbles
# Tibbles are a unique feature of the tidyverse and operate like data frames
tb <- tibble(
    a = 1:15,
    b = 6:20,
    c = 1,
    z = (a + b)^2 + c
)

class(tb)

tb # notice how the first "thousands" place digit is underlined? tibbles have useful formatting elements like this

# command for dataframes also work on tibbles
head(tb) # prints out just the first 6 rows
tail(tb) # prints out the last 6 rows


################################################################################
                            # Reading in data #
################################################################################
# Excel (.xlsx)
excel_df <- read_excel("./data/Data_Extract_From_World_Development_Indicators.xlsx")
head(excel_df)
tail(excel_df) # can we get rid of that metadata at the bottom?

excel_df <- read_excel("./data/Data_Extract_From_World_Development_Indicators.xlsx",
                       n_max = max(which(excel_df$`Country Name` == "Zimbabwe")))

tail(excel_df) 

# Google Sheets
googlesheet_df <- read_sheet("https://docs.google.com/spreadsheets/d/1LgeaXvWdGqdCZSb7Pzuh7nmOJ32ZczzJ_G_F9hq6IPE/edit#gid=0")

# .csv
csv_df <- read_csv("./data/weather_data.csv")
write.csv(csv_df, file = "./data/new_data.csv")

# .txt
txt_df <- read_delim("./data/HAP0006.txt", delim = ",")

# Stata
stata_df <- read_dta("./data/lead_mortality.dta")
write_dta(stata_df, path = "./data/new_data.dta")

# Bonus data types --------------------------------------
# Tidycensus
# To use tidycensus, you must obtain a Census API key. 
# A key can be obtained from http://api.census.gov/data/key_signup.html.

# first you need to give tidycensus your API key to use the data
census_api_key("YOUR API KEY GOES HERE")

# you need to kno what variable you want to pull - if you don't, then you can search
vars <- load_variables(
    year = 2020, 
    dataset = "pl") # pl refers tot he 2020 census (you need to read the docs to find this out)

vars %>% print(n = 50)

# let's use the get_decennial() command to get some 2020 census data
race20 <- get_decennial(
    geography = "state", 
    
    # need to know what variable 
    variables = c("P1_001N", "P1_002N", "P1_003N", "P1_004N",
                  "P1_005N", "P1_006N", "P1_007N", "P1_008N"),
    year = 2020)

head(race20,n = 20)
tail(race20,n = 20)

# Web data 
starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")
films <- starwars %>% 
    html_elements("section")
films

title <- films %>% 
    html_element("h2") %>% 
    html_text2()
title

# If the page contains tabular data you can convert it directly to a dataframe 
# with html_table():
url <- read_html("https://en.wikipedia.org/wiki/List_of_French_Open_men%27s_singles_champions")

out <- html_nodes(url, "table") %>%
    .[5] %>%
    html_table(fill = TRUE)

# store as a tibble
french_open <- as_tibble(out[[1]],
                          .name_repair = "minimal") 

french_open

################################################################################
################################################################################
                        # Basic data frame operations # 
################################################################################
# read in the WDI data & skip those last rows of meta data after Zimbabwe
wdi <- read_excel("./data/Data_Extract_From_World_Development_Indicators.xlsx")
wdi <- read_excel("./data/Data_Extract_From_World_Development_Indicators.xlsx",
                  n_max = max(which(wdi$`Country Name` == "Zimbabwe")))

head(wdi) # print out first 10 rows of data
tail(wdi) # prints out last 10 rows of data

# 3 ways to see the variable names of your data frame
names(wdi)
ls(wdi) # not that ls prints them in alphabetical order, while the other 2 print them in the order they appear in the df
colnames(wdi)

# query basic info about your data frame
ncol(wdi) # number of columns
nrow(wdi) # number of rows
length(wdi) # length when applied to a df also returns # of columns
dim(wdi) # dimensions of the df: rows by columns

# get an overview of what's in the data frame
summary(wdi)
str(wdi)
glimpse(wdi) # the tidyverse version of str()


# specify a single variable from a data frame, use the dollar sign $ operator
# note this will print out all of the values
wdi$`Country Name` # note that variable names shouldn't have spaces in them; this is a "bad" variable name
wdi$`Series Name` # that's why they're enclosed in `back ticks`
wdi$`2010 [YR2010]`# variable names also shouldn't start with numbers






################################################################################
                      # Data "Wrangling" with the Tidyverse # 
################################################################################
# Note: all the packages you'll need are loaded at the top of the script 
#       this is good convention for script organization.

# Create "good" variable names
# these are equivalent -- but the first uses the pipe (%>%) syntax
wdi_clean <- wdi %>%
    clean_names()

wdi_clean_2 <- clean_names(wdi)

# compare them
names(wdi)
names(wdi_clean)
names(wdi_clean_2)

# mutate examples
wdi <- wdi %>%  # notice how i'm chaining the original wdi df to the one with clean_names
    clean_names() %>% # which is chained to the mutate 
    mutate(
        country_name = factor(country_name),
        x2010_yr2010 = as.numeric(x2010_yr2010)
    )

str(wdi$country_name)
str(wdi$x2010_yr2010) # numeric
str(wdi$x2011_yr2011) # still a character string

# mutate + across for multiple variables
wdi <- wdi %>%
    mutate(across(starts_with("x"), as.numeric)) 

# now all of them are numeric
str(wdi$x2010_yr2010)
str(wdi$x2011_yr2011)
str(wdi$x2021_yr2021)

# filter
india <- wdi %>%
    filter(
        country_name == "India"
    )

head(india)

# to do the same filter, but without pipes, you need to specify the df first
filter(wdi, country_name == "India")

# if you don't assign the filter operation to a new object, it will only print
# out what you've done, but it won't be saved

wdi %>% 
    # notice this time I didn't assign it to a new object? this won't be saved in the environment, only printed out
    filter(
        country_name == "India" | country_name =="Bangladesh"
    )

# mutate + filter
sa <- wdi %>%
    # this south asia variable follows the WB classification for South Asian countries
    mutate(
        south_asia = country_name %in% c("Afghanistan", "Bangladesh","Bhutan",
                                         "India", "Maldives", "Nepal", 
                                         "Pakistan", "Sri Lanka")
    ) %>%
    filter(
        south_asia # by default R takes the TRUE cases
    )

# tally/count/tab
sa %>%
    count(country_name)

sa %>%
    group_by(country_name) %>%
    tally()

sa %>%
    tab(country_name)

# select
wdi %>%
    select(country_name, series_name, starts_with("x"))

wdi %>%
    select(ends_with("0"))

# you can negate a select (ie., exclude these variables)
wdi %>%
    select(-c(country_code, series_code))


# putting it all together: mutate, filter, select
wdi %>% 
    mutate(
        south_asia = country_name %in% c("Afghanistan", "Bangladesh","Bhutan",
                                         "India", "Maldives", "Nepal", 
                                         "Pakistan", "Sri Lanka")
    ) %>%
    filter(
        south_asia # by default R takes the TRUE cases
    ) %>%
    select(country_name, series_name, starts_with("x"))


# rename
wdi <- wdi %>%
    clean_names() %>%
    select(-c(country_code, series_code)) %>%
    rename(
        country = country_name,
        series = series_name
    )

# reshaping (pivoting)

# pivot_longer (from wide to long)
wdi %>%
    pivot_longer(
        -c(country, series), 
        names_to = "year",
        values_to = "value") %>%
    head()

# use regular expressions to identify a pattern in the variable names
wdi_long <- wdi %>% # this time we'll save it as a new object
    pivot_longer(
        -c(country, series), 
        names_to = "year",
        names_pattern = "(\\d+)",
        names_transform = list(year = as.integer),
        values_to = "value") 

wdi_long %>%
    head()

# pivot_wider
# create a new variable to store the series in first that will work as a variable name
wdi_long <- wdi_long %>%
    mutate(
        series_short = case_when( # use case_when() for conditional operations
            str_detect(series, "electricity") ~ "electricity",
            str_detect(series, "(?=.*Education)(?=.*female)") ~ "edu_female",
            str_detect(series, "(?=.*Education)(?=.*male)") ~ "edu_male",
            str_detect(series, "GDP per capita") ~ "gdppc",
            str_detect(series, "malaria") ~ "malaria",
            str_detect(series, "Internet") ~ "internet",
            str_detect(series,  "(?=.*Labor)(?=.*female)") ~ "lfp_female",
            str_detect(series, "(?=.*Labor)(?=.*male)") ~ "lfp_male",
            str_detect(series, "Military") ~ "military",
            str_detect(series, "Poverty") ~ "poverty_gap",
            str_detect(series, "Prevalence") ~ "underourished"
        )
    ) 

wdi_long %>%
    tab(series, series_short)

# pivot_wider and save as a new df
wdi_df <- wdi_long %>%
    select(-series) %>%
    pivot_wider(
        names_from = "series_short",
        values_from = "value"
        ) %>%
    mutate_at(
        vars(electricity:gdppc),
        as.numeric
    )

wdi_df

# Grouping

# an ungrouped data frame:
head(wdi_df)

# a grouped data frame:
wdi_df %>%
    group_by(country)

# Use group_by with summarise for lots of operations

# For example: How many observations do we have per country? 
wdi_df %>%
    group_by(country) %>%
    summarise(n = n())

# How about average GDP for each country over the 12 years of data?
wdi_df %>%
    group_by(country) %>%
    summarise(
        n = n(),
        gdp_avg = mean(gdppc, na.rm = TRUE)) # make sure to remove NAs 

# Multiple statistics for multiple variables
wdi_df %>%
    group_by(country) %>%
    summarise_at(
        vars(gdppc, electricity),
        list(~mean(., na.rm = TRUE), ~median(., na.rm = TRUE))) 

# Note that you can also compute grouped statistics with mutate, but the structure
# of the dataframe that's returned will be different

wdi_df %>%
    group_by(country) %>%
    mutate(
        n = n(),
        gdp_avg = mean(gdppc, na.rm = TRUE)) %>%
    select(country, year, gdppc, n, gdp_avg)


# Identifying duplicates
## first create a dataframe to use to demonstrate the process
df <- tribble(
    ~x, ~y, ~z,
    "a", 2, 5,
    "a", 2, 5,
    "a", 0, 5,
    "b", 3, 9
)

df # view the data frame

## Filter to keep only distinct observations
df %>%
    distinct()

## View duplicates
df %>%
    group_by(x) %>%
    add_tally() # gives you how many observations per value of x

# use janitor:: get_dupes
df %>%
    get_dupes()

# can also check for duplicates of a specific variable
df %>%
    get_dupes(x)

# Identifying missing data
## first create a dataframe to use to demonstrate the process

df <- tribble(
    ~x, ~y, ~z,
    "a", 2, NA,
    "a", 2, 5,
    "a", NaN, 5,
    "b", 3, 9,
    "c", NA, 8,
    "", 1, 7
)

df # view the data frame

# count of missing (NA or NaN) observations by each variable 
df %>%
    summarise_all(list(~sum(is.na(.))))

summary(df)

# to see which rows of data had missing values
df %>% 
    filter(if_any(everything(), is.na))

# keep only non-missing observations
df %>% 
    drop_na


# to drop rows if they contain NAs only of one variable you can use a filter
df %>% 
    filter(!is.na(z)) # use ! to negate

wdi_df %>%
    group_by(country) %>%
    summarise(missing_electricity = sum(is.na(electricity)),
              n = n(),
              pct_missing = missing_electricity/n) %>%
    tail(n = 15)

# Binding data
# First, I'll create the LOTR data used in the slides.

fship <- tribble(
    ~Film,    ~Race, ~Female, ~Male,
    "The Fellowship Of The Ring",    "Elf",    1229,   971,
    "The Fellowship Of The Ring", "Hobbit",      14,  3644,
    "The Fellowship Of The Ring",    "Man",       0,  1995
)

rking <- tribble(
    ~Film,    ~Race, ~Female, ~Male,
    "The Return Of The King",    "Elf",     183,   510,
    "The Return Of The King", "Hobbit",       2,  2673,
    "The Return Of The King",    "Man",     268,  2459
)

ttow <- tribble(
    ~Film,    ~Race, ~Female, ~Male,
    "The Two Towers",    "Elf",     331,   513,
    "The Two Towers", "Hobbit",       0,  2463,
    "The Two Towers",    "Man",     401,  3589
)


# stores a new df that binds all 3
lotr <- bind_rows(
    fship, ttow, rking
)

lotr

# example of how row binds handle missing variables
ttow_no_Female <- ttow %>% 
    mutate(Female = NULL)

ttow_no_Female

# rbind (base R) can't handle the missing variable
rbind(fship, ttow_no_Female, rking)

# but bind_rows can!
bind_rows(fship, ttow_no_Female, rking)

## see if you can try a column bind with bind_cols!


# Joining data
# We'll use the wdi_df created above (after pivoting) & another data set that 
# we'll load

country_df <- read_csv("./data/country_df.csv")


# a left join
join_df <- left_join(wdi_df, country_df, by = "country")

head(join_df)

# now joining by country AND year
join_df <- left_join(wdi_df, country_df, by = c("country", "year"))

join_df

# compare the output if we do a right join instead
right_join_df <- right_join(wdi_df, country_df, by = c("country", "year"))

right_join_df

# or see what happens if you do a left join, but swap the order 
# be sure to look at all the variables!
swap_join_df <- left_join(country_df, wdi_df, by = c("country", "year"))

swap_join_df


