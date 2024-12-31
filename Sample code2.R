### load the package

# install/load packages
pacman::p_load(
  stringr,    # many functions for handling strings
  tidyverse,  # for optional data manipulation
  tools)      # alternative for converting to title case

# import case linelist 
linelist <- import("linelist_cleaned.rds")

head(linelist, 20)

##COMBINE STRINGS

str_c("String1", "String2", "String3")

str_c("String1", "String2", "String3", sep = ", ")

### COMBINING FIRST NAME AND SURNAME
first_names <- c("abdul", "fahruk", "janice") 
last_names  <- c("hussein", "akinleye", "okeke")

# sep displays between the respective input strings, while collapse displays between the elements produced
str_c(first_names, last_names, sep = " ", collapse = ";  ")

# For newlines to print correctly, the phrase may need to be wrapped in cat()
cat(str_c(first_names, last_names, sep = " ", collapse = ";\n"))

### A simple example, of a dynamic plot caption:
str_glue("Data include {nrow(linelist)} cases and are current to {format(Sys.Date(), '%d %b %Y')}.")

### This can improve code readability if the text is long.

str_glue("Linelist as of {current_date}.\nLast case hospitalized on {last_hospital}.\n{n_missing_onset} cases are missing date of onset and not shown",
         current_date = format(Sys.Date(), '%d %b %Y'),
         last_hospital = format(as.Date(max(linelist$date_hospitalisation, na.rm=T)), '%d %b %Y'),
         n_missing_onset = nrow(linelist %>% filter(is.na(date_onset)))
)

### Pulling from a data frame

# make case data frame
case_table <- data.frame(
  zone        = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5"),
  new_cases   = c(3, 0, 7, 0, 15),
  total_cases = c(40, 4, 25, 10, 103)
)

### Use str_glue_data(), which is specially made for taking data from data frame rows:

case_table %>% 
  str_glue_data("{zone}: {new_cases} ({total_cases} total cases)")

### UNITE COLUMNS

df <- data.frame(
  case_ID = c(1:6),
  symptoms  = c("jaundice, fever, chills",     # patient 1
                "chills, aches, pains",        # patient 2 
                "fever",                       # patient 3
                "vomiting, diarrhoea",         # patient 4
                "bleeding from gums, fever",   # patient 5
                "rapid pulse, headache"),      # patient 6
  outcome = c("Recover", "Death", "Death", "Recover", "Recover", "Recover"))

df_split <- separate(df, symptoms, into = c("sym_1", "sym_2", "sym_3"), extra = "merge")


#### CLEAN AND STANDARDISE
### CHANGING CASE
str_to_upper("California") #Upper case
str_to_lower("California") #lower case
str_to_title("go to the US state of california ") #title case
tools::toTitleCase("This is the US state of california") #tools title case
str_to_sentence("the patient must be transported") # sentence case
