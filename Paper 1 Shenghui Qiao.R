## ----setup, include=FALSE-------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(opendatatoronto)
library(tidyverse)
library(knitr)
library(kableExtra)
library(dplyr)


## ----read_data, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------------
# get package
package <- show_package("059d37c6-d88b-42fb-b230-ec6a5ec74c24")

# get all resources for this package
resources <- list_package_resources("059d37c6-d88b-42fb-b230-ec6a5ec74c24")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
child_care_raw <- filter(datastore_resources, row_number()==1) %>% get_resource()


## ----cleandata, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------------
# Cleaning Data
child_care <- child_care_raw %>% 
  transmute("ID" = seq(1, nrow(child_care_raw), by = 1),
            LOC_NAME,
            LOC_ID,
            AUSPICE,
            "BUILDING_TYPE"= bldg_type,
            "INFANT_SPACE" = IGSPACE,
            "TODDLER_SPACE" = TGSPACE,
            "PRESCHOOLER_SPACE" = PGSPACE,
            "KINDERGARTEN_SPACE" = KGSPACE,
            "ELEMENTARY_SPACE" = SGSPACE,
            "TOTAL_SPACE" = TOTSPACE,
            "SUBSIDY" = subsidy) 


## ----dataextract, echo = FALSE, warning=FALSE-----------------------------------------------------------------------------
knitr::kable(head(child_care), caption = "Cleaned Data of Licensed Child Care Centers", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"))


## ----echo = FALSE---------------------------------------------------------------------------------------------------------
# Categorize Child Care Centers by Operating Auspices
non_profit <- child_care %>%
  filter(AUSPICE == 'Non Profit Agency')
commercial <- child_care %>%
  filter(AUSPICE == 'Commercial Agency')
public <- child_care %>%
  filter(AUSPICE == 'Public (City Operated) Agency')


## ----fig.cap = "\\label{fig:totalspace}Total Number of Child Care Spaces Across Operating Auspices", echo=FALSE, warning=FALSE, message=FALSE----
child_care %>% 
  ggplot(aes(x = TOTAL_SPACE, fill = AUSPICE)) +
  geom_histogram(colour = "#e9ecef", alpha = 0.7) +
  labs(x = "Total Number of Child Care Spaces",
       y = "")


## ---- fig.cap = "\\label{fig:subtype}Number of Children Care Spaces for Different Age Groups", echo=FALSE, warning=FALSE, message=FALSE----

np <- c(sum(non_profit$INFANT_SPACE), sum(non_profit$TODDLER_SPACE), sum(non_profit$PRESCHOOLER_SPACE), sum(non_profit$KINDERGARTEN_SPACE), sum(non_profit$ELEMENTARY_SPACE))

com <- c(sum(commercial$INFANT_SPACE), sum(commercial$TODDLER_SPACE), sum(commercial$PRESCHOOLER_SPACE), sum(commercial$KINDERGARTEN_SPACE), sum(commercial$ELEMENTARY_SPACE))

pub <- c(sum(public$INFANT_SPACE), sum(public$TODDLER_SPACE), sum(public$PRESCHOOLER_SPACE), sum(public$KINDERGARTEN_SPACE), sum(public$ELEMENTARY_SPACE))

data_ggp <- data.frame(x = c("Infant", "Toddler", "Preschooler", "Kindergarten", "Elementary+"),
                       y = c(np, com, pub),
                       AUSPICE = c(rep("Non Profit Agency",5) , rep("Commercial Agency",5), rep("Public (City Operated) Agency", 5)))

data_ggp %>% 
  ggplot(aes(x, y, colour = AUSPICE)) +
  geom_point(size = 3) +
  labs(x = 'Type of Children Age Groups',
       y = 'Number of Spaces')


## ----average, echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------------------

average <- data.frame("Age Group" = c("Infant", "Toddler", "Preschooler", "Kindergarten", "Elementary+"),
                      "Non Profit" = np/nrow(non_profit),
                      "Commercial"= com/nrow(commercial), 
                      "Public" = pub/nrow(public))

knitr::kable(average, caption = "Table 2. Mean Spaces for Each Age Group, Across Auspice", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))


## ---- fig.cap = "\\label{fig:subsidy}Non-Profit Agency Offers the Most Child Care Subsidy to Parents", fig.height = 4, echo=FALSE, warning=FALSE, message=FALSE----
subsidy_auspice <- child_care %>% 
  group_by(AUSPICE)

subsidy_auspice %>% 
  ggplot(aes(x = AUSPICE, fill = SUBSIDY)) +
  geom_histogram(binwidth = 1, stat = "count") +
  labs(x = "Operating Auspices",
       y = "Subsidies")



## ---- fig.cap = "\\label{fig:typebuilding}Types of Building that Each Operating Auspice Tends to be Located", fig.height=4, echo=FALSE, warning=FALSE, message=FALSE----
child_care %>% 
  ggplot(aes(x = BUILDING_TYPE, fill = AUSPICE)) +
  geom_bar(stat = "count") + 
  theme(axis.text.x = element_text(angle = 90, size = 3)) +
  labs(x = "Building Types",
       y = "Count")

