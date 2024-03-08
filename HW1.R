# Loading libraries ---------------------------------
library(tidyverse)
library(gt)
library(haven)
library(urbnmapr)
library(sf)
library(glue)
library(modelsummary)
library(fixest)
library(fastDummies)
library(ggplot2)



# Loading data ---------------------------
structural_change <- readRDS("ETD_230918.RDS")

#structural_change |> colnames()

# functions ----------------------------
perform_division <- function(prefix, your_data) {
  # Column names with "_VA_Q15" suffix
  col_va <- paste0(prefix, "_VA_Q15")
  # Column names with "_EMP" suffix
  col_emp <- paste0(prefix, "_EMP")
  # Perform division
  if (col_va %in% colnames(your_data) & col_emp %in% colnames(your_data)) {
    labor_product <- your_data[[col_va]] / your_data[[col_emp]]
    return(labor_product)
  }
}


# converting variables -----------------------
structural_change_transform <- structural_change |>
  filter((var == "VA_Q15" | var == "EMP") & country == "Bangladesh") |>
  pivot_wider(names_from = var, values_from = c(Agriculture, Mining, 
                                                Manufacturing, Utilities, Construction, 
                                                Trade, Transport, Business, Finance, 
                                                Realestate, Government, Other, Total, 
                                                Warflag)) |>
  mutate(across(ends_with("VA_Q15"), ~ (.x * 1000000 /80))) |>
  mutate(across(ends_with("EMP"), ~ .x * 1000))


# Question 1 ---------------------------
# total value added per worker in 2015
structural_change_transform |>
  filter(year == 2015) |>
  select(c(Total_VA_Q15, Total_EMP)) |>
  mutate(total_per_worker = Total_VA_Q15/Total_EMP)

# the reason for this discrepancy might be because the World Bank only takes into account
# trade-able goods in markets
# However, a lot of agricultural, home produced or locally owned production might
# not be counted in that, which in turn might underestimate the total value
# added per workers in the country

# Question 2 ---------------------
structure_2015 <- structural_change_transform |>
  filter(year == 2015) 

sectors <- list("Agriculture", "Mining", "Manufacturing", "Utilities",
                "Construction", "Trade", "Transport", "Business",
                "Finance", "Realestate", "Government", "Other") 


# Loop through sectors and perform division
sector_labor_product = list()
for (sector in sectors) {
  labor_prod = perform_division(sector, structure_2015)
  sector_labor_product = append(sector_labor_product, labor_prod)
}

# Convert sector_labor_product to numeric if it's not already numeric
if (!is.numeric(sector_labor_product)) {
  sector_labor_product <- as.numeric(sector_labor_product)
}

# Perform subtraction
sector_standardized <- sector_labor_product / 3068.131

# naming the standardized values
names(sector_standardized) <- sectors

# Normalized productivity of agriculture is -1983.73
# Employment share of agriculture is 43.2% (code for this in question 4)


# Question 3 ---------------------
# Loop through sectors to get employment

#total employment in 2015
total_emp = structure_2015$Total_EMP

# creating employment list
employment = list()

structure_2015_emp <- structure_2015 |>
  select(ends_with("_EMP")) |>
  subset(select = -c(Total_EMP, Warflag_EMP))
for (col in names(structure_2015)) {
  # Append values of each column to the list
  employment[[col]] <- structure_2015_emp[[col]]
}

names(employment) <- sectors

# combining my lists of employment 
keys <- unique(c(names(sector_standardized), names(employment)))
combined_prod <- setNames(mapply(c, sector_standardized[keys], employment[keys]), keys)

# changing to dataframe
structure_2015_long <- as.data.frame(combined_prod) |>
  pivot_longer(cols = Agriculture:Other,
               names_to = "Sector",
               values_to = "labor_prod")

# getting employment section
employment <- structure_2015_long[13:24,]

# dropping employment section from combined
structure_2015_long <- structure_2015_long[-c(13:24), ]

# column binding
structure_2015_long <- bind_cols(structure_2015_long, employment) |>
  rename(
    Sector = Sector...1,
    Sector_prod_stand = labor_prod...2,
    Employment = labor_prod...4) |>
  subset(select = -c(Sector...3)) |>
  arrange(Sector_prod_stand) |>
  mutate(cummulative_employment = cumsum(Employment)/sum(Employment)) |>
  arrange(Sector_prod_stand) 

# plotting figure
structure_2015_long |>
  ggplot( aes(x = cummulative_employment, y = Sector_prod_stand, label = Sector)) +
  geom_step() +
  geom_label() +
  geom_point() +
  expand_limits(x = 0, y = 0) +
  labs(y = "Labor Productivity Relative to Total Labor Productivtiy \n (Per Capita)", 
       x = "Cummulative employment")

# This figure shows us that the least productive sector, Agriculture, actually had the largest
# labor force share in Bangladesh in 2015.
# On the other hand, extremely productive sectors, such as Real Estate, Mining,
# Finance, and Business have minuscule shares of the labor force
# The combination of these phenomenons can drag down the overall labor productivity


# Question 4 ---------------------
# naming the non standardized values
# naming the standardized values
names(sector_labor_product) <- sectors

# order the sectors by order of production
sector_labor_product <- sort(sector_labor_product)

# Combining the non standardized values with employment 
structure_2015_long <- structure_2015_long |>
  mutate(Sector_prod_non_stand = sector_labor_product,
         Employment_share = Employment/sum(Employment),
         weighted_sector_prod = Sector_prod_non_stand*Employment_share)

# weighted sum of productivity of sectors
weighted_prod <- sum(structure_2015_long$weighted_sector_prod)

# flipping employment shares of manufacturing and agriculture
# first getting the values
agriculture_share <- structure_2015_long$Employment_share[1]
manufacturing_share <- structure_2015_long$Employment_share[3]

# now flipping
structure_2015_long$Employment_share[1] <- manufacturing_share
structure_2015_long$Employment_share[3] <- agriculture_share

# calculating the new weighted labor productivity
flipped_prod <- sum(structure_2015_long$Sector_prod_non_stand*structure_2015_long$Employment_share)

# percentage increase
percent_increase <- ((flipped_prod - weighted_prod)/weighted_prod) * 100



