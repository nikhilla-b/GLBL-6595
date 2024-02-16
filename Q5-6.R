# Loading libraries ---------------------------------
library(tidyverse)
library(gt)
library(haven)
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
sector_standardized <- sector_labor_product - 3068.131

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


#Question 5:

structural_change_bangladesh <- structural_change |>
  filter((var == "VA_Q15" | var == "EMP") & country == "Bangladesh")

# Find the rows where value added (VA) and employment (EMP) data start and end
va_start <- which(structural_change_bangladesh$var == "VA_Q15")[1] 
va_end <- which(structural_change_bangladesh$var == "VA_Q15")[length(which(structural_change_bangladesh$var == "VA_Q15"))] 
emp_start <- which(structural_change_bangladesh$var == "EMP")[1] 
emp_end <- which(structural_change_bangladesh$var == "EMP")[length(which(structural_change_bangladesh$var == "EMP"))] 

# Create Columns for Each Sector
for (sector in sectors) {
  structural_change_bangladesh[paste(sector, "Labor_Productivity", sep = "_")] <- NA
}

# Getting Labour Productivity for Each Year
for (year_index in va_start:va_end) {
  year <- structural_change_bangladesh$year[year_index]
  for (sector in sectors) {
    # Calculating
    productivity <- structural_change_bangladesh[year_index, sector] /
      structural_change_bangladesh[year_index + (emp_start - va_start), sector] 
    # Putting in the column
    structural_change_bangladesh[year_index, paste(sector, "Labor_Productivity", sep = "_")] <- productivity
  }
}

#Calculating CV:

#Columns to calculate:
start_col <- 19 
end_col <- ncol(structural_change_bangladesh)

cv_values <- numeric()

# For Loop To Calculate For Each Sector
for (i in 1:29) {
  # Extract labor productivity values for the current row
  row_values <- unlist(structural_change_bangladesh[i, start_col:end_col])
  # Calculate CV
  cv <- sd(row_values) / mean(row_values)
  cv_values <- c(cv_values, cv)
}

#Creating Dataframe for Graph:
Years <- 1990:2018
graph_data <- data.frame(Years, cv_values)

#Graphing:
ggplot(data = graph_data, aes(x = Years, y = cv_values)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line
  labs(title = "Scatterplot of CV Values Over Time", x = "Year", y = "CV Value")

#Interpretation:
#Since variation in labour productivity in catching up to mean productivity 
#over time, this could mean that the less productive sectors are catching 
#up to the other sectors. This is a result of less variation in each sector's
#productivity, resulting in more stable output. This stable environment
#should allow progress.
#This could mean that Bangladesh looks more like advanced countries
#from our previous lectures than other LIC's, due to this reduced
#variation in sector productivity

#Question 6:
#________________
# using transformed data
structural_change_bangladesh <- structural_change_transform |>
  filter(year == 1990 | year == 2005 | year == 2018)

#Aggregating Sectors:
structural_change_bangladesh <- structural_change_bangladesh |>
  mutate(mfg_va = Mining_VA_Q15 + Manufacturing_VA_Q15 + Utilities_VA_Q15,
         services_va = Construction_VA_Q15 + Trade_VA_Q15 + Transport_VA_Q15 + 
           Business_VA_Q15 + Finance_VA_Q15 + Realestate_VA_Q15 + Government_VA_Q15 +
           Other_VA_Q15,
         mfg_emp = Mining_EMP + Manufacturing_EMP + Utilities_EMP,
         services_emp = Construction_EMP + Trade_EMP + Transport_EMP + Business_EMP +
           Finance_EMP + Realestate_EMP + Government_EMP + Other_EMP)


#Creating New Dataframe for further Questions:
productivity_decomp <- structural_change_bangladesh |>
  select(year, Agriculture_VA_Q15, Agriculture_EMP, mfg_va, mfg_emp,
         services_va, services_emp)

productivity_decomp$year <- as.numeric(productivity_decomp$year)
productivity_decomp <- productivity_decomp |>
  filter((year == "1990" | year == "2005" | year == "2018")) 

#productivity_decomp$VAR <- c(rep("VA", 3), rep("EMP", 3))


# Q7 ----------------------------------------------------------------------

#Making a data frame in a format convenient to do calculations
sector <- c("Agriculture", "Agriculture", "Agriculture", "Manufacturing","Manufacturing", "Manufacturing", "Services", "Services", "Services")
year <- c("1990", "2005", "2018","1990", "2005", "2018","1990", "2005", "2018")
emp <- c("26836213", "22984794", "25808861", "5289198", "5051255", "9735080", "8690267", "18437495", "29222352")
va <- c("12232852827", "18822526115", "30887840910", "5682913328", "16003341476", "51965884144", "31048064719", "65661485604", "142898353083")
df <- data.frame(sector, year, emp, va)

df$year <- as.character(df$year)
df$emp <- as.numeric(df$emp)
df$va <- as.numeric(df$va)

#Mutating necessary variables for calculations
df <- df |> 
  mutate(lp = va/emp)

df <- df |> 
  group_by(year) |> 
  mutate(L = sum(emp),
         emp_share = emp/L,
         )

df <- df |> 
  group_by(year) |> 
  mutate(VA = sum(va),
         LP = VA/L)

#For 1990-2005 across and within productivity decomposition

df_1 <- df |> 
  filter(year == "1990" |year == "2005") |>
  group_by(sector) |> 
  mutate(del_lp = case_when(year == "2005" ~ lp - lag(lp), TRUE ~ 0),
         del_LP = case_when(year == "2005" ~ LP - lag(LP), TRUE ~ 0),
         del_emp_share = case_when(year == "2005" ~ emp_share - lag(emp_share), TRUE ~ 0))

#within

df_1 <- df_1 |> 
  group_by(sector) |> 
  mutate(product = case_when(year == "2005" ~ lag(emp_share)*del_lp, TRUE ~ 0)) |> 
  ungroup() |> 
  mutate(numerator = sum(product),
         within = numerator/del_LP)

#Interpretation
#

#Question 8 - within sector decomposition

df_1 <- df_1 |> 
  group_by(sector) |> 
  mutate(within_sec_decomp = product/del_LP)

#across 

df_1 <- df_1 |> 
  group_by(sector, year) |> 
  mutate(product_2 = del_emp_share*lp) |> 
  ungroup() |> 
  mutate(numerator_2 = sum(product_2),
         across = numerator_2/del_LP)


#For 2005-2018 across and within productivity decomposition

df_2 <- df |> 
  filter(year == "2005" |year == "2018") |>
  group_by(sector) |> 
  mutate(del_lp = case_when(year == "2018" ~ lp - lag(lp), TRUE ~ 0),
         del_LP = case_when(year == "2018" ~ LP - lag(LP), TRUE ~ 0),
         del_emp_share = case_when(year == "2018" ~ emp_share - lag(emp_share), TRUE ~ 0))

#within

df_2 <- df_2 |> 
  group_by(sector) |> 
  mutate(product = case_when(year == "2018" ~ lag(emp_share)*del_lp, TRUE ~ 0)) |> 
  ungroup() |> 
  mutate(numerator = sum(product),
         within = numerator/del_LP)

#Question 8 - within sector decomposition

df_2 <- df_2 |> 
  group_by(sector) |> 
  mutate(within_sec_decomp = product/del_LP)

#across 

df_2 <- df_2 |> 
  group_by(sector, year) |> 
  mutate(product_2 = del_emp_share*lp) |> 
  ungroup() |> 
  mutate(numerator_2 = sum(product_2),
         across = numerator_2/del_LP)

#Time Series Graph for employment share

ggplot(data = df, aes(x = year, y = emp_share, group = sector)) +
  geom_line(aes(color = sector)) +
  labs(title = "Employment share in Bangladesh")

#Question 9 --------------------------------

#We reserve judgement for Bangladesh's prospects from these findings 
#of their path of structural development. Like the methodology in 
#Fan et al, we would be optimistic for Bangladesh if the growth
#in the services sector stemmed from producitivty growth and not demand.
#This would mean that sustained growth for Bangladesh is a possibility. Since
#we do not have this available, we reserve our judgement, but are 
#cautiously optimistic.
#Intuitively, because Bangladesh has not increased its share of employment
#in manufacturing, that is also cause for concern since virtually all rich
#nations today went through a manufacturing phase before tertirisation.
