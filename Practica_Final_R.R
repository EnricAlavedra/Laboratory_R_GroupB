# Group B: Laboratory with R

# ------------------------------------------------------------------------------
# Exercise 1

# Section a,b,c:

# The first step is to load the dataset. Two options here: the first one, the used one, is by the use of the command read_csv() where manually, in a table, I'm allowed to set the type of variable is in each column and I do have a previous visualization of the output. The second option, commented below, is by the use of the library Hmisc.
library(readr)
Greenhouse_Gas_Emissions <- read_csv("owid-co2-data.csv",
                          col_types = cols(country = col_character(),
                          year = col_integer(), population = col_integer(),
                          gdp = col_integer()))
#library(Hmisc)
#Greenhouse_Gas_Emissions <- csv.get("owid-co2-data.csv", sep = ",")

# The second step is to label all the variables in the dataset using the information given in the sheet Metadata (description and unit)
# The process here will be the following: we have an already prepared dataframe with the description and the units for each variable from the dataset. The merge is easy between both dataframes as index are the same.
# To make all the labels without going one by one, using a for loop it is an optimal way: for each variable, label it with the description and the unit (in the case I have it)
library(Hmisc)
library(openxlsx)
Metadata <- read.xlsx("owid-co2-data.xlsx", 2)
description <- Metadata[, 3]
unit <- Metadata[, 4]
variableName <- names(Greenhouse_Gas_Emissions)

for (i in 1:length(variableName))
{
  if (!is.na(unit[i])) {
    unit[i] <- unit[i]
  } else {
    unit[i] <- "No information"
  }
  print(label(Greenhouse_Gas_Emissions[, i]) <- paste("DESCRIPTION: ",description[i], " UNIT: ", unit[i]))
}
# Dimensions of the dataset
dimGreenhouseGasEmissions <- dim(Greenhouse_Gas_Emissions)

# Section d:
# The statement says: Represent graphically co2, methane and nitrous_oxide together by country and year. Can be possible all countries or a selection of a country, according to the request made.
# So, according to the statement, we are proposing a function where the input parameter will be a list of countries, and the output will be a graph, for each country, with the representation of the three concentration by year

EmissionsEvolution <- function(pais)
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  countryFiltration <- Greenhouse_Gas_Emissions %>%
  filter(country == pais) %>%
  select(country, year, co2, methane, nitrous_oxide)
  data <- pivot_longer(
    countryFiltration,
    cols = c("co2", "methane", "nitrous_oxide"),
    names_to = "Variable",
    values_to = "Value"
  )
  finalPlot <- ggplot(data, aes(x = year, y = Value, color = Variable, linetype = Variable)) +
    geom_line() +
    labs(title = paste("Emssions evolution for ", pais))
  return(finalPlot)
}

countries <- c("Africa", "Spain")
for (i in 1:length(countries))
{
  pais <- countries[i]
  print(EmissionsEvolution(pais))
}

# Section e:
# In this section, we are creating a function that needs as an input the number of decades (starting from today) do I want. The function makes a subset of the datframe only selecting observations from those decades
decadeReduction <- function(decades)
{
  library(dplyr)
  startYear <- min(Greenhouse_Gas_Emissions$year)
  Greenhouse_Gas_Emissions$decade <- with(Greenhouse_Gas_Emissions, floor((Greenhouse_Gas_Emissions$year - startYear)/10))
  maxDecade <- max(Greenhouse_Gas_Emissions$decade)
  decadeSelection <- subset(Greenhouse_Gas_Emissions, decade > maxDecade - decades -1 & decade < maxDecade)
}

lastDecades <- decadeReduction(3)

# Section f:
library(dplyr)
library(tidyr)
summariseTable <- select(lastDecades, 1, 2, seq(6, length(names(lastDecades)), by = 1)) # The first step in this section is to reduce the dataset to get only the variables I need: all the quantitative and the ones for grouping by
summariseTable <- na.omit(summariseTable) # The second step is to remove those observations with NA, as they can bring problems
groupedByTable <- summariseTable %>% group_by(decade, country) %>% summarise_all(.funs = c("mean", "median", "sd", "IQR")) # Finally, apply the statistical analysis to each variable grouped by
groupedByTable <- groupedByTable[, -3]
# Section g:
# Solving the first part of the section: see the decade variations of the average co2,methane and nitrous_oxide in the total countries
library(dplyr)
library(tidyr)
library(ggplot2)
startYear <- min(Greenhouse_Gas_Emissions$year)
Greenhouse_Gas_Emissions$decade <- with(Greenhouse_Gas_Emissions, floor((Greenhouse_Gas_Emissions$year - 1750)/10))
summariseTable <- select(Greenhouse_Gas_Emissions, country, year, co2, methane, nitrous_oxide, decade)
summariseTable <- na.omit(summariseTable)
decadeVariationAll <- summariseTable %>% group_by(decade) %>% summarise_at(.vars = c("co2", "methane", "nitrous_oxide"),
                                                                           .funs = c("mean"))
data <- pivot_longer(
  decadeVariationAll,
  cols = c("co2", "methane", "nitrous_oxide"),
  names_to = "Variable",
  values_to = "Value"
)
ggplot(data, aes(x = decade, y = Value, color = Variable, linetype = Variable)) +
  geom_line() +
  labs(title = "Emissions evolution through decades")

# Solving the second part of the section: create a function that has as an output the emissions evolution for each country and as an input a list of countries
library(dplyr)
library(tidyr)
library(ggplot2)
startYear <- min(Greenhouse_Gas_Emissions$year)
Greenhouse_Gas_Emissions$decade <- with(Greenhouse_Gas_Emissions, floor((Greenhouse_Gas_Emissions$year - 1750)/10))
summariseTableCountry <- select(Greenhouse_Gas_Emissions, country, year, co2, methane, nitrous_oxide, decade)
summariseTableCountry <- na.omit(summariseTable)
EmissionsProgressByCountry <- function(country) {
  countryObs <- summariseTableCountry[which(summariseTableCountry$country == country), ]
  decadeVariationCountry <- countryObs %>% group_by(decade) %>% summarise_at(.vars = c("co2", "methane", "nitrous_oxide"),
                                                                             .funs = c("mean"))
  data <- pivot_longer(
    decadeVariationCountry,
    cols = c("co2", "methane", "nitrous_oxide"),
    names_to = "Variable",
    values_to = "Value"
  )
  finalPlot <- ggplot(data, aes(x = decade, y = Value, color = Variable, linetype = Variable)) +
    geom_line() +
    labs(title = paste("Emissions evolution through decades in ", country))
  return(finalPlot)
}

countries <- c("China", "France")
for (i in 1:length(countries))
{
  pais <- countries[i]
  print(EmissionsProgressByCountry(pais))
}

# Section h:
# In this section the aim is to create a function with an Excel sheet as an output. In this sheet, a dataframe with different statistics for all quantitative variables filtered by decade and country should be added
# We think that this section is about doing section f, but filtered by decade and country and export this data frame as a new Excel sheet
# In order to make it a little bit complex and complete, different parameters will be allowed: number of decades (starting from last) are required to consider and which statistical calculations are required

statCalculations <- function(data, statFunc)
{
  library(dplyr)
  library(tidyr)
  summariseTable <- select(data,1, 2, seq(6, length(names(lastDecades)), by = 1))
  summariseTable <- na.omit(summariseTable) # The second step is to remove those observations with NA, as they can bring problems
  groupedByTable <- summariseTable %>% group_by(decade, country) %>% summarise_all(.funs = statFunc)
  return(groupedByTable)
}

dataframeExportation <- function(decades, statFunc)
{
  library(writexl)
  # The first step is to reduce the dataframe to only consider the observations in the required decades
  dataDecadesReduction <- decadeReduction(decades)
  # The next step is to calculate the demanded statistical metrics for each variable
  statCalculations(dataDecadesReduction, statFunc)
  # Finally, export the data set as an Excel sheet
  write.xlsx(groupedByTable, file = "agg_co2.xlsx")
}

decades <- 3
statFunc <- c("mean", "median")
dataframeExportation(decades, statFunc)

# ------------------------------------------------------------------------------
# Exercise 2:

# Graph 1: Use the library plotrix to visualize a Pyramid Chart.
# Visualization, by country and using a Pyramid chart, the evolution of methane and nitrous_oxide emissions per decade
PyramidPlotCountry <- function(decades, countri)
{
  library(dplyr)
  library(tidyr)
  library(plotrix)
  # The first step is to reduce the dataset and prepare it for working: reduce the number of decades to consider and the countries considered. Then, decades have to be indexed from 1 until the maximum
  maxDecade <- max(Greenhouse_Gas_Emissions$decade)
  dataReduction <- subset(Greenhouse_Gas_Emissions, country == countri)
  decadeSelection <- subset(dataReduction, decade > maxDecade - decades -1 & decade < maxDecade)
  decadeReducedCol <- select(decadeSelection,1,80,44,46)
  decadeMean <- decadeReducedCol %>% group_by(country, decade) %>% summarise_all(.funs = "sum")
  decadeMean$decade <- with(decadeMean, 1:dim(decadeMean)[1])
  # The second step is to make the pyramid plot and return the plot
  PyramidPlot <- pyramid.plot(decadeMean$methane,decadeMean$nitrous_oxide, decadeMean$decade, main = paste("Methane vs Nitrous Oxide: ", country), top.labels = c("Methane", "Decade", "Nitrous Oxide"), lxcol = "blue", rxcol = "red", unit = "million tonnes", ppmar=c(4,1,4,1), space = 0.3, gap = 1, labelcex = 1)
  print(PyramidPlot)
}
decades <- 15
country <- "Spain"
PyramidPlotCountry(decades, country)

# Graph 2: Use the library gplots to visualize a Stacked Bar Plot
# For a given decade and giving several countries, see total gas emissions
StackedBarPlot <- function(countries, decada)
{
  library(dplyr)
  library(tidyr)
  library(gplots)
  data <- Greenhouse_Gas_Emissions
  dataCountries <- data[data$country %in% countries, ]
  dataDecade <- subset(dataCountries, decade == decada)
  dataReduction <- select(dataDecade,1,8,44,46)
  groupBydata <- dataReduction %>% group_by(country) %>% summarise_all(.funs = "sum")
  dataValues <- as.matrix(groupBydata[, -1])
  paises <- as.vector(groupBydata$country)
  StackedBarPlot <- barplot2(
    dataValues,
    names.arg = paises,
    beside = FALSE,
    legend = colnames(dataValues),
    main = "Total Emissions by Country giving a specific decade",
    xlab = "Country",
    ylab = "Emissions"
  )
  print(StackedBarPlot)
}
countries <- c("Spain", "France", "Italy")
StackedBarPlot(countries, 25)

# ------------------------------------------------------------------------------
# Exercise 3

# Section a:
# The structure of this section will be the following: the first step is just considering one country, calculate the increase of emissions given the amount of decades. Then, make a loop for all the countries given
library(dplyr)
library(tidyr)
dataReducedDecadeCountry <- function(decades, countri)
{
  library(dplyr)
  library(tidyr)
  # The first step is the creation of a dataframe to bring all data of increasing emissions
  dfPercDecade <- data.frame(
    country = character(),
    co2_increase = numeric(),
    methane_increase = numeric(),
    nitrous_increase = numeric()
  )
  # Then, creating the new column in the main dataset bringing information about decade
  startYear <- min(Greenhouse_Gas_Emissions$year)
  Greenhouse_Gas_Emissions$decade <- with(Greenhouse_Gas_Emissions, floor((Greenhouse_Gas_Emissions$year - startYear)/10))
  maxDecade <- max(Greenhouse_Gas_Emissions$decade)
  decadeSelection <- subset(Greenhouse_Gas_Emissions, decade > maxDecade - decades -1 & decade < maxDecade)
  # Only the selection of the columns interested: emissions of co2, methane and nitrous_oxide
  decadeReducedCol <- select(decadeSelection,1,80,8,44,46)
  # Groupby by country and decade and only calculating the mean
  groupedBy <- decadeReducedCol %>% group_by(decade, country) %>% summarise_all(.funs = "mean") # Finally, apply the statistical analysis to each variable grouped by
  # Once all the calculations are done, only select those where the country is the one given
  groupedByCountry <- subset(groupedBy, country == countri)
  # The second step is the calculation of the increase percentage
  for (i in 1:length(groupedByCountry))
  {
    co2_ini = as.numeric(groupedByCountry[i,3])
    co2_fin = as.numeric(groupedByCountry[i+1,3])
    co2_perc = ((co2_fin - co2_ini)/co2_ini)*100
    methane_ini = as.numeric(groupedByCountry[i,4])
    methane_fin = as.numeric(groupedByCountry[i+1,4])
    methane_perc = ((methane_fin - methane_ini)/methane_ini)*100
    nitro_ini = as.numeric(groupedByCountry[i,5])
    nitro_fin = as.numeric(groupedByCountry[i+1,5])
    nitro_perc = ((nitro_fin - nitro_ini)/nitro_ini)*100
    # Add the new variables to the created dataframe
    dfPercDecade <- dfPercDecade %>% add_row(country = as.character(groupedByCountry[i,2]), co2_increase = co2_perc, methane_increase = methane_perc, nitrous_increase = nitro_perc)
  }
  return(dfPercDecade)
}
# The second step in the process is extrapolating the previous function to all the given countries
dataReducedDecadeALL <- function(decades, countries)
{
  dfPerc<- data.frame(
    country = character(),
    co2_increase = numeric(),
    methane_increase = numeric(),
    nitrous_increase = numeric()
  )

  for (i in 1:length(countries))
  {
    countri <- countries[i]
    dataReducedCountry <- dataReducedDecadeCountry(decades, countri)
    dfPerc <- rbind(dfPerc, dataReducedCountry)
  }
  return(dfPerc)
}

countries <- c("Spain", "France", "Germany")
decades <- 6
dataDecades <- dataReducedDecadeALL(decades,countries)

# Section b:
# The first step in this section is to apply the function dataReducedDecadeCountry() given the number of decades and a country.
# But then, it is necessary to add a new column indexating the decades with a number
PredictEmissions <- function(decades, countri)
{
  library(dplyr)
  library(tidyr)
  # The first step is to reduce the dataset and prepare it for working: reduce the number of decades to consider and the countries considered. Then, decades have to be indexed from 1 until the maximum
  maxDecade <- max(Greenhouse_Gas_Emissions$decade)
  dataReduction <- subset(Greenhouse_Gas_Emissions, country == countri)
  decadeSelection <- subset(dataReduction, decade > maxDecade - decades -1 & decade < maxDecade)
  decadeReducedCol <- select(decadeSelection,1,80,8,44,46)
  decadeMean <- decadeReducedCol %>% group_by(decade, country) %>% summarise_all(.funs = "sum")
  decadeMean$decade <- with(decadeMean, 1:dim(decadeMean)[1])
  # The second step is to make a linear regression model
  lm_co2 <- lm(co2 ~ decade, data = decadeMean)
  lm_methane <- lm(methane ~ decade, data = decadeMean)
  lm_nitrous_oxide <- lm(nitrous_oxide ~ decade, data = decadeMean)
  # The third step is creating a new row where predictions will be left
  nextDecade <- data.frame(decade = max(decadeMean$decade) + 1)
  # Time for predictions
  newPredictions <- data.frame(
    decade = nextDecade$decade,
    country = countri,
    co2 = predict(lm_co2, newdata = nextDecade),
    methane = predict(lm_methane, newdata = nextDecade),
    nitrous_oxide = predict(lm_nitrous_oxide, newdata = nextDecade)
  )
  decadesMean <- rbind(decadeMean, newPredictions)
  return(decadesMean)
}
PredictEmissionsALL <- function(decades, countries)
{
  decadesMeanFinal<- data.frame(
    decade = numeric(),
    country = character(),
    co2 = numeric(),
    methane = numeric(),
    nitrous_oxide = numeric()
  )

  for (i in 1:length(countries))
  {
    countri <- as.character(countries[i])
    decadesMean <- PredictEmissions(decades, countri)
    decadesMeanFinal <- rbind(decadesMeanFinal, decadesMean)
  }
  return(decadesMeanFinal)
}
decades <- 6
country <- c("France", "Spain", "Italy")
countriesPrediction <- PredictEmissionsALL(decades, country)

# Section c:

# Results comment: To comment the results, I'm going to test many different countries and compare the last decade real values and the prediction
# In order to make the prediction as accurate as possible, I'm going to consider much more decades.
# I'm also going to consider countries from many different parts of the world, in that way I will be able to clustered by regions
PredictEmissionsSTAT <- function(decades, countries)
{
  decadesMeanFinal<- data.frame(
    decade = numeric(),
    country = character(),
    co2 = numeric(),
    methane = numeric(),
    nitrous_oxide = numeric()
  )

  for (i in 1:length(countries))
  {
    countri <- as.character(countries[i])
    decadesMean <- PredictEmissions(decades, countri)
    decadesMeanFinal <- rbind(decadesMeanFinal, decadesMean)
  }
  decadesMeanFinal <- subset(decadesMeanFinal, decade >= decades)
  decadeMeanFinalStat <- decadesMeanFinal %>% group_by(country, decade) %>% summarise_at(.vars = c("co2", "methane", "nitrous_oxide"),
                                                                                .funs = c("mean"))
  return(decadeMeanFinalStat)
}


decades <- 15
countries <- c("France", "Spain", "Italy", "Portugal", "Greece")
SouthEurope <- PredictEmissionsSTAT(decades, countries)
countries <- c("Norway", "Netherlands", "Sweden", "Finland", "Denmark")
NorthEurope <- PredictEmissionsSTAT(decades, countries)
countries <- c("Gambia", "Cameroon", "Congo", "Senegal", "Tanzania")
Africa <- PredictEmissionsSTAT(decades, countries)
countries <- c("Argentina", "Chile", "Mexico", "Uruguay", "Peru")
SouthAmerica <- PredictEmissionsSTAT(decades, countries)
countries <- c("India", "China")
Asia <- PredictEmissionsSTAT(decades, countries)
countries <- c("Vietnam", "Philippines", "Thailand")
EastAsia <- PredictEmissionsSTAT(decades, countries)






