library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(corrplot)

# Load csv to the dataframe
co2_data <- read.csv("Data/CO2 Emissions.csv", stringsAsFactors = FALSE)

#remove empty rows
co2_data <- co2_data[co2_data$API != "", ]

#remove unnecessary columns
co2_data <- co2_data[, -c(3:12,44:46)]

#CO2 Data Integration
co2_data_Tot_Emission <- co2_data[co2_data$X == "        CO2 emissions (MMtonnes CO2)", ]
co2_data_Coal_Coke <- co2_data[co2_data$X == "            Coal and coke (MMtonnes CO2)", ]
co2_data_Con_NatGas <- co2_data[co2_data$X == "            Consumed natural gas (MMtonnes CO2)", ]
co2_data_Petrol <- co2_data[co2_data$X == "            Petroleum and other liquids (MMtonnes CO2)", ]


# Add a new column 'Country' (see count(countries).R))
country <- read.csv("Data/Countries.csv", stringsAsFactors = FALSE)
co2_data_Tot_Emission$Country <- country$x
co2_data_Coal_Coke$Country <- country$x
co2_data_Con_NatGas$Country <- country$x
co2_data_Petrol$Country <- country$x

# Move 'Country' to the first column
co2_data_Tot_Emission <- co2_data_Tot_Emission[, c("Country", names(co2_data_Tot_Emission)[-length(names(co2_data_Tot_Emission))])]
co2_data_Coal_Coke <- co2_data_Coal_Coke[, c("Country", names(co2_data_Coal_Coke)[-length(names(co2_data_Coal_Coke))])]
co2_data_Con_NatGas <- co2_data_Con_NatGas[, c("Country", names(co2_data_Con_NatGas)[-length(names(co2_data_Con_NatGas))])]
co2_data_Petrol <- co2_data_Petrol[, c("Country", names(co2_data_Petrol)[-length(names(co2_data_Petrol))])]

#wide to long format
co2_data_Tot_Emission <- melt(co2_data_Tot_Emission, id.vars = c("API", "X", "Country"), 
                      variable.name = "Year", value.name = "CO2 Emissions (Total) - MMtonnes")
co2_data_Coal_Coke <- melt(co2_data_Coal_Coke, id.vars = c("API", "X", "Country"), 
                      variable.name = "Year", value.name = "CO2 Emissions (Coal&Coke) - MMtonnes")
co2_data_Con_NatGas <- melt(co2_data_Con_NatGas, id.vars = c("API", "X", "Country"), 
                      variable.name = "Year", value.name = "CO2 Emissions (Natural Gas) - MMtonnes")
co2_data_Petrol <- melt(co2_data_Petrol, id.vars = c("API", "X", "Country"), 
                      variable.name = "Year", value.name = "CO2 Emissions (Petroleum) - MMtonnes")

# Remove "X" in Year 
co2_data_Tot_Emission$Year <- gsub("X", "", co2_data_Tot_Emission$Year)
co2_data_Coal_Coke$Year <- gsub("X", "", co2_data_Coal_Coke$Year)
co2_data_Con_NatGas$Year <- gsub("X", "", co2_data_Con_NatGas$Year)
co2_data_Petrol$Year <- gsub("X", "", co2_data_Petrol$Year)

# Remove X Column
co2_data_Tot_Emission <- co2_data_Tot_Emission %>% select(-X)
co2_data_Coal_Coke <- co2_data_Coal_Coke %>% select(-X)
co2_data_Con_NatGas <- co2_data_Con_NatGas %>% select(-X)
co2_data_Petrol <- co2_data_Petrol %>% select(-X)

# Remove API Column
co2_data_Tot_Emission <- co2_data_Tot_Emission %>% select(-API)
co2_data_Coal_Coke <- co2_data_Coal_Coke %>% select(-API)
co2_data_Con_NatGas <- co2_data_Con_NatGas %>% select(-API)
co2_data_Petrol <- co2_data_Petrol %>% select(-API)

# Merge all the dataframe
Fin_Co2_df <- merge(co2_data_Tot_Emission, co2_data_Coal_Coke, by = c("Year", "Country"))
Fin_Co2_df <- merge(Fin_Co2_df, co2_data_Con_NatGas, by = c("Year", "Country"))
Fin_Co2_df <- merge(Fin_Co2_df, co2_data_Petrol, by = c("Year", "Country"))

write.csv(Fin_Co2_df, "Data/Merge Data/CO2 Emissions(cleansed).csv", row.names = FALSE)


# Load csv to the dataframe
energy_data <- read.csv("Data/Energy Generation.csv", stringsAsFactors = FALSE)

#remove empty rows
energy_data <- energy_data[energy_data$API != "",]

#remove unneccessary columns
energy_data <- energy_data[, -c(3:12,44:46)]

#Energy Data Integration
energy_data_Nuclear <- energy_data[energy_data$X == "        Nuclear (quad Btu)", ]
energy_data_Total_Renewable <- energy_data[energy_data$X == "        Renewables (quad Btu)", ]
energy_data_hydro <- energy_data[energy_data$X == "            Hydroelectricity (quad Btu)", ]
energy_data_non_hydro <- energy_data[energy_data$X == "            Non-hydroelectric renewables (quad Btu)", ]
energy_data_geo <- energy_data[energy_data$X == "                Geothermal (quad Btu)", ]
energy_data_wind <- energy_data[energy_data$X == "                Wind (quad Btu)", ]
energy_data_biowaste <- energy_data[energy_data$X == "                Biomass and waste (quad Btu)", ]
energy_data_solar <- energy_data[energy_data$X == "                    Solar (quad Btu)", ]


# Add a new column 'Country'
country <- read.csv("Data/Countries.csv", stringsAsFactors = FALSE)
energy_data_Nuclear$Country <- country$x
energy_data_Total_Renewable$Country <- country$x
energy_data_hydro$Country <- country$x
energy_data_non_hydro$Country <- country$x
energy_data_geo$Country <- country$x
energy_data_wind$Country <- country$x
energy_data_biowaste$Country <- country$x
energy_data_solar$Country <- country$x

# Move 'Country' to the first column
energy_data_Nuclear <- energy_data_Nuclear[, c("Country", names(energy_data_Nuclear)[-length(names(energy_data_Nuclear))])]
energy_data_Total_Renewable <- energy_data_Total_Renewable[, c("Country", names(energy_data_Total_Renewable)[-length(names(energy_data_Total_Renewable))])]
energy_data_hydro <- energy_data_hydro[, c("Country", names(energy_data_hydro)[-length(names(energy_data_hydro))])]
energy_data_non_hydro <- energy_data_non_hydro[, c("Country", names(energy_data_non_hydro)[-length(names(energy_data_non_hydro))])]
energy_data_geo <- energy_data_geo[, c("Country", names(energy_data_geo)[-length(names(energy_data_geo))])]
energy_data_wind <- energy_data_wind[, c("Country", names(energy_data_wind)[-length(names(energy_data_wind))])]
energy_data_biowaste <- energy_data_biowaste[, c("Country", names(energy_data_biowaste)[-length(names(energy_data_biowaste))])]
energy_data_solar <- energy_data_solar[, c("Country", names(energy_data_solar)[-length(names(energy_data_solar))])]


#wide to long format
energy_data_Nuclear <- melt(energy_data_Nuclear, id.vars = c("API", "X", "Country"), 
                            variable.name = "Year", value.name = "Nuclear - quad Btu")
energy_data_Total_Renewable <- melt(energy_data_Total_Renewable, id.vars = c("API", "X", "Country"), 
                                    variable.name = "Year", value.name = "Total Renewables - quad Btu")
energy_data_hydro <- melt(energy_data_hydro, id.vars = c("API", "X", "Country"), 
                         variable.name = "Year", value.name = "Hydroelectricity - quad Btu")
energy_data_non_hydro <- melt(energy_data_non_hydro, id.vars = c("API", "X", "Country"), 
                              variable.name = "Year", value.name = "Non hydroelectric renewables - quad Btu")
energy_data_geo<- melt(energy_data_geo, id.vars = c("API", "X", "Country"), 
                       variable.name = "Year", value.name = "Geothermal - quad Btu")
energy_data_wind <- melt(energy_data_wind, id.vars = c("API", "X", "Country"), 
                         variable.name = "Year", value.name = "Wind - quad Btu")
energy_data_biowaste <- melt(energy_data_biowaste, id.vars = c("API", "X", "Country"), 
                            variable.name = "Year", value.name = "Biowaste - quad Btu")
energy_data_solar <- melt(energy_data_solar, id.vars = c("API", "X", "Country"), 
                          variable.name = "Year", value.name = "solar - quad Btu")


# Remove "X" in Year 
energy_data_Nuclear$Year <- gsub("X", "", energy_data_Nuclear$Year)
energy_data_Total_Renewable$Year <- gsub("X", "", energy_data_Total_Renewable$Year)
energy_data_hydro$Year <- gsub("X", "", energy_data_hydro$Year)
energy_data_non_hydro$Year <- gsub("X", "", energy_data_non_hydro$Year)
energy_data_geo$Year <- gsub("X", "", energy_data_geo$Year)
energy_data_wind$Year <- gsub("X", "", energy_data_wind$Year)
energy_data_biowaste$Year <- gsub("X", "", energy_data_biowaste$Year)
energy_data_solar$Year <- gsub("X", "", energy_data_solar$Year)

# Remove X Column
energy_data_Nuclear <- energy_data_Nuclear %>% select(-X)
energy_data_Total_Renewable <- energy_data_Total_Renewable %>% select(-X)
energy_data_hydro <- energy_data_hydro %>% select(-X)
energy_data_non_hydro <- energy_data_non_hydro %>% select(-X)
energy_data_geo <- energy_data_geo %>% select(-X)
energy_data_wind <- energy_data_wind %>% select(-X)
energy_data_biowaste <- energy_data_biowaste %>% select(-X)
energy_data_solar <- energy_data_solar %>% select(-X)

# Remove API Column
energy_data_Nuclear <- energy_data_Nuclear %>% select(-API)
energy_data_Total_Renewable <- energy_data_Total_Renewable %>% select(-API)
energy_data_hydro <- energy_data_hydro %>% select(-API)
energy_data_non_hydro <- energy_data_non_hydro %>% select(-API)
energy_data_geo <- energy_data_geo %>% select(-API)
energy_data_wind <- energy_data_wind %>% select(-API)
energy_data_biowaste <- energy_data_biowaste %>% select(-API)
energy_data_solar <- energy_data_solar %>% select(-API)

# Merge all the dataframe
Fin_Energy_df <- merge(energy_data_Nuclear, energy_data_Total_Renewable, by = c("Year", "Country"))
Fin_Energy_df <- merge(Fin_Energy_df, energy_data_hydro, by = c("Year", "Country"))
Fin_Energy_df <- merge(Fin_Energy_df, energy_data_non_hydro, by = c("Year", "Country"))
Fin_Energy_df <- merge(Fin_Energy_df, energy_data_geo, by = c("Year", "Country"))
Fin_Energy_df <- merge(Fin_Energy_df, energy_data_wind, by = c("Year", "Country"))
Fin_Energy_df <- merge(Fin_Energy_df, energy_data_biowaste, by = c("Year", "Country"))
Fin_Energy_df <- merge(Fin_Energy_df, energy_data_solar, by = c("Year", "Country"))


write.csv(Fin_Energy_df, "Data/Merge Data/Electricity Generation(cleansed).csv", row.names = FALSE)


