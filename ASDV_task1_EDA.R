## Dataset
CO2_emissions <- read.csv("World_bank_world_development_indicators_CO2_emmisions_Task2.csv", header= TRUE)
str(CO2_emissions)

dim(CO2_emissions)
names(CO2_emissions)

unique(CO2_emissions$Time)
unique(CO2_emissions$Country.Name)
unique(CO2_emissions$Income.Type)

head(CO2_emissions)
tail(CO2_emissions)

summary(CO2_emissions)


hist(CO2_emissions$CO2.intensity)
hist(CO2_emissions$CO2.emissions.from.electricity.and.heat.production)
hist(CO2_emissions$CO2.emissions.from.manufacturing.industries.and.construction)
hist(CO2_emissions$CO2.emissions.from.transport)
hist(CO2_emissions$CO2.emissions.from.residential.buildings.and.commercial.and.public.services)
hist(CO2_emissions$CO2.emissions.from.other.sectors)


boxplot(CO2.intensity~Income.Type,CO2_emissions)
boxplot(CO2.emissions.from.electricity.and.heat.production~Country.Name,CO2_emissions)
boxplot(CO2.emissions.from.electricity.and.heat.production~Income.Type,CO2_emissions)
