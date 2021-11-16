# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr", "sf","rgeos") # package names
pacman::p_load(pkgs, character.only = T)

# LOAD DATA --------------------------------------------------------------------

# Cases at country level  (JHU data)
cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
readr::write_csv(cases, "data/original/cases_raw.csv")

# Shapefile for South America
#samerica <- st_read("data/original/geodata/vc965bq8111.shp") 
samerica <- st_read("data/original/South_America/South_America.shp")
samerica$COUNTRY = stringr::str_to_title(samerica$COUNTRY)
samerica <- samerica %>%
  rename(name = COUNTRY) %>%
  select(name,geometry) %>%
  mutate(name = case_when(
    name == "French Guiana (France)" ~ "French Guiana",
    TRUE ~ name))
samerica <- samerica[order(samerica$name), ]


# Policy index
policy <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
readr::write_csv(policy, "data/original/policy_raw.csv")

# Vaccine Data
vaccines <- readr::read_csv("data/original/vaccinations.csv")
vaccines <- vaccines %>%
  rename(country = location,
         vax = total_vaccinations_per_hundred) %>%
  select(country, vax, date) %>%
  filter(country %in% samerica$name)

vax <- vaccines %>%
  tidyr::spread(key = country, value = vax) %>% 
  select(-date) %>%
  as.matrix()
rownames(vax) <- sort(unique(as.character(vaccines$date)))

#Interpolate missing vaccine data
vax_interp <- vax
for(i in 1:ncol(vax))
{
  print(i)
tmp = approx(as.Date(rownames(vax)),as.numeric(vax[,i]),as.Date(rownames(vax)),yleft=0)
vax_interp[,i] = tmp$y
}


#HDI Data
HDI <- readr::read_csv("data/original/Human Development Index (HDI).csv")
HDI <- HDI %>%
  rename(name = "...2", 
         HDI = "...3") %>%
select(name, HDI) %>%
  mutate(name = case_when(
    name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    name == "Bolivia (Plurinational State of)" ~ "Bolivia",
    TRUE ~ name)) %>%
  filter(name %in% samerica$name)

HDI$HDI = as.numeric(HDI$HDI)


#Population Data
Pop2020 <- readr::read_csv("data/original/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES_SAMERICA.csv")
Pop2020 <- Pop2020 %>%
  rename(name = "...3", 
         Pop2020 = "...8") %>%
  select(name, Pop2020) %>%
  mutate(name = case_when(
    name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    name == "Bolivia (Plurinational State of)" ~ "Bolivia",
    TRUE ~ name)) %>%
  filter(name %in% samerica$name)

Pop2020$Pop2020 = as.numeric(gsub(" ","",Pop2020$Pop2020))*1000


#Age Data
Age <- readr::read_csv("data/original/median_age (UN2020).csv")
Age <- Age %>%
  rename(name = 'Country/Region', 
         Age = 'Median age') %>%
  select(name, Age) %>%
  mutate(name = case_when(
    name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    name == "Bolivia (Plurinational State of)" ~ "Bolivia",
    TRUE ~ name)) %>%
  filter(name %in% samerica$name)
Age$Age = as.numeric(Age$Age)

# DATA PREPARATION -------------------------------------------------------------

# Remove Countries from shape file (Islands, and French Guiana, which is  not contained in the policy file)
countries_to_remove <-  c("Falkland Islands (Islas Malvinas) (Uk)", "In Dispute Brazil/Uruguay", "Curacao (Neth)", "South Georgia & The South Sandwich Islands (Uk)",
                          "Bonaire (Neth)","Trinidad & Tobago","Aruba (Neth)","French Guiana")

countries_to_remove <-  c("Falkland Islands (Islas Malvinas) (Uk)", "In Dispute Brazil/Uruguay", "Curacao (Neth)", "South Georgia & The South Sandwich Islands (Uk)",
                          "Bonaire (Neth)","Trinidad & Tobago","Aruba (Neth)","French Guiana", "South Georgia and the South Sandwich Is (UK)", 
                          "Falkland Islands (UK)")

samerica <- samerica[!(samerica$name %in% countries_to_remove), ] 

#This is very slow, not sure if this is the way to best combine polygons
samerica <- samerica %>%
  group_by(name) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
#Simplify geometries
samerica <- st_simplify(samerica,dTolerance=.1)
#Add other constant covariates
samerica = inner_join(samerica, HDI, by = "name")
samerica = inner_join(samerica, Pop2020, by = "name")
samerica = inner_join(samerica, Age, by = "name")



#Other methods for combining polygons of same country (not working)
#s4 <- st_as_sf(maptools::unionSpatialPolygons(as_Spatial(s2),rep(1, nrow(s2))))
#s5 <- st_as_sf(rgeos::gBuffer(as_Spatial(s2), byid=F, width=0))




# Clean JHU data :
#   - rename countries according to shapefile
#   - keep only countries that we are going to analyse
#   - calculate daily new cases
#   - replace the 4 negatives with 0


cases <- cases %>% 
  select(-Lat, -Long) %>% 
  rename(country = `Country/Region`) %>% 
  mutate(country = case_when(
    `Province/State` == "French Guiana" ~ "French Guiana", #This needs to be done because the country listing for French Guiana is 'France'
    TRUE ~ country
  )) %>% 
  select(-`Province/State`) %>%
  filter(country %in% samerica$name)




# Check that the order of cases and countries in the shapefile are the same
cases <- cases[order(cases$country), ]
all(cases$country == unique(samerica$name))

# Reshape cases with times in the row and geographical units in the column
# so each column is the cases time series for the county
# the results should be of dimension T x I 

# First change the names of the columns to proper dates
names(cases)[-1] <- names(cases)[-1] %>% 
  as.Date(format = "%m/%d/%y") %>% 
  as.character()
counts <- t(cases[, -1]) 
colnames(counts) <- cases$country
counts <- apply(counts, 2, diff)
counts[counts < 0] <- 0

# Clean policy data
policy_clean <- policy %>% 
  select(country = CountryName, date = Date, 
         testing = `H2_Testing policy`, sindex = StringencyIndex,
         Jurisdiction) %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"),
       #  country = case_when(
      #     country == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
      #     country == "Gambia" ~ "The Gambia",
      #     country == "Eswatini" ~ "Swaziland",
      #     country == "Congo" ~ "Republic of Congo",
      #     TRUE ~ country
       #  )
) %>% 
  filter(country %in% samerica$name & Jurisdiction=="NAT_TOTAL")
policy_clean <- policy_clean %>%
  select(-Jurisdiction)
  
testing <- policy_clean %>% 
  select(-sindex) %>% 
  tidyr::spread(key = country, value = testing) %>% 
  select(-date) %>% 
  as.matrix()

testing[is.na(testing)] <- 0

rownames(testing) <- unique(as.character(policy_clean$date))

sindex <- policy_clean %>% 
  select(-testing) %>% 
  tidyr::spread(key = country, value = sindex) %>% 
  select(-date) %>% 
  as.matrix()

sindex[is.na(sindex)] <- 0

rownames(sindex) <- unique(as.character(policy_clean$date))

## Weather data
#weather_clean <- readr::read_csv("data/original/AfricaCountries_2020-12-08_ALLEXTRACTEDDATA.csv")
## See what the common dates are for the time varying datasets and censor
## accordingly 
#final_dates <- Reduce(intersect, list(as.character(weather_clean$Date),
#                                      rownames(counts), 
#                                      rownames(sindex)))


# See what the common dates are for the time varying datasets and censor
# accordingly 
# final_dates <- Reduce(intersect, list(rownames(counts), 
#                                       rownames(sindex),
#                                       rownames(vax_interp)))
final_dates <- Reduce(intersect, list(rownames(counts), 
                                      rownames(sindex)))


# añadir al inicio filas de 0 a vax_interp, para completar las fechas iniciales
# y poder procesar con más fechas y no limitarse sólo a las fechas que contiene
# el .csv de vacunación

vax_interp <- vax_interp[order(row.names(vax_interp)),] # ordenar ascendentemente por fecha

actual <- final_dates[1]
fin <- rownames(vax_interp)[1]
i <- 1

fila_ceros <- data.frame(matrix(ncol=length(paises), nrow=0))
fila_ceros[actual,] <- rep(0:0, times=length(paises))

if ( all(vax_interp[fin,]== fila_ceros) ) # comprobar que toda la primera fila sea 0 antes de hacer los cambios
{
  while(actual != fin)
  {
    nueva_fila <- data.frame(matrix(ncol=12, nrow=0))
    nueva_fila[actual,] <- vax_interp[rownames(vax_interp)==fin,]
    colnames(nueva_fila) <- colnames(vax_interp)

    vax_interp <- rbind(vax_interp, nueva_fila)

    i <- i+1
    actual <- final_dates[i]
  }
}

# ordenar ascendentemente por fecha
vax_interp <- vax_interp[order(row.names(vax_interp)),]

final_dates <- Reduce(intersect, list(rownames(counts), 
                                      rownames(sindex),
                                      rownames(vax_interp)))

counts <- counts[rownames(counts) %in% final_dates, ]
sindex <- sindex[rownames(sindex) %in% final_dates, ]
testing <- testing[rownames(testing) %in% final_dates, ]
vax_interp <- vax_interp[rownames(vax_interp) %in% final_dates,]

# Save the cleaned data 
saveRDS(sindex, "data/processed/stringency.rds")
saveRDS(testing, "data/processed/testing.rds")
saveRDS(vax_interp, "data/processed/vax.rds")
saveRDS(counts, "data/processed/daily_cases.rds")
st_write(samerica, "data/processed/geodata/samerica.gpkg", delete_dsn = T)

