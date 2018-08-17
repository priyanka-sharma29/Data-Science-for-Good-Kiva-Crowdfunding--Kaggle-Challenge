#Libraries
options(scipen = 99999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, DT, janitor, stringr, stringi, StandardizeText, scales,
               lubridate, rvest, knitr, RCurl, DescTools, PerformanceAnalytics, GGally, ggplot2, ggrepel,
               gridExtra, leaflet, highcharter, viridis, lubridate, jsonlite, countrycode, purrrlyr)

# User Defined functions
generate_genders <- function(genderfield){
  genderfield <- as.character(genderfield)
  if(is.na(genderfield))
  {
    return('unknown')
  }
  if(genderfield == 'male')
  {
    return('male')
  }
  else if(genderfield == 'female')
  {
    return('female')
  }
  else
  {
    return('group')
  }
}

find_nas_in_df <- function() {
  data.frame(sapply(df, function(x) sum(is.na(x)))) %>%
    rownames_to_column("column") %>%
    rename("na_count" = "sapply.df..function.x..sum.is.na.x...") %>%
    filter(na_count > 0) %>%
    arrange(desc(na_count)) %>%
    mutate(na_percentage = na_count * 100 / nrow(df))
}
# Working on the Data

country_stats <- read.csv("C:/Users/priya/Desktop/ALL/Spring 2018/Theja_IDS 575/theja proj/kiva/final four files/kiva_loans.csv")
locations <- read.csv("C:/Users/priya/Desktop/ALL/Spring 2018/Theja_IDS 575/theja proj/kiva/final four files/kiva_mpi_region_locations.csv")


# Merging the Data
kiva_country_stats         <-  country_stats %>%
                               select(country_code,continent)

kiva_loan_theme_ids        <-  loan_theme_ids %>%
                               select(-c(`Loan Theme ID`, `Partner ID`))

kiva_loan_themes_by_region <-  loan_themes_by_region %>%
                               rename("lattitute" = "lat",
                                       "longitude" = "lon") %>%
                               mutate(country = standardize.countrynames(input = country, suggest = "auto"),
                                       ISO = countrycode(country, 'country.name', 'iso3c'),
                                       ISO = ifelse(country == "Kosovo", "XKX", ISO),
                                       ISO = ifelse(country == "Virgin Islands", "VIR", ISO)) %>%
                                       select(ISO, rural_pct, country, lattitute, longitude)

kiva_loan                  <-  kiva_loans %>%
                               mutate(year = as.numeric(format(as.Date(date), "%Y")),
                                      continent = as.character(countrycode(sourcevar = country, origin = "country.name", destination = "continent")),
                                      continent = ifelse(is.na(continent), "Na - Disputed countries", continent),
                                      ISO = countrycode(country, 'country.name', 'iso3c'),
                                      ISO = ifelse(country == "Kosovo", "XKX", ISO), #disputed territory
                                      ISO = ifelse(country == "Virgin Islands", "VIR", ISO), #disputed territory
                                      use = str_to_lower(use),
                                      use = str_replace_all(use, "[^[:alpha:]]", " "),
                                      use = str_replace_all(use, "\\s+", " ")) %>%
                                      select(id, funded_amount, loan_amount, sector, country_code, country, ISO,
                                             region, partner_id, term_in_months, borrower_genders, repayment_interval, date)

kiva_mpi_national          <- MPI_national %>%
                              select(c(ISO, `MPI Urban`, `MPI Rural`,
                                      `Intensity of Deprivation Urban`, `Intensity of Deprivation Rural`))

kiva_world_index           <- Kaggle %>%
                              rename("country" = "Id","hdi_2014" = "Human Development Index HDI-2014") %>%
                              mutate(country = standardize.countrynames(input = country, suggest = "auto"),
                                    ISO = countrycode(country, 'country.name', 'iso3c'),
                                    ISO = ifelse(country == "Kosovo", "XKX", ISO),
                                    ISO = ifelse(country == "Virgin Islands", "VIR", ISO)) %>%
                                    select(ISO, hdi_2014)

                        df <- kiva_loan %>%
                              left_join(kiva_loan_theme_ids, by= "id") %>%
                              left_join(kiva_mpi_national, by= "ISO") %>%
                              left_join(kiva_world_index, by="ISO")%>%
                              left_join(kiva_country_stats, by="country_code")
View(df)

# Cleaning the Data

# Renaming the columns
names(df)[names(df)=="Loan Theme Type"] <- "loan_theme_type"
names(df)[names(df)=="MPI Urban"] <- "MPI_urban"
names(df)[names(df)=="MPI Rural"] <- "MPI_rural"
names(df)[names(df)=="Intensity of Deprivation Urban"] <- "intensity_Depri_urban"
names(df)[names(df)=="Intensity of Deprivation Rural"] <- "intensity_Depri_rural"

# Removing NAs
find_nas_in_df()
  # Country code
  df<-df[!is.na(df$country_code), ]
  # Region
  df<-df[!is.na(df$region), ]
  # MPI_urban, MPI_rural, intensity_Depri_urban, intensity_Depri_rural
  df<-df[!is.na(df$MPI_urban), ]
  # Loan theme type
  df<-df[!is.na(df$loan_theme_type), ]
  # HDI_2014
  df<-df[!is.na(df$hdi_2014), ]
  # Continent
  df<-df[!is.na(df$continent), ]
find_nas_in_df()

# Reducing the Data
# Asia
df_asia<-df[df$continent=="Asia",]

# 2017
df_asia$date <- as.Date(df_asia$date)
df_asia$year<-year(df_asia$date)
df_asia$month<-month(df_asia$date)
df_asia$month<-as.factor(df_asia$month)

df_asia_2017<-df_asia[df_asia$year=="2017",]

# Transformations

# Dependent Variable Transformation
# LOAN_AMOUNT
install.packages('pysch')
library(psych)
describe(df_asia_2017$loan_amount)
summary(df_asia_2017$loan_amount)
# The range is x, between x and x
# The kurtosis is  x which is far from normal distribution
# Plotting Analysis
dev.off()
par(mfrow=c(1,3))
# Histogram
hist(df_asia_2017$loan_amount,main="Loan Amount",xlab ="Loan Amount", col="steelblue", freq=F)
rug(jitter(df_asia_2017$loan_amount), col="darkgray")
box()
# Densityplot
plot(density(df_asia_2017$loan_amount),main="Loan Amount",xlab ="Loan Amount", col="darkgray", lwd=3)
box()
# Boxplot
boxplot(df_asia_2017$loan_amount, main="Loan Amount", xlab ="Loan Amount", col="orange")

# Removing the Outliers(Dependent)
df_asia_2017_New <- df_asia_2017[df_asia_2017$loan_amount < 2000,]

# Independent Variable Transformation-MOST SKEWED
# TERMS_IN_MONTHS
dev.off()
par(mfrow=c(1,3))
# Histogram
hist(df_asia_2017_New$term_in_months,main="term_in_months",xlab ="term_in_months", col="steelblue", freq=F)
rug(jitter(df_asia_2017_New$term_in_months), col="darkgray")
box()
# Densityplot
plot(density(df_asia_2017_New$term_in_months),main="term_in_months",xlab ="term_in_months", col="darkgray", lwd=3)
box()
# Boxplot
boxplot(df_asia_2017_New$term_in_months, main="term_in_months", xlab ="term_in_months", col="orange")

# Removing the Outliers(Independent)
df_asia_2017_New <- df_asia_2017_New[df_asia_2017_New$term_in_months < 30,]

# df_asia_2017_New
describe(df_asia_2017_New$loan_amount)
summary(df_asia_2017_New$loan_amount)
# The range is x, between x and x
# The kurtosis is  x which is far from normal distribution
# Plotting Analysis
dev.off()
par(mfrow=c(1,3))
# Histogram
hist(df_asia_2017_New$loan_amount,main="Loan Amount",xlab ="Loan Amount", col="steelblue", freq=F)
rug(jitter(df_asia_2017_New$loan_amount), col="darkgray")
box()
# Densityplot
plot(density(df_asia_2017_New$loan_amount),main="Loan Amount",xlab ="Loan Amount", col="darkgray", lwd=3)
box()
# Boxplot
boxplot(df_asia_2017_New$loan_amount, main="Loan Amount", xlab ="Loan Amount", col="orange")

# Handling the levels
library(dplyr)
# Borrower genders
df_asia_2017_New$borrower_genders <- sapply(df_asia_2017_New$borrower_genders, generate_genders)
# Regions
df_asia_2017_New=df_asia_2017_New %>% mutate_if(is.character, as.factor)
df_asia_2017_New$region <- as.character(df_asia_2017_New$loan_theme_type)
# Loan theme type
keep <- levels(df_asia_2017_New$loan_theme_type)[table(df_asia_2017_New$loan_theme_type) > 100]
df_asia_2017_New <- df_asia_2017_New[df_asia_2017_New$loan_theme_type %in% keep, ]
df_asia_2017_New$loan_theme_type <- df_asia_2017_New$loan_theme_type[drop = TRUE]

# Dropping unwanted columns
df_asia_2017_New$partner_id<-NULL
df_asia_2017_New$country_code<-NULL
df_asia_2017_New$continent<-NULL
df_asia_2017_New$year<-NULL

########## bivariate
# loan amount and months
df_asia_2017_New$month <- as.factor(df_asia_2017_New$month)
ggplot(data = df_asia_2017_New, aes(x=loan_amount, fill= month ))+geom_density(alpha=.8) +
  labs(title="Loan Amount distribution by months", x="Loan Amount", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "month")

# loan amount and loan theme type
ggplot(data = df_asia_2017_New, aes(x=loan_amount, fill= loan_theme_type ))+geom_density(alpha=.8) +
  labs(title="Loan Amount distribution by loan_theme_type", x="Loan Amount", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "loan_theme_type")

