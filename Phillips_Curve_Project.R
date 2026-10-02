library(WDI)
library(tidyverse)
library(DBI)
library(RSQLite)

#importing data
indicators <- c(
  inflation    = "FP.CPI.TOTL.ZG",     # consumer price inflation, annual %
  unemployment = "SL.UEM.TOTL.ZS",     # unemployment, % of labour force
  gdp_growth   = "NY.GDP.MKTP.KD.ZG"   # real GDP growth, annual %
)

countries <- c("IE", "GB", "DE", "FR", "US", "ES", "IT", "JP")
raw_data <- WDI(country = countries, indicator = indicators, start=2000, end=2024)

head(raw_data)

#cleaning data
clean_data <- raw_data %>%
  select(-iso3c) %>%
  filter(!is.na(inflation), !is.na(unemployment), !is.na(gdp_growth)) %>%
  arrange(country, year)

#----------------
#SQL
#----------------

con <- dbConnect(SQLite(), "econ.db")

dbWriteTable(con, "data", clean_data, overwrite = TRUE)
dbGetQuery(con,
           "SELECT DISTINCT country FROM data")

country_info <- tibble(
  country = c("France", "Germany", "Ireland", "Italy", "Japan", "Spain", "United Kingdom", "United States"),
  region = c("Europe", "Europe", "Europe", "Europe", "Asia", "Europe", "Europe", "North America"),
  eurozone = c(1, 1, 1, 1, 0, 1, 0, 0)
)

dbWriteTable(con, "country_info", country_info, overwrite = TRUE)

#---------------------
#Query
#---------------------
#Average inflation & unemployment by country
dbGetQuery(con, 
           "SELECT country, AVG(inflation) AS avg_inflation, AVG(unemployment) AS avg_unemployment FROM data GROUP BY country")


#All instances of 5% inflation or higher
dbGetQuery(con,
           "SELECT country, year, inflation 
           FROM data 
           WHERE inflation >= 5
           ORDER BY inflation DESC")

#Ranking countries by GDP by year
dbGetQuery(con, "
           SELECT country, year, gdp_growth,
           RANK() OVER(
           PARTITION BY year
           ORDER BY gdp_growth 
           DESC
           )
           AS gdp_rank
           FROM data
           ORDER BY year, gdp_rank
           LIMIT 10")

#Change in unemployment in each country year on year
dbGetQuery(con, "
           SELECT country, year, unemployment,
           unemployment - LAG(unemployment) OVER(
           PARTITION BY country
           ORDER BY year
           ) AS change_unemployment
           FROM data
           LIMIT 10
           ")
#-----------------
#Pulling the Model out
#-----------------
df <- dbGetQuery(con, "
                 SELECT
                 year,
                 country,
                 inflation,
                 gdp_growth,
                 unemployment,
                 unemployment-LAG(unemployment) 
                 OVER(PARTITION BY country
                 ORDER BY year) AS unemployment_change
                 FROM data
                 ")
#---Graphing the Phillips Curve---#

#Main Graph
df%>%
    ggplot(aes(y= inflation, x= unemployment))+
      geom_point(aes(colour=country))+
      geom_smooth(method = "lm", se=TRUE)+
      labs(title="Inflation vs Unemployment", x="Unemployment %", y = "Inflation %")+
      theme_minimal()
#Graph by Country
df%>%
  ggplot(aes(y= inflation, x= unemployment))+
  geom_point()+
  geom_smooth(method = "lm", se=TRUE)+
  labs(title="Inflation vs Unemployment", x="Unemployment %", y = "Inflation %")+
  theme_minimal()+
  facet_wrap(~country, scales="free")

#---Modelling the Phillips Curve----

#Naive Regression
reg1 <- lm(unemployment ~ inflation, df)
summary(reg1)

#Did the relationship change after the 2008 crisis?
reg2 <- lm(unemployment ~ inflation, filter(df, year>=2009))
reg3 <- lm(unemployment ~ inflation, filter(df, year<2009))
summary(reg2)
summary(reg3)

df <-df %>% 
  mutate(period = ifelse(year<=2008, "2000-2008", "2009-2024"))

  ggplot(df, aes(y=unemployment, x=inflation, colour = period))+
      geom_point()+
      geom_smooth(method="lm")+
      theme_minimal()+
      labs(title = "Phillips Curve Pre/Post 2008", y="Unemployment %", x="Inflation %")

#---Okun's Law Test---

#Initial Graph
ggplot(df, aes(y=gdp_growth, x=unemployment_change))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_minimal()+
  labs(title = "Okun's Law", x = "Change in % Unemployment", y="GDP Growth Annual %")
  
#Simple Regression
summary(lm(gdp_growth~unemployment_change, df))

#Disconnect Database
dbDisconnect(con)










