require(httr) #Working with url
library(jsonlite) #Working with json data for API
library(tidyverse) #Tidy dataframe packages
library(purrr) #list manipulation
library(janitor) # Data cleansing and pivot
library(patchwork) #Easy grid arrange of ggplots
library(tidyquant) #Set of finance packages
library(anytime) #read any type of date format
library(readxl) #read/write excel data
library(stringr) #string manipulation
library(timetk) #tibble format for time based dataframe
library(tibbletime) #tibble format for time based dataframe
library(PortfolioAnalytics) #Porfolio analysis
library(ROI) #Optimization package
library(ROI.plugin.glpk) #Plugins needed
library(ROI.plugin.quadprog) #Plugins needed
library(knitr) #Tables in rmd
library(kableExtra) #Graphics for knitr tables
library(cowplot) #Grid plot for list plots
library(ggstatsplot) #Statistical testing in plot
library(h2o) #Machine learning models
library(lime) #Allow for black box models to be easily evaluated
library(lubridate) #Allow for changes in date format
library(gridExtra)
library(ggdendro)
library(zoo)
library(tsibble)
library(broom)



options(scipen=999)

rm(list=ls())


# 01. Data ----------------------------------------------------------------

API_Structure <- tribble(
  ~Category, ~Informaton, ~url, ~Options, ~TimeUpdate,
  "Company Valuation","Symbols List", "https://financialmodelingprep.com/api/v3/company/stock/list", NULL, NULL,
  "Company Valuation", "Company Profile","https://financialmodelingprep.com/api/v3/company/profile/","company", "Minute",
  "Company Valuation", "Income Statement","https://financialmodelingprep.com/api/v3/financials/income-statement/","company, time", "Annual/Quarter",
  "Company Valuation", "Balance Sheet Statement", "https://financialmodelingprep.com/api/v3/financials/balance-sheet-statement/","company, time",  "Annual/Quarter",
  "Company Valuation", "Cash Flow Statement", "https://financialmodelingprep.com/api/v3/financials/cash-flow-statement/", "company, time",  "Annual/Quarter",
  "Company Valuation", "Company Financial Ratios", "https://financialmodelingprep.com/api/v3/financial-ratios/", "Company", "Annual",
  "Company Valuation", "Company Enterprise Value",  "https://financialmodelingprep.com/api/v3/enterprise-value/", "company, time",  "Annual/Quarter",
  "Company Valuation", "Company Key Metrics", "https://financialmodelingprep.com/api/v3/company-key-metrics/", "company, time",  "Annual/Quarter",
  "Company Valuation", "Company Rating", "https://financialmodelingprep.com/api/v3/company/rating/", "Company", "Daily",
  "Stock Price", "Stock Real-time Price", "https://financialmodelingprep.com/api/v3/stock/real-time-price/", "Company", "Real-time",
  "Stock Price", "Historical Daily Price", "https://financialmodelingprep.com/api/v3/historical-price-full/", "Company", "Daily"
) %>%
  mutate(id = row_number()) %>%
  select(id, everything())


kable(API_Structure[,-1], caption = "API Structure") %>%
  kable_styling(full_width = F)



# 1.1 Brief overview of stock lists ---------------------------------------

#Company informations
GetCompanyProfile <- function(url, company = NULL){
  
  headers = c(
    `Upgrade-Insecure-Requests`= '1'
  )
  
  params = list(
    `datatype` = 'json'
  )
  
  res <- httr::GET(url = paste0(url,"/",company),
                   httr::add_headers(.headers=headers), query = params)
  
  data <- content(res, as = "text")
  
  data <- fromJSON(data, flatten = T) %>%
    flatten_dfr()
  
  return(data)
  
}

#Get data from API structure
GetData <- function(url, company = NULL, Period = NULL){
  
  
  headers = c(
    `Upgrade-Insecure-Requests`= '1'
  )
  
  params = list(
    `datatype` = 'json'
  )
  
  
  
  if (is.null(company) & is.null(Period)) {
    res <- httr::GET(url = url,
                     httr::add_headers(.headers=headers), query = params)
    
  } else if (is.null(Period)) {
    res <- httr::GET(url = paste0(url,"/",company),
                     httr::add_headers(.headers=headers), query = params)
    
  } else {
    res <- httr::GET(url = paste0(url,"/",company, "?period=",Period),
                     httr::add_headers(.headers=headers), query = params)
  }
  
  data <- content(res, as = "text")
  
  data <- fromJSON(data, flatten = T) %>%
    detect(is.data.frame) %>%
    as_tibble()
  
  return(data)
  
}


# 1.2 Companies Symbol ----------------------------------------------------

Stock_Lists <- GetData(url = "https://financialmodelingprep.com/api/v3/company/stock/list")

glimpse(Stock_Lists)


# 1.2.1 SP500 -------------------------------------------------------------

#SP500 Indexes
SP500 <- tq_index("SP500")


Stock_Lists <- GetData(url = "https://financialmodelingprep.com/api/v3/company/stock/list") %>%
  filter(symbol %in% SP500$symbol) %>% #Symbols of SP500 
  filter(!symbol %in% c("J","AMCR")) #Companies that doesn't have data from API and causes error


glimpse(Stock_Lists)




# 2. Project Data ---------------------------------------------------------


# 2.1 Segments ------------------------------------------------------------

segments <- Stock_Lists[1:400, ] %>% #Filter data for memory capacity
  mutate(Company_Profile = map(symbol, ~GetCompanyProfile(API_Structure[2,4], company = ..1))) %>% #Get Data per symbol
  select(Company_Profile) %>% #Select nested list
  unnest() %>% # Unnest it
  mutate(industry = case_when(industry == "" ~ "Funds", TRUE ~ industry), #Set sectors and industries empty as funds
         sector = case_when(sector == "" ~ "Funds", TRUE ~ sector)) %>%
  select(symbol, companyName, industry, sector) #Select the data required for this dataframe

glimpse(segments)



# 2.1 Price Sectors -------------------------------------------------------

PriceSectors <- Stock_Lists[1:400, ] %>% #Filter data for memory capacity
  mutate(Company_Profile = map(symbol, ~GetCompanyProfile(API_Structure[2,4],
                                                          company = ..1))) %>% #Get Data per symbol
  select(Company_Profile) %>% #Select nested list
  unnest() %>% # unnest it
  mutate(industry = case_when(industry == "" ~ "Funds", TRUE ~ industry),  #Set sectors and industries empty as funds
         sector = case_when(sector == "" ~ "Funds", TRUE ~ sector))

glimpse(PriceSectors)



# 2.2. KeyMetrics ---------------------------------------------------------

#metrics
path <- "Market KeyMetrics.xlsx"

Metrics_Info <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)


kable(head(Metrics_Info$KeyMetrics), caption = "10 Metrics info") %>%
  kable_styling(full_width = F)



KeyMetrics <- Stock_Lists[1:400, ] %>% #Filter data for memory capacity
  mutate(Company_Key_Metrics = map(symbol, ~GetData(API_Structure[8,4], company = ..1))) %>% #Get Data per symbol
  select(symbol, name, Company_Key_Metrics) %>% #Select data and nested API data
  unnest(Company_Key_Metrics) %>% # Unnest it
  gather(key = "metric", value = "value", -symbol, -date, -name) %>% # Pivot the metrics per symbol
  inner_join(segments, by = "symbol") %>% #Get segments data to enrich the dataset
  inner_join(Metrics_Info$KeyMetrics, by = c("metric"="Metric")) %>% #Get the description and formula of metrics
  select(-companyName) %>% # Remove duplicate columns
  mutate(value = as.double(value), date = anydate(date)) %>% # Fix data structure
  group_by(metric, Explanation, Formula) %>% # Nest data per metric
  nest()


glimpse(KeyMetrics)



# 2.3 Historical Prices ---------------------------------------------------

HistoricalPrices <- Stock_Lists[1:400, ] %>% #Filter data for memory capacity
  #Get Data per symbo
  mutate(Historical_Daily_Price = map(symbol, ~GetData(API_Structure[11,4],
                                                       company = ..1) %>%
                                        mutate(date = anytime(date)))) %>%
  #Adjust monthly price
  mutate(Monthly_AdjPrice = map(Historical_Daily_Price,  ~..1 %>%
                                  tq_transmute(select = close,
                                               mutate_fun = to.monthly,
                                               indexAt = "lastof"))) %>%
  select(-price) %>% # Remove duplicated column
  inner_join(segments, by = "symbol") %>% #Enrich dataframe with segments data
  select(symbol:exchange, industry:sector, everything(), -companyName) #Select and organized data needed


glimpse(HistoricalPrices)



# 3. Overview of Data -----------------------------------------------------


# 3.1 Industry & Sector ---------------------------------------------------

#Sectors
p1 <- segments %>%
  mutate(industry = fct_rev(fct_infreq(sector))) %>%
  ggplot() +
  aes(x = industry, fill = sector) +
  geom_bar() +
  coord_flip() +
  scale_fill_hue() +
  guides(fill = "none") + 
  theme_minimal()+
  labs(title = "Companies", subtitle = "per sector", y = "Companies", x = "Sector")

p2 <- segments %>%
  mutate(industry = fct_rev(fct_infreq(industry))) %>%
  ggplot() +
  aes(x = industry, fill = industry) +
  geom_bar() +
  coord_flip() +
  scale_fill_hue() +
  guides(fill = "none") + 
  theme_minimal() +
  labs(title = "Companies", subtitle = "per industry", y = "Companies")

p1 | p2


#industry
p1 <- PriceSectors %>%
  mutate(industry = fct_reorder(sector, price)) %>%
  ggplot() +
  aes(x = industry, y = price, fill = sector) + 
  geom_boxplot() +
  scale_y_log10() +
  coord_flip() +
  scale_fill_hue() +
  guides(fill = "none") + 
  theme_minimal() +
  labs(title = "Sector", subtitle = "Per log of price", x = "Sector")

p2 <- PriceSectors %>%
  mutate(industry = fct_reorder(industry, price)) %>%
  ggplot() +
  aes(x = industry, y = price, fill = industry) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip() +
  scale_fill_hue() +
  guides(fill = "none") + 
  theme_minimal()+
  labs(title = "Industry", subtitle = "Per log of price")

p1 | p2



#TopBottom companies
p1 <- PriceSectors %>%
  mutate(price = as.double(price)) %>%
  arrange(-price) %>%
  head(20) %>%
  mutate(symbol = fct_reorder(symbol, price)) %>%
  ggplot() +
  aes(x = symbol, y = price, fill = sector) + 
  geom_col() +
  coord_flip() +
  scale_fill_hue() +
  theme_minimal() +
  labs(title = "Top 20 companies", subtitle = "per price")

p2 <- PriceSectors %>%
  mutate(price = as.double(price)) %>%
  filter(price > 0) %>%
  arrange(price) %>%
  head(20) %>%
  mutate(symbol = fct_rev(fct_reorder(symbol, price))) %>%
  ggplot() +
  aes(x = symbol, y = price, fill = sector) + 
  geom_col() +
  coord_flip() +
  scale_fill_hue() +
  theme_minimal() +
  labs(title = "Bottom 20 companies", subtitle = "per price")

p1 | p2



#Risk

p1 <- PriceSectors %>%
  mutate(beta = as.double(beta)) %>%
  mutate(industry = fct_reorder(sector, beta)) %>%
  ggplot() +
  aes(x = industry, y = beta, fill = sector) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_hue() +
  guides(fill = "none") + 
  theme_minimal() +
  labs(title = "Sector", subtitle = "Per risk (Beta)", x = "Sector")

p2 <- PriceSectors %>%
  mutate(beta = as.double(beta)) %>%
  mutate(industry = fct_reorder(industry, beta)) %>%
  ggplot() +
  aes(x = industry, y = beta, fill = industry) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_hue() +
  guides(fill = "none") + 
  theme_minimal() +
  labs(title = "Industry", subtitle = "Per risk (Beta)")


p1 | p2



# 3.2 KeyMetrics ----------------------------------------------------------

plots <- function(data, metric, Explanation, Formula){
  ggplot(data) +
    aes(x = sector, y = value, fill = sector) +
    geom_boxplot() +
    scale_fill_hue() +
    scale_y_continuous(trans = "log10") +
    theme_minimal() +
    coord_flip() + 
    guides(fill = "none") + 
    labs(title = metric, subtitle = Explanation, caption = paste0("Formula: ",Formula))
}

KeyMetrics <- KeyMetrics %>%
  mutate(plot = map(data, ~plots(..1, metric, str_wrap(Explanation,80), Formula)))


walk(KeyMetrics$plot, plot)


#Correlation
KeyMetrics %>%
  unnest(data) %>% 
  ungroup() %>% #removes grouped data, otherwise select will bring grouped atributes as well
  select(sector, metric, symbol, date,  value) %>% # select variables needed to spread
  spread(key = metric, value = value) %>% # spread metrics to column that will be correlated
  select(-sector, -symbol, -date) %>% # remove columns not needed
  drop_na() %>% #Remove any na on metrics data, to fix correlation function return NA
  cor() %>% # Apply correlation function
  as.data.frame() %>% # Convert matrix class to data frame 
  rownames_to_column("Metric") %>% # Include row names id from matrix to a column named data frame
  gather( "metric", "correlation", -Metric) %>% # gather all correlation into a single column
  filter(Metric != metric) %>% # Remove any metrics equal (That results in correlation 1)
  arrange(-correlation) %>% # Arrange correlation, this will be used in id creation later
  filter(correlation >= 0.8) %>% # Filter only correlations greater than 0.8
  mutate(id = case_when(Metric == lag(metric, 1) ~ 1, TRUE ~ 0)) %>% # Column created to remove duplicates of metrics x metrics
  filter(id == 1) %>% # Removing duplicates
  select(-id) %>% # Removing aux column
  kable(caption = "Correlation of metrics greather than 80%") %>%
  kable_styling(full_width = F)



# 3.3 Price ---------------------------------------------------------------

#Candlestick for SP500
HistoricalPrices %>%
  select(sector, Historical_Daily_Price) %>%
  unnest() %>%
  group_by(date) %>%
  summarise(close = mean(close), open = mean(open), low = mean(low), high = mean(high)) %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "SP500 Candlestick Chart", 
       subtitle = "Mean OHLC per sector",
       y = "Closing Price", x = "") + 
  theme_tq()



#Candlestick per sector
Sector_Daily_OHLC <- HistoricalPrices %>%
  select(sector, Historical_Daily_Price) %>%
  unnest() %>%
  group_by(sector,date) %>%
  summarise(close = mean(close), open = mean(open), low = mean(low), high = mean(high))

Sector_Daily_OHLC %>%
  ggplot(aes(x = date, y = close, group = sector)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "Sectors Candlestick Chart", 
       subtitle = "Mean OHLC per sector",
       y = "Closing Price", x = "") + 
  facet_wrap(~ sector, ncol = 4, scale = "free_y") + 
  theme_tq()



#Clustering
Clustering <- function(Cluster_DF, Df_aux){
  require(gridExtra)
  require(ggdendro)
  require(zoo)
  require(purrr)
  require(tsibble)
  require(broom)
  
  
  # Clustering
  hc <- hclust(dist(t(Df_aux[,-1])), "ave")
  
  # 8.1 DF clusters
  library(factoextra)
  NbClust <- fviz_nbclust(Df_aux[,-1], FUN = hcut, method = "silhouette")
  
  
  k <- which.max(NbClust$data$y)
  
  cut_avg <- cutree(hc, k = k) %>%
    tidy() %>%
    rename("Data"="names", "cluster"="x") 
  
  
  # Number of clusters plot
  NbClustersPlot <- plot(NbClust)
  
  ### Plot
  hcdata <- dendro_data(hc)
  names_order <- hcdata$labels$label
  
  # Use the folloing to remove labels from dendogram so not doubling up - but good for checking
  hcdata$labels$label <- ''
  p1 <- ggdendrogram(hcdata, rotate=TRUE, leaf_labels=FALSE)
  
  # Autoplot only accepts time series data type
  Zoo_DF <- read.zoo(Df_aux)
  
  # Scale the time series and plot
  maxs <- apply(Zoo_DF, 2, max)
  mins <- apply(Zoo_DF, 2, min)
  joined_ts_scales <- scale(Zoo_DF, center = mins, scale = maxs - mins)
  
  new_data <- joined_ts_scales[,rev(as.character(names_order))]
  
  p2 <- autoplot(new_data, facets = Series ~ . ) + 
    xlab('') + ylab('') + theme(legend.position="none")
  
  gp1<-ggplotGrob(p1)
  gp2<-ggplotGrob(p2) 
  
  grid <- grid.arrange(gp2, gp1, ncol=2, widths=c(4,2))
  
  
  aux <- data.frame(Model_Name = Cluster_DF) %>%
    mutate(Clustered = purrr::map(Model_Name, ~cut_avg),
           hc = purrr::map(Model_Name, ~hc),
           NbClust= purrr::map(Model_Name, ~NbClust),
           NbClustersPlot= purrr::map(Model_Name, ~NbClustersPlot),
           p1= purrr::map(Model_Name, ~p1),
           p2= purrr::map(Model_Name, ~p2),
           grid = purrr::map(Model_Name, ~grid)
    )
  
  
  return(aux)
}


Clust_DF <- Sector_Daily_OHLC %>%
  select(sector, date, close) %>%
  spread(sector, close) %>%
  filter_all(all_vars(!is.na(.)))

Clusters <- Clustering("Sectors", Clust_DF)


Clusters_sectors <- map_dfr(Clusters$Clustered, ~..1) %>%
  rename("sector"=Data) %>%
  arrange(cluster)


kable(Clusters_sectors, caption = "Sector Clusters") %>%
  kable_styling(full_width = F)


#Annual returns per sectors
HistoricalPrices %>%
  unnest(Monthly_AdjPrice) %>%
  group_by(sector) %>%
  tq_transmute(select = close, mutate_fun = periodReturn, period = "yearly", type = "arithmetic") %>%
  ggplot(aes(x = date, y = yearly.returns, fill = sector)) +
  geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sectors: Annual Returns",
       y = "Annual Returns", x = "") + 
  facet_wrap(~ sector, ncol = 4, scales = "free_y") +
  theme_tq() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") + 
  scale_fill_tq()



#Quaterly max min per sector
Sector_max_by_qtr <- HistoricalPrices %>%
  unnest(Historical_Daily_Price) %>%
  group_by(sector) %>%
  tq_transmute(select = close, mutate_fun = apply.quarterly, FUN= max,
               col_rename = "max.close") %>%
  mutate(year.qtr = paste0(lubridate::year(date), "-Q",
                           lubridate::quarter(date))) %>%
  select(-date)



Sector_min_by_qtr <- HistoricalPrices %>%
  unnest(Historical_Daily_Price) %>%
  group_by(sector) %>%
  tq_transmute(select = close, mutate_fun = apply.quarterly,
               FUN= min, col_rename = "min.close") %>%
  mutate(year.qtr = paste0(lubridate::year(date), "-Q",
                           lubridate::quarter(date))) %>%
  select(-date)

Sector_by_qtr <- left_join(Sector_max_by_qtr, Sector_min_by_qtr,
                           by = c("sector" = "sector", "year.qtr" = "year.qtr"))


Sector_by_qtr %>%
  ggplot(aes(x = year.qtr, color = sector)) +
  geom_segment(aes(xend = year.qtr, y = min.close, yend = max.close),
               size = 1) +
  geom_point(aes(y = max.close), size = 2) +
  geom_point(aes(y = min.close), size = 2) +
  facet_wrap(~ sector, ncol =4, scale = "free_y") +
  labs(title = "Sector: Min/Max Price By Quarter",
       y = "Stock Price", color = "") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "none")




# 4. Portfolio Analysis ---------------------------------------------------

# 4.1 Sectors -------------------------------------------------------------

Sector_Returns <- Sector_Daily_OHLC %>%
  group_by(sector) %>% 
  tq_transmute(select     = close, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "close") %>%
  spread(sector, close) %>%
  filter_all(all_vars(!is.na(.))) %>%
  tk_xts(data = ., date_var = date, silent = TRUE)  #Its needed to run porfolio.spec

charts.PerformanceSummary(Sector_Returns,main = "Sectors Performance", legend.loc = NULL)


Optimize <- function(Returns){
  
  # Create the portfolio specification
  port_spec <- portfolio.spec(colnames(Returns)) %>%
    
    # Add a full investment constraint such that the weights sum to 1
    add.constraint(portfolio = ., type = "full_investment") %>%
    
    # Add a long only constraint such that the weight of an asset is between 0 and 1
    add.constraint(portfolio = ., type = "long_only") %>%
    
    # Add an objective to minimize portfolio standard deviation
    add.objective(portfolio = ., type = "risk", name = "StdDev")
  
  # Solve the optimization problem
  opt <- optimize.portfolio(Returns, portfolio = port_spec,
                            optimize_method = "ROI", trace=TRUE)
  
  
  return(opt)
  
}


SectorReturns <- Optimize(Sector_Returns) %>%
  extractWeights() %>%
  data.frame(Name = names(.), weights = round(.,3), row.names = NULL) %>%
  select(-.)



#Plot returns weight
plots2 <- function(weights, sector=NULL){
  
  plot <- weights %>%
    mutate(Name = fct_reorder(Name, weights)) %>%
    filter(weights > 0.01) %>%
    ggplot(aes(x = Name, y=weights, fill = Name)) +
    geom_col() + 
    scale_fill_brewer(palette = "RdBu") +
    theme_minimal() +
    coord_flip() + 
    guides(fill = "none") +
    labs(title = paste0("Sectors ", sector))
  
  return(plot)
  
}

plots2(SectorReturns)




# 4.2 Companies per sector ------------------------------------------------

#Since we have to model this grouped per sector, all this piece of script is doing is merging the same daily period return close and converting it to a xts date time based data grouped by sector with purrr and allowing symbol companies inside each sector list
Symbol_Returns <- HistoricalPrices %>%
  select(symbol,sector, Historical_Daily_Price) %>%
  unnest() %>%
  group_by(sector) %>%
  nest() %>%
  mutate(data = map(data, ~..1 %>%
                      select(symbol, date, close) %>%
                      group_by(symbol) %>%
                      tq_transmute(select     = close, 
                                   mutate_fun = periodReturn, 
                                   period     = "daily", 
                                   col_rename = "close") %>%
                      spread(symbol, close) %>%
                      filter_all(all_vars(!is.na(.))) %>%
                      tk_xts(data = ., date_var = date, silent = TRUE)))

#Optimizing per each purrr list of sectors
Symbol_Returns <- Symbol_Returns %>%
  mutate(optimize = map(data, ~Optimize(..1)))

#Simple extract weights and organizing it to be able to plot
Symbol_Returns <- Symbol_Returns %>%
  mutate(weights = map(optimize, extractWeights),
         weights = map(weights, ~data.frame(Name = names(..1),
                                            weights = ..1, row.names = NULL)))


#Extracting worst and best symbols and ploting each sector weigths
Symbol_Returns <- Symbol_Returns %>%
  mutate(Best = map(weights, ~ filter(..1, weights == max(weights)) %>%
                      select(Name)),
         Worst = map(weights, ~ filter(..1, weights == min(weights)) %>%
                       select(Name)),
         plots = map(weights, ~plots2(..1, sector)))


walk(Symbol_Returns$plots, plot)



# 5. KeyMetrics x Porfolio Mitigation -------------------------------------

# 5.1 Best x Worst companies per sector -----------------------------------

#Organizing best and worst data to include in keyMetrics dataset
Symbol_Returns_pf <- Symbol_Returns %>%
  select(sector, Best, Worst) %>%
  unnest() %>%
  rename("Best"="Name", "Worst"="Name1")

# Join both best and worst companies inside the keymetrics dataframe
KeyMetrics <- KeyMetrics %>%
  mutate(data = map(data, ~..1 %>% inner_join(Symbol_Returns_pf, by = "sector")))

#Update the keymetrics plot to include both best and worst companies as geom_point and geom_label
plots3 <- function(data, metric, Explanation, Formula){
  
  Best <- filter(data, symbol == Best) %>%
    group_by(symbol, sector) %>%
    summarise(value = mean(value))
  
  Worst <- filter(data, symbol == Worst) %>%
    group_by(symbol, sector) %>%
    summarise(value = mean(value))
  
  
  ggplot(data, aes(x = sector, y = value, fill = sector)) +
    geom_boxplot() +
    geom_point(data = Best, aes(x = sector, y = value), colour = "blue") +
    geom_label(data=Best,aes(label=symbol), nudge_x = 0.3, nudge_y = 0.05,
               size = 2, fill = "grey", colour = "blue") +
    geom_point(data = Worst, aes(x = sector, y = value), colour = "red") +
    geom_label(data=Worst,aes(label=symbol), nudge_x = 0.3, nudge_y = 0.05,
               size = 2, fill = "grey", colour = "red") +
    scale_fill_hue() +
    scale_y_continuous(trans = "log10") +
    theme_minimal() +
    coord_flip() + 
    guides(fill = "none") + 
    labs(title = metric, subtitle = Explanation, caption = paste0("Formula: ",Formula))
}


#Apply the function
KeyMetrics <- KeyMetrics %>%
  mutate(plot2 = map(data, ~plots3(..1, metric, str_wrap(Explanation,80), Formula)))


kable(Symbol_Returns_pf, caption = "Best and Worst company per Porfolio risk mitigation") %>%
  kable_styling(full_width = F)



# 5.2 Plots ---------------------------------------------------------------

walk(KeyMetrics$plot2, plot)


# 5.3 Model & Results -----------------------------------------------------

#Part 1
ListGroups <- Symbol_Returns %>%
  select(sector, weights) %>%
  unnest() %>%
  rename("symbol"="Name") #Rename to make it easier for innner join

# Join weight symbols, remove sector from nested data and nest it again...
KeyMetrics <- KeyMetrics %>%
  mutate(data2 = map(data, ~..1 %>%
                       inner_join(ListGroups, by = c("sector","symbol"))))


# Part 2

#Let's create a function with h2o to help us model per sector

h2o.init(max_mem_size = "5g")

h2o.no_progress()

H2o_Model <- function(Data,x, y){
  require(h2o)
  require(tidyverse)
  require(purrr)
  
  Data$sector <- as.factor(Data$sector)
  
  Data_h2o <- as.h2o(Data)
  
  set.seed(123)
  
  automl_glm <- h2o.glm(
    x = x, 
    y = y,
    training_frame = Data_h2o)
  
  Name_Model <- "H2O_GLM"
  
  coef <- h2o.coef(automl_glm) %>%
    as.data.frame()
  
  #Model summary
  CV_Summary <- h2o.performance(automl_glm)
  
  #lime
  # explainer <- lime(x = Data[,x],
  # model = automl_glm)
  
  # explanation <- explain(x = Data[,x], explainer = explainer, bin_continuous = TRUE,
  # feature_select = "auto", n_features = 2)
  
  # Features_Plot <- plot_features(explanation,cases = 1)
  
  aux <- data.frame(Model_Name = Name_Model) %>%
    mutate(Model = map(Model_Name, ~automl_glm),
           coefs = map(Model_Name, ~coef),
           CV_Summary = map(Model_Name, ~CV_Summary)#,
           #explanation = map(Model_Name, ~explanation),
           #Features_Plot = map(Model_Name, ~Features_Plot)
    )
  
  
  return(aux)
  
}


# Let's model
KeyMetrics <- KeyMetrics %>%
  mutate(H2o_Model = map(data2, ~H2o_Model(..1, x = c("value","sector"),
                                           y = "weights")))


h2o.shutdown(prompt = F)


#Organize coeficients for kable
Coefiecients <- KeyMetrics %>%
  mutate(coefs = map(H2o_Model, ~..1$coefs %>%
                       reduce(as.data.frame) %>%
                       rownames_to_column(var = "Parameter"))) %>%
  ungroup() %>%
  select(metric, coefs) %>%
  unnest(coefs) %>%
  rename("value" = ".") %>%
  mutate(Parameter = gsub(Parameter, pattern = "sector.", replacement = "")) %>%
  spread(key = Parameter, value = value) %>%
  select(metric, Intercept, value, everything()) %>%
  arrange(-Intercept, -value)


kable(Coefiecients, caption = "Metrics coeficients per sectors", digits = 3) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 10)

