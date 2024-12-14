setwd("C:/Users/vk0589/OneDrive - UNT System/Documents/UNT/INFO_5307/Datasets")
#install.packages("tidyverse")
#read.csv("OnlineRetail.csv")
library(readxl)
data1 <- read_excel("OnlineRetail.xlsx")
data2 <- as.data.frame(data1)
# head(data2)
summary(data2)
# Transforming Data for Plotting #####
# Renaming the Country Names
data2$Country[data2$Country == "United Kingdom"] = "UK"
data2$Country[data2$Country == "EIRE"] = "Ireland"
data2$Country[data2$Country == "RSA"] = "South Africa"
data2$Country[data2$Country == "Hong Kong"] = "China"

# Changing characters to factors
data2$InvoiceNo <- as.factor(data2$InvoiceNo)
data2$Description <- as.factor(data2$Description)
data2$CustomerID <- as.factor(data2$CustomerID)
data2$Description <- as.factor(data2$Description)
data2$Country <- as.factor(data2$Country)
data2$StockCode <- as.factor(data2$StockCode)
# summary(data2)

# World Map ####
# install.packages("tidyverse")
# install.packages("maps")
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

# creating variable for World Map
world <-map_data("world") 

# Plotting Countries with ggplot on World Map
# Following Map indicates the data available in OnlineRetail.xlsx dataset
# Changing to factor
world %>%
  left_join(data2, by = c("region" = "Country")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Quantity)) + 
  geom_polygon(color = "#DFDFDF", size = 0.1) +
  scale_fill_gradient(low = "yellow", high = "red" )

# Findings #####
library(tidyverse)
library(ggplot2)
library(dplyr)

# Observations summary based on Country (Count)
summary(data2$Country)
# Purchase Time v/s Country
data2%>%
  ggplot(aes(InvoiceDate, Country))+
  geom_point(color = "#1D55B6", size = 1) + 
  labs(x = "Time of Purchase", y = "Country",
       title = "Purchase Frequency of All Countries")

# Purchase Time v/s Quantity for whole Data:
qplot(x = InvoiceDate, y = Quantity,
      data = data2,
      xlab = "PurchaseTime",
      ylab = "Quantity Purchased",
      main = "PurchaseTime v/s Quantity Purchased (Whole Data)")

# Purchase Time v/s Price Spent for whole Data:
qplot(x = InvoiceDate, y = Quantity*UnitPrice,
      data = data2,
      xlab = "PurchaseTime",
      ylab = "Total Amount Spent",
      main = "PurchaseTime v/s Total Amount (Whole Data)")


# Tests based on Countries
# 1) USA
filter(data2, Country == "USA") -> us_data
summary(us_data)
# qplot(x = InvoiceDate, y = Quantity, data = us_data, color = CustomerID)
# qplot(x = InvoiceDate, y = Quantity, data = us_data, color = InvoiceNo)
qplot(x = InvoiceDate, y = Quantity, 
      data = us_data, 
      xlab = "Month of Purchase (Per each Customers)",
      ylab = "Quantity",
      main = "Purchase Month v/s Quantity",
      color = InvoiceNo, # Coloring Points based on 'InvoiceNo'
      facets = ~ CustomerID) # Separated Graphs for CustomerID

# 2) France
filter(data2, Country == "France") -> frc_data
summary(frc_data)
qplot(x = InvoiceDate, y = Quantity, 
      xlab = "Month of Purchase",
      ylab = "Quantity",
      main = "Purchase Month v/s Quantity",
      data = frc_data,
      color = InvoiceNo)
#color = CustomerID - FAILURE - Lot of Customers - Inappropriate Graph

# 3) Spain
filter(data2, Country == "Spain") -> spn_data
summary(spn_data)
qplot(x = InvoiceDate, y = Quantity, 
      data = spn_data,
      xlab = "Month of Purchase",
      ylab = "Quantity",
      main = "Purchase Month v/s Quantity",
      color = CustomerID) #color = CustomerID - MANAGABLE - 31 Customers

# 4) Japan  
filter(data2, Country == "Japan") -> jpn_data
summary(jpn_data)
# qplot(x = InvoiceDate, y = Quantity*UnitPrice, data = jpn_data, color = CustomerID)
# 8 Customers
qplot(x = InvoiceDate, y = Quantity*UnitPrice, 
      data = jpn_data,
      xlab = "Month of Purchase",
      ylab = "Total Price",
      main = "Purchase Month v/s Total Price (Per Each Customer)",
      color = InvoiceNo, 
      facets = ~ CustomerID)
# TotalPrice (UnitPrice x Quantity)

# 5) Italy 
filter(data2, Country == "Italy") -> ity_data
summary(ity_data)
qplot(x = InvoiceDate, y = Quantity*UnitPrice, 
      data = ity_data,
      xlab = "Month of Purchase",
      ylab = "Total Price",
      main = "Purchase Month v/s Total Price",
      color = CustomerID)

# 6) Canada 
filter(data2, Country == "Canada") -> cnd_data
summary(cnd_data)
# qplot(x = InvoiceDate, y = Quantity*UnitPrice, data = cnd_data, color = CustomerID)
qplot(x = InvoiceDate, y = Quantity*UnitPrice, 
      data = cnd_data, 
      xlab = "Month of Purchase",
      ylab = "Total Price",
      main = "Purchase Month v/s Total Price (Per Each Customer)",
      color = InvoiceNo, 
      facets = ~ CustomerID)

# 7) Unspecified 
filter(data2, Country == "Unspecified") -> unsp_data
summary(unsp_data)
# qplot(x = InvoiceDate, y = Quantity*UnitPrice, data = unsp_data, color = CustomerID)
qplot(x = InvoiceDate, y = Quantity*UnitPrice, 
      data = unsp_data,
      xlab = "Month of Purchase",
      ylab = "Total Price",
      main = "Purchase Month v/s Total Price (Per Each Customer)",
      color = InvoiceNo, 
      facets = ~ CustomerID)
# Last graph consists of the data of the Invoices where Country name was not mentioned
