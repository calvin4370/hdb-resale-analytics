# HDB Resale Price Prediction
![R](https://img.shields.io/badge/Language-R-276DC3)
![Shiny](https://img.shields.io/badge/Framework-Shiny_(bslib)-blue)
![XGBoost](https://img.shields.io/badge/Model-XGBoost-orange)
![Status](https://img.shields.io/badge/Status-Deployed_on_ShinyApps.io-success)

An end-to-end predictive analytics project investigating 200,000+ HDB resale transactions (2017-2025). The workflow includes data cleaning, geocoding via OneMap API, geospatial feature engineering, and exploratory data analysis, culminating in a production-grade valuation model.

The final **XGBoost** model achieves an **$R^2$ of 0.976** and an **RMSE of $28.7k**, reducing prediction error by **~46%** compared to the baseline OLS linear regression model.

### **[Try the Shiny App](https://chan-jun-jie.shinyapps.io/hdb-resale-price-prediction/)**


## Problem Statement
Estimating the resale value of an HDB flat in Singapore is difficult due to non-linear factors like storey level, distances from MRT / CBD and interaction effects between factors. This project allows potential buyers and sellers of HDB flats elligible for resale to get an instant, data-driven prediction for the resale price of their flat.

Unlike traditional linear models such as OLS  and Ridge regression, this application utilises **XGBoost (Extreme Gradient Boosting)** to capture non-linear pricing dynamics and interaction effects between features automatically.

## Dataset
1.  **HDB Resale Prices: raw_resale_prices.csv**
    * *Source:* [Data.gov.sg](https://data.gov.sg/)
    * *Dataset:* Resale flat prices based on registration date from Jan-2017 onwards.

2.  **Coordinates of HDB addreses: hdb_coordinates.csv**
    * *Source:* [OneMap API](https://www.onemap.gov.sg/docs/)
    * Generated csv by calling the OneMap API to obtain coordinate (lat / long) info of all HDB addresses

3.  **MRT Station Locations: mrt_lrt_stations.csv**
    * *Source:* [Kaggle - MRT & LRT Stations in Singapore](https://www.kaggle.com/datasets/lzytim/full-list-of-mrt-and-lrt-stations-in-singapore)

## Project Workflow

## Models Implemented

## Model Comparison

## Tech Stack

## How to Run the Project