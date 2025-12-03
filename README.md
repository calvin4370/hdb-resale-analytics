# hdb-resale-analytics
A valuation model to predict HDB resale prices using linear regression

## 📁 Data Sources

1.  **HDB Resale Prices: raw_resale_prices.csv**
    * *Source:* [Data.gov.sg](https://data.gov.sg/)
    * *Dataset:* Resale flat prices based on registration date from Jan-2017 onwards.

2.  **Coordinates of HDB addreses: hdb_coordinates.csv**
    * *Source:* [OneMap API](https://www.onemap.gov.sg/docs/)
    * Generated csv by calling the OneMap API to obtain coordinate (lat / long) info of all HDB addresses

3.  **MRT Station Locations: mrt_lrt_stations.csv**
    * *Source:* [Kaggle - MRT & LRT Stations in Singapore](https://www.kaggle.com/datasets/lzytim/full-list-of-mrt-and-lrt-stations-in-singapore)