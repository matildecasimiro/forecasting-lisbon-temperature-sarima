# Lisbon Temperature Forecasting using Box-Jenkins Methodology

This project applies the Box-Jenkins methodology to analyze and forecast monthly temperatures in Lisbon between 1995 and 2020.

## Overview

The analysis follows the complete Box-Jenkins framework:

* Time series exploration and visualization
* Stationarity testing (ADF Test)
* Seasonal differencing
* ACF and PACF analysis
* SARIMA model selection
* Residual diagnostics
* Forecast evaluation

## Results

Several SARIMA models were compared using AIC, BIC, and forecast accuracy metrics. The best-performing model was:

**SARIMA(1,0,0)(1,1,1)**

The model successfully captured the seasonal behavior of Lisbon's temperatures and produced the most accurate forecasts on the test set.

## Technologies

* R
* tsibble
* fable
* forecast
* ggplot2
* tidyverse

## Repository Structure

```text
.
├── data/
├── notebooks/
└── README.md
```

## Skills Demonstrated

* Time Series Analysis
* Forecasting
* Box-Jenkins Methodology
* SARIMA Models
* Statistical Testing
* Model Evaluation
