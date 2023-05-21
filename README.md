# GARCH Modeling Application

This project is a GARCH Modeling Application implemented in R using the Shiny framework. The application allows users to input a stock ticker symbol and generates GARCH models to forecast future volatility. It provides visualizations of volatility forecasts and a table with forecasted values for different GARCH models.

## Methodology

The GARCH (Generalized Autoregressive Conditional Heteroskedasticity) models are fitted to historical stock return data using the "quantmod" and "rugarch" packages. These models capture the volatility clustering and persistence observed in financial markets. The application utilizes various GARCH models, including sGARCH, eGARCH, gjrGARCH, apARCH, and iGARCH.

### GARCH Models

- **sGARCH (Standard GARCH)**: The sGARCH model assumes that volatility is driven by past squared errors or innovations.
- **eGARCH (Exponential GARCH)**: The eGARCH model captures the asymmetric response of volatility to positive and negative shocks.
- **gjrGARCH (Generalized Autoregressive Conditional Heteroskedasticity)**: The gjrGARCH model extends the sGARCH model to incorporate an additional parameter for asymmetric volatility response.
- **apARCH (Asymmetric Power Autoregressive Conditional Heteroskedasticity)**: The apARCH model allows for asymmetric volatility responses and utilizes a power transformation on the past innovations.
- **iGARCH (Integrated GARCH)**: The iGARCH model includes an additional term for the lagged conditional variance, capturing the persistence in volatility.

### Parameters in GARCH Models

In GARCH modeling, the parameters p, d, and q are used to specify the orders of the autoregressive (AR), integrated (I), and moving average (MA) components, respectively.

- **p (AR Order)**: The autoregressive order determines the number of lagged conditional variances to include in the model. A higher value of p captures more complex patterns in volatility persistence. However, using too many lagged variances can lead to overfitting.

- **d (I Order)**: The integrated order refers to the differencing order necessary to achieve stationarity in the time series. A non-zero value of d indicates the presence of a unit root, and the series requires differencing to remove it. In GARCH models, d is typically set to 0 since the differencing is usually performed on the original time series before fitting the GARCH model.

- **q (MA Order)**: The moving average order determines the number of lagged residuals (or innovations) to include in the model. The residuals represent the unexpected or unexplained part of the series. Including higher values of q allows capturing the short-term volatility dynamics. However, similar to the autoregressive order, using too many lagged residuals can result in overfitting.

The appropriate selection of p, d, and q depends on the characteristics of the time series being modeled. It is common to use model selection techniques such as information criteria (e.g., AIC, BIC) and diagnostic tests (e.g., residual analysis) to determine the optimal values for these parameters.

## Features

- Input a stock ticker symbol to fetch historical data.
- Run GARCH models and generate volatility forecasts.
- Visualize volatility forecasts using a line plot.
- View forecasted volatility values in a table.
- Compare different GARCH models for volatility forecasting.

## Future Work

The application can be enhanced with additional features, such as:
- Allowing users to select different time periods for historical data.
- Providing more customization options for the volatility plot.
- Adding more advanced GARCH models for volatility forecasting.

## Disclaimer

This application is for informational purposes only and does not constitute financial advice. The accuracy and reliability of the volatility forecasts are subject to various factors and should not be solely relied upon for investment decisions.

## Contact Information

For any questions or feedback, please contact:

- Seung Soo (Joseph) Chae
- Email: jssc1025@gmail.com
- GitHub: [jssc1025](https://github.com/jssc1025)

