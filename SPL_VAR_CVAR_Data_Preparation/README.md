<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: SPL_VAR_CVAR_Data_Preparation

Published in: Statistical programming languages - Student Project on ''Applied Value at Risk and Conditional Value at Risk''

Description: Creates a function to automatically provide historical stock price data for the VaR and CVaR analysis. After downloading time series data for the required stocks from the internet, they are converted into data frame and all occurring NA observations are imputed. Currency effects are adjusted for american stocks. Finally, the returns of single stocks and those of a hypothetical portfolio are calculated.

Keywords: finance, stock prizes, imputation, currency effects, moving weights, portfolio returns

Author: Marvin Gauer, Sandra Faltl, Michael Schimpke, Laureen Lake, Anna Friesen

See also: SPL_VAR_CVAR_Vizu, SPL_VAR_CVAR_Testing

Submitted: Tue, March 14 2017 by Michael Schimpke

Input: Stock price data from Yahoo Finance Currency exchange rate data from Oanda

Output: Function output list containing used stocks and weights, log-returns of single stock, log-returns and normalized log-returns of hypothetical portfolio, yearly log-returns of portfolio.

```
