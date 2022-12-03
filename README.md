# Data Description
**data_complete.xlsx** contains all the data from 2019-01-01 to 2021-12-31. It has 1927 non-null observations.

**data_complete_2021_11.xlsx** contains data only from 2021-11. It has 448 observations, the most out of any months.



### They both have the following structure:
## Company Information
**Reference**: https://wrds-www.wharton.upenn.edu/pages/get-data/center-research-security-prices-crsp/monthly-update/stock-version-2/monthly-stock-file/

***ticker***: Ticker of the company

***date***: Date of the data

***icb_industry_code***: ICB industry code of the company. (https://www.ftserussell.com/data/industry-classification-benchmark-icb)

***monthly_return***: Monthly return of the company

## Market(SPY) Data
**Reference**: https://wrds-www.wharton.upenn.edu/pages/get-data/center-research-security-prices-crsp/monthly-update/stock-version-2/monthly-stock-file/

***snp_monthly_return***: Monthly return of S&P 500

## Bond Data
**Reference**: https://wrds-www.wharton.upenn.edu/pages/get-data/center-research-security-prices-crsp/monthly-update/index-treasury-and-inflation/us-treasury-and-inflation-indexes/

***ten_yr_ytm_rf***: 10-year treasury yield-to-maturity (risk-free rate)

## Fama-French Betas
**Reference**: https://wrds-www.wharton.upenn.edu/documents/1582/WRDS_Beta_Suite_Documentation_3T4EcS7.pdf

***b_mkt_fama_french_3fac***: Fama-French 3-factor beta of the company


***b_smb_fama_french_3fac***: Fama-French 3-factor SMB beta of the company

***b_hml_fama_french_3fac***: Fama-French 3-factor HML beta of the company

***excess_return_fama_french_3fac***: Fama-French 3-factor excess return of the company

## CAPM Betas
**Reference**: https://wrds-www.wharton.upenn.edu/documents/1582/WRDS_Beta_Suite_Documentation_3T4EcS7.pdf

***b_mkt_capm_mkt***: CAPM beta of the company

***excess_return_capm***: CAPM excess return of the company

## Company Financial Ratios
**Reference**:https://wrds-www.wharton.upenn.edu/pages/get-data/financial-ratios-suite-wrds/financial-ratios/financial-ratios-firm-level-by-wrds-beta/

***monthly_mktcap***: Monthly market capitalization of the company

***monthly_price_volume***: Monthly price volume of the company

***book_to_market***: Book to market ratio of the company

***capital_ratio***: Capital ratio of the company

***enterprise_value_multiple***: Enterprise value multiple of the company

***price_to_book***: Price to book ratio of the company

***return_on_equity***: Return on equity of the company

***inventory_current_asset_frac***: Inventory current asset fraction of the company

***receivable_current_asset_frac***: Receivable current asset fraction of the company

***cash_ratio***: Cash ratio of the company

***effective_tax_rate***: Effective tax rate of the company

***debt_assets***: Debt assets ratio of the company

## ESG Scores
**Reference**: https://wrds-www.wharton.upenn.edu/pages/get-data/compustat-capital-iq-standard-poors/trucost/esg-scores/esg-daily-scores/

***Economic Governance Dimension***: Economic governance dimension of the company

***Environmental Dimension***: Environmental dimension of the company

***S&P Global ESG Score***: S&P Global General ESG Score of the company

***Social Dimension***: Social dimension score of the company


## Usage (deprecated)

***data_price***: company-specific information (market cap, monthly return, etc).

***esg_score_finished***: ESG scores after cleaning. 

***data_bond***: bonds data.

***fama_french_betas***: fama-french betas.

***capm_betas***: CAPM betas

***data_complete_missing_beta***: All months all df merged except fama-french and CAPM beta. 

***data_complete_missing_capm_beta***: All months all df merged except CAPM beta. 

***data_complete***:: All months all df above merged.

***data_complete_2021_11***:: 2021-11, with all df above merged.

## (deprecated) Usage 

***df***: original company-level data from refinitv. Extracted from ***stock_data_super_large.xlsx***

***df_risk_free***: original 10-yr US yield data from St. Louis Fed. Extracted from ***10_year_yield_US.xlsx***

***df_mkt***: original price data for SPDR S&P 500 from refinitiv. Extracted from ***SPY_data_final.xlsx***

***df_mutual***:original price data for US equity mutual funds from WRDS+Refinitiv. Extracted from ***mutual_fund_returns_WRDS.xlsx***

***df_spread***: This is the final df that you want to run regression on. This is outputted in ***fama-french_full.xlsx*** if you want to use R or excel for further analyses. 

## Notes

*CM* stands for calendar month, with CM1 being October 2022, CM96 being October 2014. 