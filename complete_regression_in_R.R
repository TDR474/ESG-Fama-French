library("corrplot")
library("lmtest")
library("sandwich")
library("readxl")
library("stargazer")
library("lessR")
library("ggplot2")
library("dplyr")
library(e1071)
library(data.table)
library(car)
library(caTools)
library(mctest)
library("writexl")

library(pedometrics)data_2021_renamed

setwd(dir = "/Users/frankzhu/Desktop/Metrics_GP/project-draft/")

# read in excel 
data <- read_excel("final.xls")


# data <- read_excel("data_analysis_beta.xlsx")
# Possible transformation
data$ESGRATIO = data$SPGlobalESGScore / (mean(data$SPGlobalESGScore))
data$mktcapm = data$mktcap / 1000000
data$yearly_return_adjusted = data$yearly_return - (2.1/100)
data
# With mktcap, book_to_market1, b_mkt_fama_french_3fac1, SPGlobalESGScore, and interaction between industry and SPGlobalESGScore
# dependent variable is yearly_return

# partition the data
data_2021 <- data[data$year == 2021, ]
data_2020 <- data[data$year == 2020, ]
data_2019 <- data[data$year == 2019, ]



## Renaming for presentation
data_2021_renamed <- data_2021 %>% 
  rename(
    "Company_Excess_Return" = "yearly_return_adjusted",
    "Market_Cap" = "mktcapm",
    "Book_to_Market" = "book_to_market1",
    "Shares_Turnover" = "shares_turnover",
    "Accrual" = "yearly_accrual",
    "FF_Market_Beta" = "b_mkt_fama_french_3fac1",
    "Industry_" = "industry_",
    "Governance_Dimension" = "EconomicGovernanceDimensionw",
    "Environmental_Dimension" = "EnvironmentalDimensionw",
    "Social_Dimension" = "SocialDimensionw")


# Summary Stats
data_2021_renamed_summary <- data_2021_renamed[c("Company_Excess_Return", "Market_Cap","Book_to_Market", "Shares_Turnover","Accrual", "FF_Market_Beta", "SPGlobalESGScore")]
data_2021_renamed_summary[data_2021_renamed_summary == 0] <- NA
st(data_2021_renamed_summary)
st(data_2021_renamed_summary,out='latex',file='data_2021_renamed_summary')




########################
# Final_regression_paper
reg0.5 <- lm(Company_Excess_Return ~ Market_Cap + Book_to_Market + FF_Market_Beta, data = data_2021_renamed)
reg0.6 <- lm(Company_Excess_Return ~ Market_Cap + Book_to_Market + FF_Market_Beta + SPGlobalESGScore, data = data_2021_renamed)
reg1.51 <- lm(Company_Excess_Return ~ Market_Cap + Book_to_Market + Shares_Turnover  + Accrual + FF_Market_Beta + SPGlobalESGScore, data = data_2021_renamed)
reg3.5 <- lm(Company_Excess_Return ~ Market_Cap + Book_to_Market  + Shares_Turnover + Accrual + FF_Market_Beta + SPGlobalESGScore  + Industry_, data = data_2021_renamed)


rob_se_final<- list(
  sqrt(diag(vcovHC(reg0.5, type = "HC1"))),
  sqrt(diag(vcovHC(reg0.6, type = "HC1"))),
  sqrt(diag(vcovHC(reg1.5, type = "HC1"))),
  sqrt(diag(vcovHC(reg3.5, type = "HC1")))
)

stargazer(reg0.5, reg0.6, reg1.5, reg3.5,
          se = rob_se_final,
          title = "ESG Regression Analysis Complete (2021)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2021Final.html")

# stargazer latex 

stargazer(reg1, reg1.5, reg1.51, reg3.5,
          se = rob_se_final,
          title = "ESG Regression Analysis",
          type ="latex",
          notes=c("Standard Errors are Heteroskedasticity Robust"))

#### Variance inflation factor 
viffac = vif(reg3.5)

# latex VIF
kbl(viffac, caption = "Variance Inflation Factors", booktabs = T,"latex") %>%
  kable_styling(latex_options = c("striped","hold_position"))%>%
  footnote(general = "VIF > 10 is often considered a sign for collinearity issues",threeparttable = T)

bptest(reg3.5)

stepVIF(reg3.5, threshold = 10, verbose = True)

# Correlation plot

data_2021_renamed_summary <- data_2021_renamed_summary[complete.cases(data_2021_renamed_summary), ] 
data_2021_renamed_summary <- na.omit(data_2021_renamed_summary)

corrplot(cor(data_2021_corr), addCoef.col = "black", tl.col = "black", tl.srt = 45)
corrplot(cor(data_2021_corr), method = "color", type = "upper", order = "hclust", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45, number.cex=0.75, diag = FALSE)


###################################################################
######################### More Regressions ############################
###################################################################


####
## For 2021 Analyses
####
# log version
reg1 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2021)
reg3 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data_2021)
reg5 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2021)

# By All Dimensions, including general
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + SPGlobalESGScore, data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + SPGlobalESGScore + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2021)


# By All Dimensions, excluding general
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension, data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + industry_ + industry_:SPGlobalESGScore, data = data_2021)

# By Environmental Dimension 
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EnvironmentalDimension , data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EnvironmentalDimension + industry_:EnvironmentalDimension, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	EnvironmentalDimension + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	EnvironmentalDimension + industry_ + industry_:EnvironmentalDimension, data = data_2021)

# By Social Dimension 
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SocialDimension , data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SocialDimension + industry_:SocialDimension, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	SocialDimension + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	SocialDimension + industry_ + industry_:SocialDimension, data = data_2021)

# By Governance Dimension 
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension , data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension + industry_:EconomicGovernanceDimension, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	EconomicGovernanceDimension + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	EconomicGovernanceDimension + industry_ + industry_:EconomicGovernanceDimension, data = data_2021)

# Normal Version - SMB & HML
reg1 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2021)
reg3 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2021)

# Loaded Normal Version
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + inventory_current_asset_frac1 + capital_ratio1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + inventory_current_asset_frac1 + capital_ratio1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + inventory_current_asset_frac1 + capital_ratio1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + inventory_current_asset_frac1 + capital_ratio1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + inventory_current_asset_frac1 + capital_ratio1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2021)


# Normal Version
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2021)

# Normal Version More Phases
reg1 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2021)
reg1.5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2021)
reg1.51 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension , data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_:SPGlobalESGScore, data = data_2021)
reg3.5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2021)
reg3.6 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_ + industry_:SPGlobalESGScore, data = data_2021)

# Normal Version More Phases weighted
reg1 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2021)
reg1.5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2021)
reg1.51 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw , data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_:SPGlobalESGScore, data = data_2021)
reg3.5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2021)
reg3.6 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_ , data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_ + industry_:SPGlobalESGScore, data = data_2021)

# Normal Version More Phases weighted with calculated ESG Score
reg1 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2021)
reg1.5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPESGScorec, data = data_2021)
reg1.51 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw , data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_:SPESGScorec, data = data_2021)
reg3.5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec  + industry_, data = data_2021)
reg3.6 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_ , data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_ + industry_:SPESGScorec, data = data_2021)


# Final regression
reg1 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2021)
reg1.5 <- lm(yearly_return ~ mktcapm + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2021)
reg1.51 <- lm(yearly_return ~ mktcapm + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2021)
reg3.5 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2021)
reg3 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2021)
reg3.6 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2021)

# Final regression w/ turnover
reg1 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1  + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1, data = data_2021)
reg1.5 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1 + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2021)
reg1.51 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1 + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2021)
reg3.5 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1  + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2021)
reg3 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1  + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2021)
reg3.6 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1  + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1+ shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2021)

# Final regression w/out PE
reg1 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1  + shares_turnover  + yearly_accrual + b_mkt_fama_french_3fac1, data = data_2021)
reg1.5 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1 + shares_turnover  + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2021)
reg1.51 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1 + shares_turnover +  + yearly_accrual + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2021)
reg3.5 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1  + shares_turnover  + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2021)
reg3 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1  + shares_turnover  + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2021)
reg3.6 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1  + shares_turnover  + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2021)
reg4 <- lm(yearly_return_adjusted ~ mktcapm + book_to_market1+ shares_turnover  + yearly_accrual + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2021)

summary(data_2021_renamed$Industry_)
rob_se_imdone<- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                     sqrt(diag(vcovHC(reg1.5, type = "HC1"))),
                     sqrt(diag(vcovHC(reg1.51, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3.5, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3.6, type = "HC1"))),
                     sqrt(diag(vcovHC(reg4, type = "HC1")))
)

stargazer(reg1, reg1.5, reg1.51,  reg3.5,reg3, reg3.6, reg4,
          se = rob_se_imdone,
          title = "ESG Regression Analysis",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2021Weighted.tex")





# Heterosckedasticity robust result 
# gather robust standard errors in a list

rob_se_all <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg1.5, type = "HC1"))),
                   sqrt(diag(vcovHC(reg1.51, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3.5, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3.6, type = "HC1"))),
                   sqrt(diag(vcovHC(reg4, type = "HC1"))),
                   sqrt(diag(vcovHC(reg5, type = "HC1"))))

rob_se_123 <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3, type = "HC1"))))

rob_se_124 <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg4, type = "HC1"))))




# stargazer for all regression results


stargazer(reg1, reg1.5, reg1.51, reg2, reg3, reg3.5, reg3.6, reg4, reg5,
          se = rob_se_all,
          title = "ESG Regression Analysis Complete (2021)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2021Hierarchical.html")

# stargazer without industry, just the industry interaction
stargazer(reg1, reg2, reg3,
          se = rob_se_123,
          title = "ESG Regression Analysis with Industry Interaction (2021)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2021_industry_interaction_only.html")

# stargazer without industry interaction, just the industry 
stargazer(reg1, reg2, reg4,
          se = rob_se_124,
          title = "ESG Regression Analysis with Industry Interaction (2021)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2021_industry_only.html")



####
## For 2020 Analyses
####
# log version
reg1 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1, data = data_2020)
reg2 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2020)
reg3 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2020)
reg4 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data_2020)
reg5 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2020)

# By All Dimensions, including general
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + SPGlobalESGScore, data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + SPGlobalESGScore + industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2020)

# By All Dimensions, excluding general
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension , data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + industry_:SPGlobalESGScore, data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension +  industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension + industry_ + industry_:SPGlobalESGScore, data = data_2020)

# By Environmental Dimension 
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EnvironmentalDimension , data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EnvironmentalDimension + industry_:EnvironmentalDimension, data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	EnvironmentalDimension + industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	EnvironmentalDimension + industry_ + industry_:EnvironmentalDimension, data = data_2020)

# By Social Dimension 
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SocialDimension , data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SocialDimension + industry_:SocialDimension, data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	SocialDimension + industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	SocialDimension + industry_ + industry_:SocialDimension, data = data_2020)

# By Governance Dimension 
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension , data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension + industry_:EconomicGovernanceDimension, data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	EconomicGovernanceDimension + industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 +	EconomicGovernanceDimension + industry_ + industry_:EconomicGovernanceDimension, data = data_2020)

# Normal Version
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2020)

# Normal Version More Phases
reg1 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2020)
reg1.5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2020)
reg1.51 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension , data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_:SPGlobalESGScore, data = data_2020)
reg3.5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2020)
reg3.6 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_ , data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_ + industry_:SPGlobalESGScore, data = data_2020)

# Normal Version More Phases weighted
reg1 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2020)
reg1.5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScorew, data = data_2020)
reg1.51 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScorew + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw , data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScorew + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_:SPGlobalESGScorew, data = data_2020)
reg3.5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScorew  + industry_, data = data_2020)
reg3.6 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_ , data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScorew + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScorew + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_ + industry_:SPGlobalESGScorew, data = data_2020)

# Normal Version More Phases weighted with calculated ESG Score
reg1 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2020)
reg1.5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPESGScorec, data = data_2020)
reg1.51 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw , data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_:SPESGScorec, data = data_2020)
reg3.5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec  + industry_, data = data_2020)
reg3.6 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_ , data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPESGScorec + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_ + industry_:SPESGScorec, data = data_2020)


# Final regression
reg1 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2020)
reg1.5 <- lm(yearly_return ~ mktcapm + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2020)
reg1.51 <- lm(yearly_return ~ mktcapm + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2020)
reg3.5 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2020)
reg3 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2020)
reg3.6 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2020)
reg4 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2020)

# Final regression w/ turnover
reg1 <- lm(yearly_return ~ mktcapm + book_to_market1  + shares_turnover + b_mkt_fama_french_3fac1, data = data_2020)
reg1.5 <- lm(yearly_return ~ mktcapm + book_to_market1 + shares_turnover + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2020)
reg1.51 <- lm(yearly_return ~ mktcapm + book_to_market1 + shares_turnover + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2020)
reg3.5 <- lm(yearly_return ~ mktcapm + book_to_market1  + shares_turnover + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2020)
reg3 <- lm(yearly_return ~ mktcapm + book_to_market1  + shares_turnover + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2020)
reg3.6 <- lm(yearly_return ~ mktcapm + book_to_market1  + shares_turnover + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2020)
reg4 <- lm(yearly_return ~ mktcapm + book_to_market1+ shares_turnover + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2020)



# Final regression w/ turnover
reg1 <- lm(yearly_return ~ mktcapm + book_to_market1  + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1, data = data_2020)
reg1.5 <- lm(yearly_return ~ mktcapm + book_to_market1 + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2020)
reg1.51 <- lm(yearly_return ~ mktcapm + book_to_market1 + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2020)
reg3.5 <- lm(yearly_return ~ mktcapm + book_to_market1  + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2020)
reg3 <- lm(yearly_return ~ mktcapm + book_to_market1  + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2020)
reg3.6 <- lm(yearly_return ~ mktcapm + book_to_market1  + shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2020)
reg4 <- lm(yearly_return ~ mktcapm + book_to_market1+ shares_turnover + yearly_price_to_earnings + yearly_accrual + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2020)


# Heterosckedasticity robust result 
# gather robust standard errors in a list
rob_se_imdone<- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                     sqrt(diag(vcovHC(reg1.5, type = "HC1"))),
                     sqrt(diag(vcovHC(reg1.51, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3.5, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3.6, type = "HC1"))),
                     sqrt(diag(vcovHC(reg4, type = "HC1")))
)

rob_se_all <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg1.5, type = "HC1"))),
                   sqrt(diag(vcovHC(reg1.51, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3.5, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3.6, type = "HC1"))),
                   sqrt(diag(vcovHC(reg4, type = "HC1"))),
                   sqrt(diag(vcovHC(reg5, type = "HC1"))))

rob_se_123 <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3, type = "HC1"))))

rob_se_124 <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg4, type = "HC1"))))

# stargazer for all regression results
stargazer(reg1, reg1.5, reg1.51, reg3.5, reg3,  reg3.6, reg4,
          se = rob_se_imdone,
          title = "ESG Regression Analysis Complete (2020)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2020Weighted.html")



stargazer(reg1, reg1.5, reg1.51, reg2, reg3, reg3.5, reg3.6, reg4, reg5, 
          se = rob_se_all,
          title = "ESG Regression Analysis Complete (2020)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2020Hierarchical.html")

# stargazer without industry, just the industry interaction
stargazer(reg1, reg2, reg3,
          se = rob_se_123,
          title = "ESG Regression Analysis with Industry Interaction (2020)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2020_industry_interaction_only.html")

# stargazer without industry interaction, just the industry 
stargazer(reg1, reg2, reg4,
          se = rob_se_124,
          title = "ESG Regression Analysis with Industry Interaction (2020)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2020_industry_only.html")

####
## For 2019 Analyses
####
# log version
reg1 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1, data = data_2019)
reg2 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2019)
reg3 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2019)
reg4 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data_2019)
reg5 <- lm(log(yearly_return) ~ log(mktcap) + log(book_to_market1) + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2019)

# Normal Version
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2019)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2019)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2019)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data_2019)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2019)


# Normal Version More Phases
reg1 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2019)
reg1.5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2019)
reg1.51 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension, data = data_2019)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension , data = data_2019)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_:SPGlobalESGScore, data = data_2019)
reg3.5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2019)
reg3.6 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_ , data = data_2019)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_, data = data_2019)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EconomicGovernanceDimension +	EnvironmentalDimension + SocialDimension  + industry_ + industry_:SPGlobalESGScore, data = data_2019)


# Final regression
reg1 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1, data = data_2019)
reg1.5 <- lm(yearly_return ~ mktcapm + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore, data = data_2019)
reg1.51 <- lm(yearly_return ~ mktcapm + book_to_market1 + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw, data = data_2019)
reg3.5 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore  + industry_, data = data_2019)
reg3 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data_2019)
reg3.6 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data_2019)
reg4 <- lm(yearly_return ~ mktcapm + book_to_market1  + b_mkt_fama_french_3fac1 + EconomicGovernanceDimensionw +	EnvironmentalDimensionw + SocialDimensionw  + industry_, data = data_2019)


# Heterosckedasticity robust result 
# gather robust standard errors in a list
rob_se_imdone<- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                     sqrt(diag(vcovHC(reg1.5, type = "HC1"))),
                     sqrt(diag(vcovHC(reg1.51, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3.5, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3, type = "HC1"))),
                     sqrt(diag(vcovHC(reg3.6, type = "HC1"))),
                     sqrt(diag(vcovHC(reg4, type = "HC1")))
)

rob_se_all <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg1.5, type = "HC1"))),
                   sqrt(diag(vcovHC(reg1.51, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3.5, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3.6, type = "HC1"))),
                   sqrt(diag(vcovHC(reg4, type = "HC1"))),
                   sqrt(diag(vcovHC(reg5, type = "HC1"))))

rob_se_123 <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg3, type = "HC1"))))

rob_se_124 <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
                   sqrt(diag(vcovHC(reg2, type = "HC1"))),
                   sqrt(diag(vcovHC(reg4, type = "HC1"))))

# stargazer for all regression results
stargazer(reg1, reg1.5, reg1.51, reg3.5, reg3,  reg3.6, reg4,
          se = rob_se_imdone,
          title = "ESG Regression Analysis Complete (2019)",
          type ="text",
          digits = 5,
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2019Weighted.html")


# stargazer without industry, just the industry interaction
stargazer(reg1, reg2, reg3,
          se = rob_se_123,
          title = "ESG Regression Analysis with Industry Interaction (2019)",
          type ="text",
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2019_industry_interaction_only.html")

# stargazer without industry interaction, just the industry 
stargazer(reg1, reg2, reg4,
          se = rob_se_124,
          title = "ESG Regression Analysis with Industry Interaction (2019)",
          type ="text",
          notes=c("Standard Errors are Heteroskedasticity Robust"),
          out = "Regression_Results_html/ESG2019_industry_only.html")



# flip the direction of causality for interesting testing
reg_flip2021 <- lm(SPGlobalESGScore ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ yearly_return + industry, data = data_2019)
summary(reg_flip2021)
reg_flip <- lm(SPGlobalESGScore ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ yearly_return + industry, data = data_2020)
summary(reg_flip2020)
reg_flip <- lm(SPGlobalESGScore ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ yearly_return + industry, data = data_2021)
summary(reg_flip2019)


# Normal Version - All year - All 3 Factors
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data)

# Normal Version - All year - SMB & HML
reg1 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1, data = data)
reg2 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data)
reg3 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_:SPGlobalESGScore, data = data)
reg4 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore + industry_, data = data)
reg5 <- lm(yearly_return ~ b_smb_fama_french_3fac1 + b_hml_fama_french_3fac1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + industry_ + industry_:SPGlobalESGScore, data = data)





