library("readxl")
# read in excel Final_data_yearly.xlsx

data <- read_excel("Final_data_yearly.xlsx")

# With mktcap, book_to_market1, b_mkt_fama_french_3fac1, SPGlobalESGScore, and interaction between icb_industry_code and SPGlobalESGScore
# dependent variable is yearly_return

# only select year from 2021
data_2021 <- data[data$year == 2021, ]
data_2020 <- data[data$year == 2020, ]
data_2020 <- data[data$year == 2019, ]

reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + icb_industry_code:SPGlobalESGScore, data = data_2021)

# 2021 with EnvironmentalDimension, SocialDimension, EconomicGovernanceDimension
reg11 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg22 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2021)
reg23 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore + EnvironmentalDimension + SocialDimension + EconomicGovernanceDimension, data = data_2021)
reg34 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + EnvironmentalDimension + SocialDimension + EconomicGovernanceDimension + icb_industry_code:SPGlobalESGScore, data = data_2021)


# stargazer for reg1, reg2,  reg3, outpit to latex
stargazer(reg1, reg2, reg3, type = "latex", title = "ESG Regression Analysis (2021)")

# now for 2020

reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2020)
reg6 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + icb_industry_code:SPGlobalESGScore, data = data_2020)

stargazer(reg4, reg5, reg6, type = "latex", title = "ESG Regression Analysis (2020)")

# now for 2019

reg7 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2019)
reg8 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ SPGlobalESGScore, data = data_2019)
reg9 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + SPGlobalESGScore + icb_industry_code:SPGlobalESGScore, data = data_2019)

stargazer(reg7, reg8, reg9, type = "latex", title = "ESG Regression Analysis (2019)")

# drop NAs in data_2021
data_2021 <- data_2021[complete.cases(data_2021), ]
# produce correlation matrix plot for 2021
proj_cor = data_2021 %>% select(mktcap, book_to_market1, b_mkt_fama_french_3fac1, SPGlobalESGScore, icb_industry_code, yearly_return)
proj_cor = cor(proj_cor)
corrplot(m, method = "color", type = "upper", order = "hclust", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45, number.cex=0.75, diag = FALSE)

