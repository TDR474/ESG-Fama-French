library("corrplot")
library("lmtest")
library("sandwich")
library("readxl")
library("stargazer")

setwd(dir = "/Users/frankzhu/Desktop/Metrics_GP/project-draft/")

# read in excel Final_data_yearly.xlsx
data <- read_excel("Final_data_yearly.xls")

# Possible transformation
data$ESGRATIO = data$SPGlobalESGScore / (mean(data$SPGlobalESGScore))
data
# With mktcap, book_to_market1, b_mkt_fama_french_3fac1, SPGlobalESGScore, and interaction between industry and SPGlobalESGScore
# dependent variable is yearly_return

# partition the data
data_2021 <- data[data$year == 2021, ]
data_2020 <- data[data$year == 2020, ]
data_2020 <- data[data$year == 2019, ]

data_2021 <- data[data$year == 2021, ]
data_2020 <- data[data$year == 2020, ]
data_2019 <- data[data$year == 2019, ]

####
## For 2021 Analyses
####
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2021)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ ESGRATIO, data = data_2021)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + ESGRATIO + industry_:ESGRATIO, data = data_2021)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ ESGRATIO + industry_, data = data_2021)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + ESGRATIO + industry_ + industry_:ESGRATIO, data = data_2021)

# Heterosckedasticity robust result 
# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
               sqrt(diag(vcovHC(reg2, type = "HC1"))),
               sqrt(diag(vcovHC(reg3, type = "HC1"))),
               sqrt(diag(vcovHC(reg4, type = "HC1"))),
               sqrt(diag(vcovHC(reg5, type = "HC1"))))

# stargazer for reg1, reg2,  reg3, outpit to latex
stargazer(reg1, reg2, reg3, reg4, reg5,
          se = rob_se,
          title = "ESG Regression Analysis (2021)",
          type ="text")


####
## For 2020 Analyses
####
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2020)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ ESGRATIO, data = data_2020)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ ESGRATIO + industry_, data = data_2020)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + ESGRATIO + industry_:ESGRATIO, data = data_2020)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + ESGRATIO + industry_ + industry_:ESGRATIO, data = data_2020)

# Heterosckedasticity robust result 
# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
               sqrt(diag(vcovHC(reg2, type = "HC1"))),
               sqrt(diag(vcovHC(reg3, type = "HC1"))),
               sqrt(diag(vcovHC(reg4, type = "HC1"))),
               sqrt(diag(vcovHC(reg5, type = "HC1"))))

# stargazer for reg1, reg2,  reg3, outpit to latex
stargazer(reg1, reg2, reg3, reg4, reg5,
          se = rob_se,
          title = "ESG Regression Analysis (2020)",
          type ="text")

####
## For 2019 Analyses
####
reg1 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1, data = data_2019)
reg2 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ ESGRATIO, data = data_2019)
reg3 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ ESGRATIO + industry_, data = data_2019)
reg4 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + ESGRATIO + industry_:ESGRATIO, data = data_2019)
reg5 <- lm(yearly_return ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1 + ESGRATIO + industry_ + industry_:ESGRATIO, data = data_2019)

# Heterosckedasticity robust result 
# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(reg1, type = "HC1"))),
               sqrt(diag(vcovHC(reg2, type = "HC1"))),
               sqrt(diag(vcovHC(reg3, type = "HC1"))),
               sqrt(diag(vcovHC(reg4, type = "HC1"))),
               sqrt(diag(vcovHC(reg5, type = "HC1"))))

# stargazer for reg1, reg2,  reg3, outpit to latex
stargazer(reg1, reg2, reg3, reg4, reg5,
          se = rob_se,
          title = "ESG Regression Analysis (2019)",
          type ="text")




# flip the direction of causality for interesting testing
reg_flip2021 <- lm(SPGlobalESGScore ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ yearly_return + industry, data = data_2019)
summary(reg_flip2021)
reg_flip <- lm(SPGlobalESGScore ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ yearly_return + industry, data = data_2020)
summary(reg_flip2020)
reg_flip <- lm(SPGlobalESGScore ~ mktcap + book_to_market1 + b_mkt_fama_french_3fac1+ yearly_return + industry, data = data_2021)
summary(reg_flip2019)

