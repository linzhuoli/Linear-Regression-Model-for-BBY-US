# library(stats4)
require(dplyr)
rm(list = ls())

getwd()
setwd("C:/Users/zlin/Desktop/Merch Impact/Compliance Data/Best Buy")

# Read data
dat_raw = read.csv("Merch Impact for Chromebook.csv", header = TRUE, stringsAsFactors = FALSE)
dat_raw$Fixt_delta = as.numeric(dat_raw$Fixt_delta)

dat_pro = read.csv("BBYStoreProfile5_5.csv", header = TRUE, stringsAsFactors = FALSE)
dat_pro$Store.ID = substr(dat_pro$Store.ID, 13, nchar(dat_pro$Store.ID))
dat_pro = dat_pro[dat_pro$Store.ID %in% dat_raw$BBY_US_stN,]
dat_pro = dat_pro[order(as.numeric(dat_pro$Store.ID)),]

# Add one more column for Fixture type index (Extended Endcap = 1, Legacy Endcap = 0)
dat = mutate(dat_raw, Fixture_ind = ifelse(dat_raw$Fixture.Type == "Legacy Endcap", 0, 1))
dat$Fixture_ind = as.factor(dat$Fixture_ind)
dat = data.frame(dat,dat_pro$PRS.Mobile.Computing.Unit.Sales.Index)

# Convert compliance variable into categorical variable for Chi-square test.
col_num = c(match("Loc_delta", names(dat)),match("Demo_delta", names(dat)),match("Msg_delta", names(dat)),
            match("OSA_delta", names(dat)))

for (i in col_num) {
  dat[,i] = ifelse(dat[,i] <= 0, "no", "yes")
}
dat[,"score_SLA"] = ifelse(dat[,"score_SLA"] <= 0.8, "low", ifelse(dat[,"score_SLA"] > 0.8 & dat[,"score_SLA"] <= 0.9, "med", "high"))

# temp = dat[dat$Ave_sale < 100,]
# dat = mutate(dat, salelvl = ifelse(dat$Ave_sale <= quantile(temp$Ave_sale,1/3), "Low",
#                                                      ifelse(dat$Ave_sale > quantile(temp$Ave_sale,1/3) &
#                                                               dat$Ave_sale <= quantile(temp$Ave_sale,2/3), "Med",
#                                                             ifelse(dat$Ave_sale > quantile(temp$Ave_sale,2/3) &
#                                                                      dat$Ave_sale <= quantile(temp$Ave_sale,1), "High", "Outlier"))))

# dat = mutate(dat, salelvl = ifelse(dat$Ave_sale <= 1, "Low",
#                                                      ifelse(dat$Ave_sale > 1 &
#                                                               dat$Ave_sale <= 2,"Med", "High")))


colnames(dat)[27] = "sale_index"

# cutoff_point = mean(dat$Ave_sale)+3*sd(dat$Ave_sale)
# dat = dat[dat$Ave_sale < 100,] # Remove the outliers for Chromecast
# dat = dat[dat$Ave_sale < 7,] # Remove the outliers for Chromecast Audio
dat = dat[dat$Ave_sale < 12.5,] # Remove the outliers for Chromebook
# dat = dat[dat$Ave_sale < 4,] # Remove the outliers for AW

# sale_freq = as.data.frame(table(dat$Ave_sale))
# colnames(sale_freq)[1] = "sale"


# dat_high = dat[dat$salelvl == "High",]
# dat_med = dat[dat$salelvl == "Med",]
# dat_low = dat[dat$salelvl == "Low",]
# # 
# fit_high = lm(Ave_sale ~ OSA_delta + Msg_delta + Demo_delta + Loc_delta + sale_index, data = dat_high)
# summary(fit_high)
# 
# fit_med = lm(Ave_sale ~ OSA_delta + Msg_delta + Demo_delta + Loc_delta + sale_index, data = dat_med)
# summary(fit_med)
# 
# fit_low = lm(Ave_sale ~ OSA_delta + Msg_delta + Demo_delta + Loc_delta + sale_index, data = dat_low)
# summary(fit_low)

fit_CC = lm(Ave_sale ~ OSA_delta + Msg_delta + Demo_delta + Loc_delta + sale_index, data = dat)
summary(fit_CC)

fit_CC_new = lm(Ave_sale ~ OSA_delta + Msg_delta + Loc_delta + sale_index, data = dat)
summary(fit_CC_new)

fit_CCA = lm(Ave_sale ~ OSA_delta + Msg_delta + Loc_delta + sale_index, data = dat)
summary(fit_CCA)

fit_CCA_new = lm(Ave_sale ~ OSA_delta + Msg_delta + sale_index, data = dat)
summary(fit_CCA_new)

fit_CB = lm(Ave_sale ~ Demo_delta + Loc_delta + score_SLA + sale_index, data = dat)
summary(fit_CB)

fit_CB_new = lm(Ave_sale ~ Demo_delta + score_SLA + sale_index, data = dat)
summary(fit_CB_new)

fit_AW = lm(Ave_sale ~ OSA_delta + Msg_delta + Demo_delta + Loc_delta + sale_index, data = dat)
summary(fit_AW)

fit_AW_new = lm(Ave_sale ~ Demo_delta + sale_index, data = dat)
summary(fit_AW_new)

fit = lm(Ave_sale ~ OSA_delta, data = dat)
summary(fit)
plot(dat$Ave_sale ~ factor(dat$OSA_delta))
