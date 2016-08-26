############################################################################################################################
#### This program is to collect and aggregate data from Compliance files, weekly sales file and Best Buy store profile  ####
#### for building a model.                                                                                              ####
############################################################################################################################

# Remove all variable in the list for space saving
rm(list = ls())

# Load packages
require(lubridate)

# Global variable - count how many weeks from a start date to an end date
start_date = as.POSIXct("2016-01-10")
end_date = as.POSIXct("2016-06-26")
wk_num = ceiling(difftime(strptime(end_date, "%Y-%m-%d"),strptime(start_date, "%Y-%m-%d"), units = "weeks"))
wk_num = as.numeric(wk_num) + 1


# Set file path
getwd()
setwd("C:/Users/zlin/Desktop/Merch Impact/Compliance Data/AW")

# Start the function
DataAggregation_BBY = function(input1, input2, input3) {
  print(wk_num)
  if (!file.exists(input1) | !file.exists(input2) | !file.exists(input3)) {
    stop()
  } else {
    file1 = input1 # Compliance data
    file2 = input2 # Store profile data
    file3 = input3 # Weekly sales data
  }
  

  # file1 = "CB Compliance.csv"
  # file2 = "BBYStoreProfile5_5.csv"
  # file3 = "WeeklyCBSales.csv"
  # Load data file - Chromecast Compliance data
  BBY = read.csv(file1, header = TRUE, stringsAsFactors = FALSE)
  # BBY = read.csv("AW Compliance.csv", header = TRUE, stringsAsFactors = FALSE)
  BBY$Visit_WeekEnding = strptime(BBY$Visit_WeekEnding, "%m/%d/%Y")
  
  # Load data file - Best Buy Store profile and extract the store number from the variable
  BBY_pro = read.csv(file2, header = TRUE, stringsAsFactors = FALSE)
  # BBY_pro = read.csv("BBYStoreProfile5_5.csv", header = TRUE, stringsAsFactors = FALSE)
  BBY_pro$Store.ID = substr(BBY_pro$Store.ID, 13, nchar(BBY_pro$Store.ID))
  #colnames(BBY_pro)[1] = "Location.Number"
  
  # Load data file - Chromecast weekly sales
  BBY_wk_sale = read.csv(file3, header = TRUE, stringsAsFactors = FALSE)
  # BBY_wk_sale = read.csv("AW sales.csv", header = TRUE, stringsAsFactors = FALSE)
  BBY_wk_sale$Week.Ending = strptime(BBY_wk_sale$Week.Ending, "%m/%d/%Y")
  if ("Location" %in% colnames(BBY_wk_sale)) {
    names(BBY_wk_sale)[names(BBY_wk_sale)=="Location"] = "Location.Number"
  }
  # Extract data from Chromecast Compliance data we need use - All Best Buy stores in US
  # BBY_CC_US = BBY_CC[BBY_CC$CountryCode == "US" & BBY_CC$ChainName == "Best Buy" & BBY_CC$Visit_WeekEnding > 2016-01-01
  #                    & BBY_CC$Visit_WeekEnding < 2016-06-30,]
  
  BBY_US = BBY[BBY$CountryCode == "US" & BBY$ChainName == "Best Buy" & BBY$Visit_WeekEnding >= start_date & 
                 BBY$Visit_WeekEnding <= end_date,]
  
  # Extract data from weekly sales file by matching store numbers
  BBY_wk_sale_mat = BBY_wk_sale[which(BBY_wk_sale$Location.Number %in% BBY_US$StoreNumber),]
  if (file1 == "CC Compliance.csv") {
    BBY_wk_sale_mat = BBY_wk_sale_mat[BBY_wk_sale_mat$Week.Ending >= start_date & BBY_wk_sale_mat$Week.Ending <= end_date &
                                          BBY_wk_sale_mat$SKU.Description != "Google Chromecast Audio",]
  } else if (file1 == "CCA Compliance.csv") {
    BBY_wk_sale_mat = BBY_wk_sale_mat[BBY_wk_sale_mat$Week.Ending >= start_date & BBY_wk_sale_mat$Week.Ending <= end_date &
                                        BBY_wk_sale_mat$SKU.Description == "Google Chromecast Audio",]
  } else {
    BBY_wk_sale_mat = BBY_wk_sale_mat[BBY_wk_sale_mat$Week.Ending >= start_date & BBY_wk_sale_mat$Week.Ending <= end_date,]
  } 
  
  # test = aggregate(BBY_wk_CCsale_mat$SumOfUnits, by = list(month = substr(BBY_wk_CCsale_mat$Week.Ending,1,7)), FUN = sum)
  # plot(test$x, ylim = c(70000,200000))
  
  
  # Extract data from BBY profile by matching store numbers and reorder by store number
  BBY_pro_mat = BBY_pro[which(BBY_pro$Store.ID %in% BBY_US$StoreNumber),]
  BBY_pro_mat = BBY_pro_mat[order(as.numeric(BBY_pro_mat$Store.ID)),]
  
  # Aggregate data by store number
  BBY_US_split = split(BBY_US, BBY_US$StoreNumber)
  
  BBY_US_var = c()
  BBY_US_stN = c()
  for (i in 1:length(BBY_US_split)) {
    #x = sapply(BBY_CC_US_split[[i]][,13:34], mean)
    BBY_US_var = rbind(BBY_US_var, sapply(BBY_US_split[[i]][,13:34], mean))
    BBY_US_stN = c(BBY_US_stN, BBY_US_split[[i]][1,7]) # Collect corresponding store numbers
  }
  
  # Calculate average sales data
  # Function to calculate mean
  # ave = function(x) {
  #   aver = sum(x)/26
  #   return(aver)
  # }
  
  # Start to aggregate
  BBY_wk_sale_mat_split = split(BBY_wk_sale_mat, BBY_wk_sale_mat$Location.Number)
  
  Ave_sale = c()
  for (i in 1:length(BBY_wk_sale_mat_split)) {
    y = sum(BBY_wk_sale_mat_split[[i]][,"SumOfUnits"]) / wk_num
    Ave_sale = c(Ave_sale, y)
  }
  
  
  # Combine all data into one data frame for future use
  BBY_US_all = cbind(BBY_US_stN, Ave_sale, BBY_US_var, BBY_pro_mat$Fixture)
  BBY_US_all = BBY_US_all[order(as.numeric(BBY_US_all[,1])),]
  colnames(BBY_US_all)[ncol(BBY_US_all)] = "Fixture Type"
  BBY_US_all = data.frame(BBY_US_all)
  # return(class(BBY_US_all))
  # Delete records which Fixture Type is N/A
  BBY_US_all = BBY_US_all[BBY_US_all$Fixture.Type != "N/A",]
  
  if (file1 == "CC Compliance.csv") {
    write.csv(BBY_US_all, "Merch Impact for Chromecast.csv", row.names = FALSE)
  } else if (file1 == "CB Compliance.csv") {
    write.csv(BBY_US_all, "Merch Impact for Chromebook.csv", row.names = FALSE)
  } else if(file1 == "CCA Compliance.csv") {
    write.csv(BBY_US_all, "Merch Impact for Chromecast Audio.csv", row.names = FALSE)
  } else {
    write.csv(BBY_US_all, "Merch Impact for AW.csv", row.names = FALSE)
  }
}
# write.csv(BBY_CC_US_all, "Merch Impact.csv", row.names = FALSE)

# View(BBY_CC_US_all)
DataAggregation_BBY("AW Compliance.csv","BBYStoreProfile5_5.csv","AW sales.csv")
