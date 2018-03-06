library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Cost-burdened Households by Town
# Created by Jenna Daly
# On 03/05/2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

options(scipen=999)
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2016,
    table = "DP04"
)

cost_burdened <- data.table()
for (data in acsdata) {
    year <- data@endyear
   if (year %in% c(2015, 2016)) {
      cost_home_total_num <- acsSum(data, 219, "Homeowner Total Num") #VC159
      cost_owner_num_30 <- acsSum(data, 227, "Homeowner 30 Num") #VC163
      cost_owner_pct_30 <- acsSum(data, 228, "Homeowner 30 Pct") #VC163
      cost_owner_num_35 <- acsSum(data, 229, "Homeowner 35 Num") #VC164
      cost_owner_pct_35 <- acsSum(data, 230, "Homeowner 35 Pct") #VC164
      cost_rent_total_num <- acsSum(data, 271, "Renter Total Num") #VC198
      cost_renter_num_30 <- acsSum(data, 281, "Renter 30 Num") #VC203     
      cost_renter_pct_30 <- acsSum(data, 282, "Renter 30 Pct") #VC203 
      cost_renter_num_35 <- acsSum(data, 283, "Renter 35 Num") #VC204      
      cost_renter_pct_35 <- acsSum(data, 284, "Renter 35 Pct") #VC204     
    } else if (year %in% c(2010, 2011, 2012)) {
      cost_home_total_num <- acsSum(data, 215, "Homeowner Total Num") #VC155
      cost_owner_num_30 <- acsSum(data, 223, "Homeowner 30 Num") #VC159
      cost_owner_pct_30 <- acsSum(data, 224, "Homeowner 30 Pct") #VC159
      cost_owner_num_35 <- acsSum(data, 225, "Homeowner 35 Num") #VC160
      cost_owner_pct_35 <- acsSum(data, 226, "Homeowner 35 Pct") #VC160
      cost_rent_total_num <- acsSum(data, 267, "Renter Total Num") #VC191 
      cost_renter_num_30 <- acsSum(data, 277, "Renter 30 Num") #VC196
      cost_renter_pct_30 <- acsSum(data, 278, "Renter 30 Pct") #VC196
      cost_renter_num_35 <- acsSum(data, 279, "Renter 35 Num") #VC197
      cost_renter_pct_35 <- acsSum(data, 280, "Renter 35 Pct") #VC197
    } else { #2013, 2014
      cost_home_total_num <- acsSum(data, 215, "Homeowner Total Num") #VC157
      cost_owner_num_30 <- acsSum(data, 223, "Homeowner 30 Num") #VC161
      cost_owner_pct_30 <- acsSum(data, 224, "Homeowner 30 Pct") #VC161
      cost_owner_num_35 <- acsSum(data, 225, "Homeowner 35 Num") #VC162
      cost_owner_pct_35 <- acsSum(data, 226, "Homeowner 35 Pct") #VC162
      cost_rent_total_num <- acsSum(data, 267, "Renter Total Num") #VC196 
      cost_renter_num_30 <- acsSum(data, 277, "Renter 30 Num") #VC196
      cost_renter_pct_30 <- acsSum(data, 278, "Renter 30 Pct") #VC196
      cost_renter_num_35 <- acsSum(data, 279, "Renter 35 Num") #VC197
      cost_renter_pct_35 <- acsSum(data, 280, "Renter 35 Pct") #VC197
    }

    datafips <- data.table(fips = getACSFips(data))
    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(cost_home_total_num),
        estimate(cost_owner_num_30),
        estimate(cost_owner_pct_30),
        estimate(cost_owner_num_35),
        estimate(cost_owner_pct_35),
        estimate(cost_rent_total_num), 
        estimate(cost_renter_num_30),
        estimate(cost_renter_pct_30),
        estimate(cost_renter_num_35),
        estimate(cost_renter_pct_35)
    )
    
    names(estimates)[names(estimates) == "HC01_VC159.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)"] <- "Total Homeowners Num"
    names(estimates)[names(estimates) == "HC01_VC163.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Num"
    names(estimates)[names(estimates) == "HC03_VC163.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Pct"
    names(estimates)[names(estimates) == "HC01_VC164.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 35.0 percent or more"] <- "Total Homeowners 35 Num"
    names(estimates)[names(estimates) == "HC03_VC164.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 35.0 percent or more" ] <- "Total Homeowners 35 Pct"
    names(estimates)[names(estimates) == "HC01_VC203.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Renters 30 Num"                         
    names(estimates)[names(estimates) == "HC03_VC203.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 30.0 to 34.9 percent" ] <- "Total Renters 30 Pct"                         
    names(estimates)[names(estimates) == "HC01_VC204.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 35.0 percent or more" ] <- "Total Renters 35 Num"                        
    names(estimates)[names(estimates) == "HC03_VC204.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 35.0 percent or more"] <- "Total Renters 35 Pct"
    names(estimates)[names(estimates) == "HC01_VC198.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed)"] <- "Total Renters Num"                                                
    names(estimates)[names(estimates) == "HC01_VC155.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)"] <- "Total Homeowners Num"
    names(estimates)[names(estimates) == "HC01_VC159.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Num"
    names(estimates)[names(estimates) == "HC03_VC159.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Pct"
    names(estimates)[names(estimates) == "HC01_VC160.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 35.0 percent or more"] <- "Total Homeowners 35 Num"
    names(estimates)[names(estimates) == "HC03_VC160.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 35.0 percent or more "] <- "Total Homeowners 35 Pct"
    names(estimates)[names(estimates) == "HC03_VC196.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - 30.0 to 34.9 percent"] <- "Total Renters 30 Pct"
    names(estimates)[names(estimates) == "HC03_VC197.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - 35.0 percent or more"] <- "Total Renters 35 Pct"
    names(estimates)[names(estimates) == "HC01_VC196.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - 30.0 to 34.9 percent"] <- "Total Renters 30 Num"
    names(estimates)[names(estimates) == "HC01_VC197.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - 35.0 percent or more"] <- "Total Renters 35 Num"
    names(estimates)[names(estimates) == "HC01_VC191.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed)"] <- "Total Renters Num" 
    names(estimates)[names(estimates) == "HC03_VC160.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 35.0 percent or more"] <- "Total Homeowners 35 Pct"
    names(estimates)[names(estimates) == "HC01_VC157.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)"] <- "Total Homeowners Num"
    names(estimates)[names(estimates) == "HC01_VC161.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Num"
    names(estimates)[names(estimates) == "HC01_VC162.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 35.0 percent or more"] <- "Total Homeowners 35 Num"
    names(estimates)[names(estimates) == "HC01_VC196.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed)"] <- "Total Renters Num"
    names(estimates)[names(estimates) == "HC01_VC201.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Renters 30 Num"
    names(estimates)[names(estimates) == "HC01_VC202.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 35.0 percent or more"] <- "Total Renters 35 Num"
    names(estimates)[names(estimates) == "HC03_VC161.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Pct"
    names(estimates)[names(estimates) == "HC03_VC162.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 35.0 percent or more"] <- "Total Homeowners 35 Pct"
    names(estimates)[names(estimates) == "HC03_VC201.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Renters 30 Pct"
    names(estimates)[names(estimates) == "HC03_VC202.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 35.0 percent or more"] <- "Total Renters 35 Pct"

    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Variable",
        variable.factor = F,
        value.name = "Number",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(cost_home_total_num) * 1.645,
        standard.error(cost_owner_num_30) * 1.645,
        standard.error(cost_owner_pct_30) * 1.645,
        standard.error(cost_owner_num_35) * 1.645,
        standard.error(cost_owner_pct_35) * 1.645,
        standard.error(cost_rent_total_num) * 1.645, 
        standard.error(cost_renter_num_30) * 1.645,
        standard.error(cost_renter_pct_30) * 1.645,
        standard.error(cost_renter_num_35) * 1.645,
        standard.error(cost_renter_pct_35) * 1.645
    )
    
    names(moes)[names(moes) == "HC01_VC159.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)"] <- "Total Homeowners Num"
    names(moes)[names(moes) == "HC01_VC163.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Num"
    names(moes)[names(moes) == "HC03_VC163.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Pct"
    names(moes)[names(moes) == "HC01_VC164.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 35.0 percent or more"] <- "Total Homeowners 35 Num"
    names(moes)[names(moes) == "HC03_VC164.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 35.0 percent or more" ] <- "Total Homeowners 35 Pct"
    names(moes)[names(moes) == "HC01_VC203.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Renters 30 Num"                         
    names(moes)[names(moes) == "HC03_VC203.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 30.0 to 34.9 percent" ] <- "Total Renters 30 Pct"                         
    names(moes)[names(moes) == "HC01_VC204.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 35.0 percent or more" ] <- "Total Renters 35 Num"                        
    names(moes)[names(moes) == "HC03_VC204.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 35.0 percent or more"] <- "Total Renters 35 Pct"
    names(moes)[names(moes) == "HC01_VC198.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed)"] <- "Total Renters Num"                                                
    names(moes)[names(moes) == "HC01_VC155.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)"] <- "Total Homeowners Num"
    names(moes)[names(moes) == "HC01_VC159.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Num"
    names(moes)[names(moes) == "HC03_VC159.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Pct"
    names(moes)[names(moes) == "HC01_VC160.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 35.0 percent or more"] <- "Total Homeowners 35 Num"
    names(moes)[names(moes) == "HC03_VC160.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 35.0 percent or more "] <- "Total Homeowners 35 Pct"
    names(moes)[names(moes) == "HC03_VC196.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - 30.0 to 34.9 percent"] <- "Total Renters 30 Pct"
    names(moes)[names(moes) == "HC03_VC197.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - 35.0 percent or more"] <- "Total Renters 35 Pct"
    names(moes)[names(moes) == "HC01_VC196.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - 30.0 to 34.9 percent"] <- "Total Renters 30 Num"
    names(moes)[names(moes) == "HC01_VC197.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - 35.0 percent or more"] <- "Total Renters 35 Num"
    names(moes)[names(moes) == "HC01_VC191.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed)"] <- "Total Renters Num" 
    names(moes)[names(moes) == "HC03_VC160.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 35.0 percent or more"] <- "Total Homeowners 35 Pct"
    names(moes)[names(moes) == "HC03_VC160.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - 35.0 percent or more"] <- "Total Homeowners 35 Pct"
    names(moes)[names(moes) == "HC01_VC157.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed)"] <- "Total Homeowners Num"
    names(moes)[names(moes) == "HC01_VC161.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Num"
    names(moes)[names(moes) == "HC01_VC162.Estimate; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 35.0 percent or more"] <- "Total Homeowners 35 Num"
    names(moes)[names(moes) == "HC01_VC196.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed)"] <- "Total Renters Num"
    names(moes)[names(moes) == "HC01_VC201.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Renters 30 Num"
    names(moes)[names(moes) == "HC01_VC202.Estimate; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 35.0 percent or more"] <- "Total Renters 35 Num"
    names(moes)[names(moes) == "HC03_VC161.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Homeowners 30 Pct"
    names(moes)[names(moes) == "HC03_VC162.Percent; SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME (SMOCAPI) - Housing units with a mortgage (excluding units where SMOCAPI cannot be computed) - 35.0 percent or more"] <- "Total Homeowners 35 Pct"
    names(moes)[names(moes) == "HC03_VC201.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 30.0 to 34.9 percent"] <- "Total Renters 30 Pct"
    names(moes)[names(moes) == "HC03_VC202.Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI) - Occupied units paying rent (excluding units where GRAPI cannot be computed) - 35.0 percent or more"] <- "Total Renters 35 Pct"

    
    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Variable",
        variable.factor = F,
        value.name = "Margins of Error",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Variable`)
    setkey(moes, FIPS, Year, `Variable`)

    cost_burdened <- rbind(cost_burdened, estimates[moes])
}
 
cost_burdened <- cost_burdened[cost_burdened$FIPS != "0900100000",]

#Calculate total below burdened (30% and 35%)
cost_burdened_long <- gather(cost_burdened, `Meaure Type`, Value, 4:5, factor_key = FALSE)
cost_burdened_est <- cost_burdened_long[cost_burdened_long$`Meaure Type` == "Number",]
cost_burdened_est_wide <- spread(cost_burdened_est, Variable, Value)
cost_burdened_moe <- cost_burdened_long[cost_burdened_long$`Meaure Type` == "Margins of Error",]
cost_burdened_moe_wide <- spread(cost_burdened_moe, Variable, Value)
cost_burdened_wide <- merge(cost_burdened_est_wide, cost_burdened_moe_wide, by = c("FIPS", "Year"))

#############################################################################################
# Helper function for MOE
calcMOE <- function(x, y, moex, moey) {
  moex2 <- moex^2
  moey2 <- moey^2
  d <- x/y
  d2 <- d^2
  
  radicand <- ifelse(
    moex2 < (d2 * moey2),
    moex2 + (d2 * moey2),
    moex2 - (d2 * moey2)
  )
  
  return(sqrt(radicand)/y)
}
#############################################################################################

cost_burdened_wide_calc <- cost_burdened_wide %>% 
  mutate(`Total Cost-burdened Homeowners Num` = (`Total Homeowners 30 Num.x` + `Total Homeowners 35 Num.x`),
         `Total Cost-burdened Homeowners Num MOE` =sqrt((`Total Homeowners 30 Num.y`^2) + (`Total Homeowners 35 Num.y`^2)),
         `Total Cost-burdened Homeowners Pct` = (`Total Homeowners 30 Pct.x` + `Total Homeowners 35 Pct.x`),
         `Total Cost-burdened Homeowners Pct MOE` = sqrt((`Total Homeowners 30 Pct.y`^2) + (`Total Homeowners 35 Pct.y`^2)),
         
         `Total Cost-burdened Renters Num` = (`Total Renters 30 Num.x` + `Total Renters 35 Num.x`),
         `Total Cost-burdened Renters Num MOE` =sqrt((`Total Renters 30 Num.y`^2) + (`Total Renters 35 Num.y`^2)),
         `Total Cost-burdened Renters Pct` = (`Total Renters 30 Pct.x` + `Total Renters 35 Pct.x`),
         `Total Cost-burdened Renters Pct MOE` = sqrt((`Total Renters 30 Pct.y`^2) + (`Total Renters 35 Pct.y`^2)),
         
         `Total Cost-burdened All Num` = (`Total Homeowners 30 Num.x` + `Total Homeowners 35 Num.x` + `Total Renters 30 Num.x` + `Total Renters 35 Num.x`),
         `Total Cost-burdened All Num MOE` = sqrt((`Total Renters 30 Num.y`^2) + (`Total Renters 35 Num.y`^2) + (`Total Homeowners 30 Num.y`^2) + (`Total Homeowners 35 Num.y`^2)),
         `Total Cost-burdened All Pct` = ((`Total Homeowners 30 Num.x` + `Total Homeowners 35 Num.x` + `Total Renters 30 Num.x` + `Total Renters 35 Num.x`) / (`Total Homeowners Num.x` + `Total Renters Num.x`))*100,
         `Total Cost-burdened All Pct MOE` = 
           calcMOE((`Total Homeowners 30 Num.x` + `Total Homeowners 35 Num.x` + `Total Renters 30 Num.x` + `Total Renters 35 Num.x`), 
                   (`Total Homeowners Num.x` + `Total Renters Num.x`), 
                   (sqrt((`Total Renters 30 Num.y`^2) + (`Total Renters 35 Num.y`^2) + (`Total Homeowners 30 Num.y`^2) + (`Total Homeowners 35 Num.y`^2))), 
                   (sqrt((`Total Homeowners Num.y`)^2 + (`Total Renters Num.y`)^2)))*100
           # x1 = (`Total Homeowners 30 Num.x` + `Total Homeowners 35 Num.x` + `Total Renters 30 Num.x` + `Total Renters 35 Num.x`)
           # x2 = (`Total Homeowners Num.x` + `Total Renters Num.x`)
           # M1 = sqrt((`Total Renters 30 Num.y`^2) + (`Total Renters 35 Num.y`^2) + (`Total Homeowners 30 Num.y`^2) + (`Total Homeowners 35 Num.y`^2))
           # M2 = sqrt((`Total Homeowners Num.x`)^2 + (`Total Renters Num.x`)^2)
         
         ) %>% 
  select(FIPS, Year, `Total Cost-burdened Renters Num`, `Total Cost-burdened Renters Num MOE`, `Total Cost-burdened Renters Pct`, `Total Cost-burdened Renters Pct MOE`, 
         `Total Cost-burdened Homeowners Num`, `Total Cost-burdened Homeowners Num MOE`, `Total Cost-burdened Homeowners Pct`, `Total Cost-burdened Homeowners Pct MOE`,
         `Total Cost-burdened All Num`, `Total Cost-burdened All Num MOE`, `Total Cost-burdened All Pct`, `Total Cost-burdened All Pct MOE`)


cost_burdened_final <- gather(cost_burdened_wide_calc, Variable, Value, 3:14, factor_key = FALSE)

#Clean up columns
cost_burdened_final$`Householder Status` <- "All"
cost_burdened_final$`Householder Status`[grepl("Homeowner", cost_burdened_final$Variable)] <- "Homeowner"
cost_burdened_final$`Householder Status`[grepl("Renter", cost_burdened_final$Variable)] <- "Renter"
cost_burdened_final$`Measure Type`[grepl("Num", cost_burdened_final$Variable)] <- "Number"
cost_burdened_final$`Measure Type`[grepl("Pct", cost_burdened_final$Variable)] <- "Percent"
cost_burdened_final$Variable[grepl("MOE", cost_burdened_final$Variable)] <- "Margins of Error"
cost_burdened_final$Variable[grepl("Total", cost_burdened_final$Variable)] <- "Cost-burdened Households"


#Merge in Towns by FIPS (filter out county data)
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

cost_burdened_final_fips <- merge(cost_burdened_final, towns, by = "FIPS")

cost_burdened_final_fips$Year <- paste(cost_burdened_final_fips$Year-4, cost_burdened_final_fips$Year, sep="-")

cost_burdened_complete <- cost_burdened_final_fips %>% 
  select(Town, FIPS, Year, `Householder Status`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Householder Status`, `Measure Type`, Variable)

cost_burdened_complete$Value <-  round(cost_burdened_complete$Value, 2)

write.table(
    cost_burdened_complete,
    file.path("data", "cost-burdened-households-town-2016.csv"),
    sep = ",",
    row.names = F,
    col.names = T,
    na = "-9999" 
)

