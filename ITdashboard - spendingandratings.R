### ----------------------------------------------------------------------- ###
### Title: Analysis of IT Dashboard Data: Spending and CIO Ratings (Dynamic) 
### Purpose: To analyze investment- and project-level spending data with
### a breakdown of DME, O&M, and investment types; and CIO ratings by  
### government-wide and by agency views.
### Prepared by: Josh Leiling
### Prepared date: August 26, 2017
### Note 1: IT Dashboard - based on currently available data sets
### Note 2: Budget Year = Fiscal Year 2017
### Note 3: Dollars from original data sets are in millions
### ----------------------------------------------------------------------- ###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # or setwd(".")

library(ggplot2)
library(dplyr)

portfolio <- read.csv("itportfolio.csv",as.is = TRUE)
businesscase <- read.csv("businesscase.csv", as.is = TRUE)

#create new data frames for data of interest, check for missing data or errors

## Special Note: When new/updated datasets are imported, you MUST check to   ##
## ensure that the column reference matches the data frame variables below.  ##
## For example, in the first data frame below the agency name should be the  ##
## second column in the dataset, so that portfolio[,2] will call it          ##
## correctly. If for some reason the agency name moves to another column,    ##
## say the third column, then rewrite as agencyname=portfolio[,3] and so on  ##
## for remaining variables. An easy way to identify/verify the column number ##
## is to copy the first row of the dataset, paste special/transpose into an  ##
## Excel sheet, then number the column variables.                            ##  

df_portfolio <- data.frame(agencyname=portfolio[,2], 
                BY_dollar_total=portfolio[,22],
                BY_dollar_dme=portfolio[,27],
                BY_dollar_om=portfolio[,33],
                investmenttype=portfolio[,11],
                stringsAsFactors = FALSE)
rm(portfolio)

df_businesscase <- data.frame(agencyname=businesscase[,4],
                              cioratingcolor=businesscase[,26],
                              cioratingvalue=businesscase[,24],
                              BY_dollar_total=businesscase[,19],
                              stringsAsFactors = FALSE)
rm(businesscase)

agencies <- distinct(df_portfolio, agencyname)
agencies_abbrev <- c("Agriculture","Commerce","Defense","HHS","DOI","DOJ","DOL",
  "State","Treasury","SSA","Education","Energy","EPA","Transportation","GSA",
  "DHS","HUD","NASA","OPM","SBA","VA","USAID","USACE","NARA","NSF","NRC")
df_agencies <- data.frame(agencies, agencies_abbrev, stringsAsFactors = FALSE)

## ---------------------- counts by agency ----------------------------------

df_counts <- data.frame(agencies)

#num_total = counts total number of investments by agency
a1 <- group_by(df_portfolio, agencyname)
a2 <- summarise(a1, count = n())
df_counts$num_total <- a2$count
#num_dme = counts number of investments with DME spend
a1 <- group_by(df_portfolio, agencyname)
a2 <- filter(a1, BY_dollar_dme > 0)
a3 <- summarise(a2, count = n())
df_counts$num_dme <- a3$count
#num_om = counts number of investmetns with O&M spend
a1 <- group_by(df_portfolio, agencyname)
a2 <- filter(a1, BY_dollar_om > 0)
a3 <- summarise(a2, count = n())
df_counts$num_om <- a3$count
#num_major = counts number of major investments
a1 <- group_by(df_portfolio, agencyname)
a2 <- filter(a1, investmenttype == "01 - Major")
a3 <- summarise(a2, count = n())
df_counts$num_major <- a3$count
#num_nonmajor = counts number of nonmajor investments
a1 <- group_by(df_portfolio, agencyname)
a2 <- filter(a1, investmenttype == "02 - Non Major")
a3 <- summarise(a2, count = n())
df_counts$num_nonmajor <- a3$count
#num_green = counts number of investments with a "green" CIO rating
a1 <- group_by(df_businesscase, agencyname)
a2 <- filter(a1, cioratingcolor == "Green")
a3 <- summarise(a2, count = n())
a4 <- merge(df_counts, a3, by="agencyname", all = TRUE)
df_counts$num_green <- a4$count
#num_yellow = counts number of investments with a "yellow" CIO rating
a1 <- group_by(df_businesscase, agencyname)
a2 <- filter(a1, cioratingcolor == "Yellow")
a3 <- summarise(a2, count = n())
a4 <- merge(df_counts, a3, by="agencyname", all = TRUE)
df_counts$num_yellow <- a4$count
#num_red = counts number of investments with a "red" CIO rating
a1 <- group_by(df_businesscase, agencyname)
a2 <- filter(a1, cioratingcolor == "Red")
a3 <- summarise(a2, count = n())
a4 <- merge(df_counts, a3, by="agencyname", all = TRUE)
df_counts$num_red <- a4$count
#convert NA values to 0
df_counts[is.na(df_counts)] <- 0

## ---------------------dollar totals by agency-------------------------------

df_dollars <- data.frame(agencies)

#spending - total of investments
a1 <- group_by(df_portfolio, agencyname)
a2 <- (summarise(a1, dollar_total = round(sum(BY_dollar_total),0)))
df_dollars <- merge(df_dollars, a2, by="agencyname", all = TRUE)
#spending - investments in development
a1 <- group_by(df_portfolio, agencyname)
a2 <- (summarise(a1, dollar_dme = round(sum(BY_dollar_dme),0)))
df_dollars <- merge(df_dollars, a2, by="agencyname", all = TRUE)
#spending - investments in O&M
a1 <- group_by(df_portfolio, agencyname)
a2 <- (summarise(a1, dollar_om = round(sum(BY_dollar_om),0)))
df_dollars <- merge(df_dollars, a2, by="agencyname", all = TRUE)
#spending - major investments
a1 <- group_by(df_businesscase, agencyname) ##discussed with KW, this is the
                                            ##correct dataset for calcuating
                                            ##major $ totals
a2 <- (summarise(a1, dollar_major = round(sum(BY_dollar_total),0)))
df_dollars <- merge(df_dollars, a2, by="agencyname", all = TRUE)
#spending - major investments in development
a1 <- group_by(df_portfolio, agencyname) 
a2 <- filter(a1, investmenttype == "01 - Major")
a3 <- (summarise(a2, dollar_major_dme = round(sum(BY_dollar_dme),0)))
df_dollars <- merge(df_dollars, a3, by="agencyname", all = TRUE)
#spending - major investments in O&M spending
a1 <- group_by(df_portfolio, agencyname) 
a2 <- filter(a1, investmenttype == "01 - Major")
a3 <- (summarise(a2, dollar_major_om = round(sum(BY_dollar_om),0)))
df_dollars <- merge(df_dollars, a3, by="agencyname", all = TRUE)
#spending - nonmajor investments 
a1 <- group_by(df_portfolio, agencyname)
a2 <- filter(a1, investmenttype == "02 - Non Major")
a3 <- (summarise(a2, dollar_nonmajor = round(sum(BY_dollar_total),0)))
df_dollars <- merge(df_dollars, a3, by="agencyname", all = TRUE)
#spending - nonmajor investments in development
a1 <- group_by(df_portfolio, agencyname) 
a2 <- filter(a1, investmenttype == "02 - Non Major")
a3 <- (summarise(a2, dollar_nonmajor_dme = round(sum(BY_dollar_dme),0)))
df_dollars <- merge(df_dollars, a3, by="agencyname", all = TRUE)
#spending - nonmajor investments in O&M
a1 <- group_by(df_portfolio, agencyname) 
a2 <- filter(a1, investmenttype == "02 - Non Major")
a3 <- (summarise(a2, dollar_nonmajor_om = round(sum(BY_dollar_om),0)))
df_dollars <- merge(df_dollars, a3, by="agencyname", all = TRUE)
#spending - investments (with a business case) with green CIO rating
a1 <- group_by(df_businesscase, agencyname)
a2 <- filter(a1, cioratingcolor == "Green")
a3 <- summarise(a2, dollar_green = round(sum(BY_dollar_total),0))
df_dollars <- merge(df_dollars, a3, by="agencyname", all = TRUE)
#spending - investments (with a business case) with yellow CIO rating
a1 <- group_by(df_businesscase, agencyname)
a2 <- filter(a1, cioratingcolor == "Yellow")
a3 <- summarise(a2, dollar_yellow = round(sum(BY_dollar_total),0))
df_dollars <- merge(df_dollars, a3, by="agencyname", all = TRUE)
#spending - investments (with a business case) with red CIO rating
a1 <- group_by(df_businesscase, agencyname)
a2 <- filter(a1, cioratingcolor == "Red")
a3 <- summarise(a2, dollar_red = round(sum(BY_dollar_total),0))
df_dollars <- merge(df_dollars, a3, by="agencyname", all = TRUE)
#convert NA values to 0 
df_dollars[is.na(df_dollars)] <- 0

## -------------------- percentages (of dollars) -------------------------
## this will not always add up to 100% because what's reported in the
## portfolio dataset and business case dataset for majors/non-majors
## and DME vs. O&M does not always add up, for a variety of reasons

df_percentages <- data.frame(agencies)

#percentage of investment dollars in development
a1 <- group_by(df_dollars, agencyname)
a2 <- summarise (a1, pct_dme = round((dollar_dme / dollar_total),2))
df_percentages <- merge(df_percentages, a2, by="agencyname", all = TRUE)
#percentage of investment dollars in operations
a1 <- group_by(df_dollars, agencyname) 
a2 <- summarise (a1, pct_om = round((dollar_om / dollar_total),2))
df_percentages <- merge(df_percentages, a2, by="agencyname", all = TRUE)
#percentage of major investment dollars for majors
a1 <- group_by(df_dollars, agencyname) 
a2 <- summarise (a1, pct_major = round((dollar_major / dollar_total),2))
df_percentages <- merge(df_percentages, a2, by="agencyname", all = TRUE)
#percentage of nonmajor investment dollars
a1 <- group_by(df_dollars, agencyname) 
a2 <- summarise (a1, pct_nonmajor = round((dollar_nonmajor / dollar_total),2))
df_percentages <- merge(df_percentages, a2, by="agencyname", all = TRUE)
#percentage of major investment dollars with a green CIO rating
a1 <- group_by(df_dollars, agencyname) 
a2 <- summarise (a1, pct_green = round((dollar_green / dollar_major),2))
df_percentages <- merge(df_percentages, a2, by="agencyname", all = TRUE)
#percentage of major investment dollars with a yellow CIO rating
a1 <- group_by(df_dollars, agencyname) 
a2 <- summarise (a1, pct_yellow = round((dollar_yellow / dollar_major),2))
df_percentages <- merge(df_percentages, a2, by="agencyname", all = TRUE)
#percentage of major investment dollars with a red CIO rating
a1 <- group_by(df_dollars, agencyname) 
a2 <- summarise (a1, pct_red = round((dollar_red / dollar_major),2))
df_percentages <- merge(df_percentages, a2, by="agencyname", all = TRUE)

## -------------------- average CIO ratings -------------------------------                                        

df_ratings <- data.frame(agencies)
a1 <- group_by(df_businesscase, agencyname)
a2 <- summarise (a1, avgciorating = round(mean(cioratingvalue),1))
df_ratings <- merge(df_ratings, a2, by="agencyname", all = TRUE)

## -------------------- merged data frame ---------------------------------

df_merged <- data.frame(
  Reduce(function(x, y) merge(x, y, all=TRUE),
         list(df_counts, df_dollars, df_percentages, df_ratings))
)

rm(a1,a2,a3,a4)

## ------------ Save data frames as output files ---------------- ##

write.csv(df_counts,file='itdashboard_counts.csv')
write.csv(df_dollars,file='itdashboard_dollars.csv')
write.csv(df_percentages,file='itdashboard_percentages.csv')
write.csv(df_ratings,file='itdashboard_ratings.csv')
write.csv(df_merged,file='itdashboard_merged.csv')

## ------------- Visualize data results ----------------------------------- ##

#scatterplot of CIO ratings, counts, and spend
p1 <- ggplot(data=df_merged,
            aes(x=avgciorating,y=agencyname,colour=num_total,size=dollar_total))
p1 <- p1 + geom_point() +
ggtitle("IT Investments by CIO Rating, Number Total, and Dollar Total - Current Budget Year")
ggsave("scatter_ciorating_count_spend.pdf")
ggsave("scatter_ciorating_count_spend.jpg")
print(p1)

#barplot of total number of investments, by agency
p2 <- data.frame(
  agency = df_merged$agencyname,
  dollar = df_merged$num_total
)
p2 <- ggplot(p2, aes(x = agency, y = dollar))
p2 <- p2 + geom_bar(stat = "identity")
p2 <- p2 + coord_flip()
p2 <- p2 + xlab("Agency") + ylab("Number of Investments") + 
  ggtitle("Number of IT Investments, Current Budget Year")
ggsave("bar_totalinvestmentnumber.pdf")
ggsave("bar_totalinvestmentnumber.jpg")
print(p2)

#barplot of total dollar value of investments, by agency
p3 <- data.frame(
  agency = df_merged$agencyname,
  dollar = df_merged$dollar_total
)
p3 <- ggplot(p3, aes(x = factor(agency), y = dollar))
p3 <- p3 + geom_bar(stat = "identity")
p3 <- p3 + coord_flip()
p3 <- p3 + xlab("Agency") + ylab("Investment Dollars ($M)") + 
  ggtitle("IT Investment Dollars, Current Budget Year")
ggsave("bar_totalinvestmentdollars.pdf")
ggsave("bar_totalinvestmentdollars.jpg")
print(p3)

#stacked bar, $ Major vs. Nonmajor
p4 <- ggplot(df_portfolio, aes(x = agencyname, y = BY_dollar_total, 
                               fill = investmenttype))
p4 <- p4 + stat_summary(fun.y=sum,position="stack",geom="bar")
p4 <- p4 + coord_flip()
p4 <- p4 + xlab("Agency") + ylab("Investment Dollars ($M)") + 
  ggtitle("IT Investment Dollars by Investment Type, Current Budget Year") +
  scale_fill_manual(values=c("#E69F00","#009E73","#0072B2","#000000"),
                    name = "Investment Type")
ggsave("stackedbar_majorvsnonmajordollars.pdf")
ggsave("stackedbar_majorvsnonmajordollars.jpg")
print(p4)

#stacked bar, $ Green, Yellow, Red
p5 <- ggplot(df_businesscase, aes(x = agencyname, y = BY_dollar_total, 
                                     fill = cioratingcolor))
p5 <- p5 + stat_summary(fun.y=sum,position="stack",geom="bar")
p5 <- p5 + coord_flip()
p5 <- p5 + xlab("Agency") + ylab("Investment Dollars ($M)") + 
  ggtitle("IT Investment Dollars by CIO Rating, Current Budget Year") +
  scale_fill_manual(values=c("#009E73","#990000","#F0E442"),
                    name = "CIO Rating",
                    breaks=c("Green","Yellow","Red"))
ggsave("stackedbar_cioratingdollars.pdf")
ggsave("stackedbar_cioratingdollars.jpg")
print(p5)

### ----------------------- END ------------------------------------------ ###