ufo_data=read.csv("ufo.csv")
View(ufo_data)
ufo_data<- subset(ufo_data, select = -X)


#1.1 structure of the data frame
str(ufo_data)
#1.2 first 15 rows
ufo_data[1:15,]
ufo_data
#1.3 counting the rows in the df
nrow(ufo_data)
#2 converting the datetime field "chr" to datetime
ufo_data$datetime=as.Date(ufo_data$datetime, format=c("%m/%d/%Y"))
#3
str(ufo_data$datetime)
ufo_data[0,]
#4 Renaming the variable names in the data frame
names(ufo_data)
names(ufo_data)[names(ufo_data) == "duration..seconds."] <- "DurationSeconds"
names(ufo_data)[names(ufo_data) == "duration..hours.min."] <- "DurationHrsMins"
names(ufo_data)[names(ufo_data) == "date.posted"] <- "DatePosted"
names(ufo_data)
View(ufo_data)
#5 type casting the variable latitude
str(ufo_data)
ufo_data$latitude=as.numeric(ufo_data$latitude)
str(ufo_data)
#6
install.packages("mice")
install.packages("VIM")
library(mice)
library(VIM)
#6.1 How many records have no missing data content?
md.pattern(ufo_data)
variables_with_missing <- sum(colSums(is.na(ufo_data)) > 0)
variables_with_missing
#6.2 Records with no missing data
sum(complete.cases(ufo_data))
#6.3 How many variables have the datetime records missing?
dt_rec_missing=sum(is.na(ufo_data$datetime))
dt_rec_missing
#6.4 Which variable has the largest number of missing data points?
max_missing=names(which.max(colSums(is.na(ufo_data))))
max_missing
#6.5 What percent of data is available without missing data points?
ttl_dp=nrow(ufo_data) * ncol(ufo_data)
ttl_dp
missing_dp=sum(is.na(ufo_data))
missing_dp
percentage= ((ttl_dp - missing_dp)/ttl_dp)*100
percentage
#6.6 Discussing the nature of the missing data within the data frame
# The missing data analysis helps in understanding the completeness of the dataset.
# Observing which variables have the most missing values can guide data cleaning and imputation strategies.
# The percentage of data available without missing points gives an overview of the dataset's quality.
# It's important to investigate the reasons behind missing data to decide on appropriate handling methods.
#7 Decide what to do with your missing data. Then apply your suggestion. How many records have you deleted from the ufo data frame?
# Assuming ufo_data is your data frame
png(file = "missingvars.png")
aggr_plot <- aggr(ufo_data, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE,
                  labels=names(ufo_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
dev.off()
ttl_recs <- nrow(ufo_data)
ufo_data_clean <- na.omit(ufo_data)
records_deleted <- ttl_recs - nrow(ufo_data_clean)
records_deleted
View(ufo_data_clean)
#8
ufo_data_sorted <- ufo_data[order(ufo_data$shape, ufo_data$city), ]
View(ufo_data_sorted)
sorted_ufo_data <- ufo_data_sorted[, c("datetime", "city", "country", "shape")]
sorted_ufo_data[1:15,]
#9
ufo_gb_disk <- subset(ufo_data, country == "gb" & shape == "disk")
nrow(ufo_gb_disk)
#10
write.csv(ufo_data_clean, "modified_ufo.csv", row.names = FALSE)
write.csv(ufo_gb_disk, "ufo_gb.csv", row.names = FALSE)
write.csv(sorted_ufo_data, "sorted_ufo.csv", row.names = FALSE)