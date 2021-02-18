# Import and read the data
foo <- read.csv("https://tinyurl.com/yb4phxx8") 

# column names of the data set 
names(foo)

# dimensions of the data set
dim(foo)

# print first rows of the data set
head(foo)

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

# this function addresses missing values, turning them into "NA" instead of leaving them blank, and turning the values into "Date objects
# loop through the "date.columns" 
for(i in date.columns)  
  {
# find missing values
  which_values_are_missing <- which(as.character(foo[, i]) == "")
# replace missing values with NA
  foo[which_values_are_missing, i] <- NA
# turn values into "Date" objects
  foo[, i] <- as.Date(as.character(foo[, i]))
  }

# remove NAs for column "Rating" in "foo" 
which.have.NAs <- which(is.na(foo$Rating == TRUE)) 
new_foo <- foo[-which.have.NAs, ]

#### ASSIGNMENT: 
# Question 1: 
# as instructed in the prompt, I will only consider projects with non-missing “Circulation.Date” >= 2009-01-01
# remove missing values 
noNA_foo <- new_foo[!is.na(new_foo$CirculationDate), ] 
# only consider “Circulation.Date” >= 2009-01-01 
df <- noNA_foo[which(noNA_foo$CirculationDate >= as.Date("2009-01-01")), ] 

# Question 1(a): 
# remove projects that do not have original completion dates 
# First step, identify missing original completion dates 
no_orgcompdate <- which(is.na(df$OriginalCompletionDate)) 
# remove
df_withDates <- df[-no_orgcompdate,] 

#calculate expected duration of projects 
expected_duration <- mean(df_withDates$OriginalCompletionDate) - mean(df_withDates$ApprovalDate) 
#print expected duration of projects 
expected_duration

################################
# Question 1(b):
# Create a circulation Year column, "%Y" is to keep the year as 4 digits 
df_withDates$CirculationYear <- format(df_withDates$CirculationDate, "%Y")

# check if there are any NAs in the Revised Completion Date column
sum(is.na(df_withDates$RevisedCompletionDate))

# Create a delay column
df_withDates$Delay <- df_withDates$RevisedCompletionDate - df_withDates$OriginalCompletionDate 

#load the dplyr library that has a data processing pipeline
install.packages("dplyr")
library (dplyr)

# this function calculates the mean, median and interquartile range of the project delay in different circulation year 
# "%>%" passes what is to the left to the following function 
delayByYear <- df_withDates %>%  
  group_by(CirculationYear) %>%    
  summarise(mean.delay = mean(Delay), 
            median.delay = median(Delay),
            IQR.delay = quantile(Delay, 0.75) - quantile(Delay, 0.25))
delayByYear

# plot a graph of mean, median and interquartile range of the project delay in different circulation year to see the pattern visibly 
plot(delayByYear$CirculationYear, delayByYear$mean.delay, 
     pch=15, col="red", ylim=c(100, 800), 
     xlab="Circulation Year", ylab="Delay (days)", main="Project Delay")
points(delayByYear$CirculationYear, delayByYear$median.delay, pch=16, col="blue")
points(delayByYear$CirculationYear, delayByYear$IQR.delay, pch=17, col="green")
legend("bottomleft", pch=c(15,16,17), 
       col=c("red","blue","green"), 
       legend=c("Mean Delay", "Median Delay", "IQR of Delays"))
grid(nx=NA, ny=NULL) 

################################
# Question 1(c)
# Create actual duration column
df_withDates$ActualDuration <- df_withDates$RevisedCompletionDate - df_withDates$ApprovalDate

# Create expected duration column 
df_withDates$ExpectedDuration <- df_withDates$OriginalCompletionDate - df_withDates$ApprovalDate

# The difference between original planned project duration and actual duration
mean(df_withDates$ExpectedDuration) - mean(df_withDates$ActualDuration) 
median(df_withDates$ExpectedDuration) - median(df_withDates$ActualDuration)
quantile(df_withDates$ExpectedDuration) - quantile(df_withDates$ActualDuration)
IQR(df_withDates$ExpectedDuration) - IQR(df_withDates$ActualDuration) 

################################
# Question 2
# Narrow the data set down to projects completed between 2010 and now 
df2010 <- noNA_foo[which(noNA_foo$RevisedCompletionDate >= as.Date("2010-01-01")), ]

# print the title of the table
print("Distribution of Project Ratings")
# print the table that shows the distribution of project ratings
prop.table(table(df2010$Rating)) * 100  

################################
# Question 3:
# Create data frame that contains PATA projects only
df_PATA <- df2010[which(df2010$Type == "PATA"),]

#print the title of the table
print("Distribution of Project Ratings (PATA only)")
# print the table that shows the distribution of the PATA project ratings
table(df_PATA$Rating) / length(df_PATA$Rating) * 100


################################
# Question 4: 
# Top 10% of projects by final project budget
df_TopQuant <- df[which(df$RevisedAmount >= quantile(df$RevisedAmount, 0.9)),]
# Bottom 10% of projects by final project budget
df_BottomQuant <- df[which(df$RevisedAmount <= quantile(df$RevisedAmount, 0.1)),]

# Compare the ratings
(table(df_TopQuant$Rating) / length(df_TopQuant$Rating) * 100) - 
  (table(df_BottomQuant$Rating) / length(df_BottomQuant$Rating) * 100)

# Compare other features
# Dept
table(df_TopQuant$Dept) - table(df_BottomQuant$Dept)

# Cluster
(table(df_TopQuant$Cluster) / length(df_TopQuant$Cluster) * 100) - 
  (table(df_BottomQuant$Cluster) / length(df_BottomQuant$Cluster) * 100)

# Country
(table(df_TopQuant$Country) / length(df_TopQuant$Country) * 100) - 
  (table(df_BottomQuant$Country) / length(df_BottomQuant$Country) * 100)
