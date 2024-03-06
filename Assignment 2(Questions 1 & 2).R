ethiopiafirms <- middle_01_all_clean_1_
summary(ethiopiafirms)

#Question 1

#Histogram of firm employment
hist(ethiopiafirms$emp, breaks = 1000, freq = FALSE, xlim = c(0,100), 
     main = "Distribution of Firm Employment in Ethiopia (Small & Medium Enterprises)",
     xlab = "Number of employees",
     ylab = "Share of Plants",)

# Fitting a density function to the data
density_fit <- density(ethiopiafirms$emp)

# Adding the line of best fit
lines(density_fit, col = "red", lwd = 2)

#Log total distribution
lemp <- log(ethiopiafirms$emp)
hist(lemp, freq = FALSE,
     main = "Log Distribution of Firm Employment in Ethiopia",
     xlab = "Log number of employees",
     ylab = "Share of Plants",)

lempline <- density(lemp)
lines(lempline, col = "pink", lwd = 2)

#Distribution for Sample 1
sample1 <- filter(ethiopiafirms, sample == 1)
hist(sample1$emp, freq = FALSE,
     main = "Distribution of Firm Employment in Ethiopia (Medium & Large Firms)",
     xlab = "Number of employees",
     ylab = "Share of Plants",)

#Log distribution for Sample 1
lemp1 <- log(sample1$emp)
hist(lemp1, freq = FALSE,
     main = "Distribution of Log Firm Employment in Ethiopia (Medium & Large Firms)",
     xlab = "Log number of employees",
     ylab = "Share of Plants",)

#Distribution for Sample 2
sample2 <- filter(ethiopiafirms, sample == 2)
hist(sample1$emp, freq = FALSE,
     main = "Distribution of Firm Employment in Ethiopia (Small-scale Firms)",
     xlab = "Number of employees",
     ylab = "Share of Plants",)

#Log distribution for Sample 2
lemp2 <- log(sample2$emp)
hist(lemp2, freq = FALSE,
     main = "Distribution of Log Firm Employment in Ethiopia (Small-scale Firms)",
     xlab = "Log number of employees",
     ylab = "Share of Plants",)

#It would be misleading to only use Sample 1 as sample 1 focuses on the medium
#and large sized enterprises and using those would skew the mean employment in
#a firm to be much larger than it actually is. It would also not be representative
#of true firm employment distribution in Ethiopia as Sample 1 makes up a considerable
#portion of the number of firms, and on a macro scale, the true employment distribution
#is skewed largely to the left, indicating more small-sized firms, and removing sample
#1 would change this distribution drastically.


#Question 2

#histogram of firm sales
hist(ethiopiafirms$sales, breaks = 50, freq = FALSE, 
     main = "Distribution of Sales in Ethiopia (Small & Medium Enterprises)",
     xlab = "Number of sales",
     ylab = "Share of Plants",)

# Fitting a density function to the data
density_fit2 <- density(ethiopiafirms$sales, na.rm = TRUE)

# Adding the line of best fit
lines(density_fit2, col = "blue", lwd = 2)

#Taking log of sales instead to normalize distribution of sales
lsales <- log(ethiopiafirms$sales)
hist(lsales, freq = FALSE,
     main = "Distribution of Sales in Ethiopia",
     xlab = "Log number of sales",
     ylab = "Share of Plants",)

density_fit3 <- density(lsales, na.rm = TRUE)
lines(density_fit3, col = "green", lwd = 2)

#Taking the log of sales gives a much more normalized plot

plot(ethiopiafirms$emp, ethiopiafirms$sales)
abline(lm(ethiopiafirms$sales ~ ethiopiafirms$emp))

#Since their relationship is so positively related, it stands that as firms expand
#and become larger with more employees, their sales increase, which is evident from
#the similarity between the histograms of employment and sales

#Some of the possible reasons for the differences include: industry and sector
#differences, where some small firms may be specialized in certain industries 
#and may have achieved economies of scale, allowing them to produce a very high
#number of sales with fewer employees. Furthermore, it's important to consider
#the type of output produced by large and medium firms, which may have a lower
#market price if it's mass produced compared to small firms, therefore, requiring
#more sales to break even. 
