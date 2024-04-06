#1.Write dplyr and ggplot code to answer the following questions?

library(dplyr)
library(ggplot2)
hospital_data <- read.csv("hospitals.csv")

#e) Hospital with the Lowest Number of Beds
hospital_lowest_beds <- hospital_data %>%
  filter(Beds == min(Beds))

#f) Hospital with the Lowest Expense
hospital_lowest_expense <- hospital_data %>%
  filter(Total.Expense == min(Total.Expense))

#g) Number of Hospitals that Deliver Babies
hospitals_deliver_babies <- hospital_data %>%
  filter(Births > 0) %>%
  count()

#h) Scatterplot of Number of Beds vs Total Expense
ggplot(hospital_data, aes(x = Beds, y = Total.Expense)) +
  geom_point() +
  labs(title = "Number of Beds vs Total Expense", x = "Number of Beds", y = "Total Expense")

#i) Scatterplot of Admissions vs Total Expense
ggplot(hospital_data, aes(x = Admissions, y = Total.Expense)) +
  geom_point() +
  labs(title = "Admissions vs Total Expense", x = "Admissions", y = "Total Expense")

#j) Scatterplot of Beds vs Total Expense for Hospitals that Deliver Babies
hospital_data %>%
  filter(Births > 0) %>%
  ggplot(aes(x = Beds, y = Total.Expense)) +
  geom_point() +
  labs(title = "Beds vs Total Expense for Hospitals Delivering Babies", x = "Beds", y = "Total Expense")

#k)relationship between outpatient visits and total expense
ggplot(hospital_data, aes(x = Outpatient.Visits, y = Total.Expense)) +
  geom_point() +
  labs(title = "Outpatient Visits vs Total Expense", x = "Outpatient Visits", y = "Total Expense")

#2. Descriptive Analysis 

#i. For Pie Chart:One slice should be labeled Admissions. Choose another attribute for the second slice.
# Summarize total Admissions and another attribute (e.g., Beds)

total_admissions <- sum(hospital_data$Admissions)
total_beds <- sum(hospital_data$Beds) # Just as an example attribute

pie_data <- data.frame(
  Category = c("Admissions", "Beds"),
  Count = c(total_admissions, total_beds)
)

ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Distribution of Admissions and Beds") +
  theme_void()

#ii. For the column/ bar charts:Ensure that one column is titled Admissions. Choose a different attribute 
#for the other column.

bar_data <- data.frame(
  Category = c("Admissions", "Total Expense"),
  Value = c(sum(hospital_data$Admissions), sum(hospital_data$Total.Expense))
)

ggplot(bar_data, aes(x = Category, y = Value, fill = Category)) +
  geom_col() +
  labs(title = "Comparison of Admissions and Total Expense", x = "", y = "Total") +
  theme_minimal()

#iii. For Line Chart:one of the lines should represent Expense; choose another attribute for the second line.

hospital_data$Date <- as.Date(hospital_data$Date)

ggplot(hospital_data, aes(x = Total.Expense)) +
  geom_line(aes(y = Payroll.Expense, colour = "Payroll Expense")) +
  geom_line(aes(y = Total.Expense, colour = "Total expense")) +
  labs(title = "Trend of Payroll Expense and Total Expense Over Time", x = "Date", y = "Value") +
  theme_minimal() +
  scale_colour_manual(values = c("Payroll Expense" = "blue", "Total expense" = "red"))

#3. Perform one simple regression (one predictor) to provide recommendations.

model <- lm(Total.Expense ~ Beds, data = hospital_data)
summary(model)

ggplot(hospital_data, aes(x = Beds, y = Total.Expense)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regression of Total Expense on Beds", x = "Beds", y = "Total Expense")

#iii. What is the value of the R^2?

#R^2=0.6043

#iv. What does the 𝑅! measure in this case? ( Hint: Percentage of variation in ... explained by ...)

# R^2 measures the proportion of the variance in the dependent variable(total expenses) that is 
#predictable from the independant variable (Beds).

#v. What are the pvalues ? How many pvalues are reported, why ?What does each pvalue mean? 
#(Hint: pvalues have are related to generalizing from the sample to the population: what is
#the sample here, what is the population?) (1-2 sentences)

#intercept p-value and slope(Beds) p-value

#vi. Explain R square, pvalues.

#R^2 i the change of Y that is explained by x, P-vaulue is the measure in the null hypothesis/ 
#the probability that the test statistic will be as extreme or more extreme than the observed 
# value under the null hypothesis.

#vii. What would be the right attribute size (independent variable) that seems most appropriate 
#to lead you in the expense range of $55–$75 million?

#55,000,000 = 1084.56x -16060.93
#x=50720.37 
#75,000,000= 1084.56x -16060.93 
#x=69201.77 
#
#4. Perform one multivariate regression to provide recommendations.

#i. The dependent variable should be Total Expense (the column in the data set). 
#Choose two independent variables from one of the remaining attributes.

model_multi <- lm(Total.Expense ~ Admissions + Beds, data = hospital_data)

summary(model_multi)

#ii. What is the value of the R^2?

#R^2=0.7398

#iii. What does the 𝑅! measure in this case? ( Hint: Percentage of variation in ... explained by ...)

# R^2 measures the proportion of the variance in the dependent variable(total expenses) that is 
#predictable from the independant variables (addmissions and beds). It ptovides an indication
#of the goodness of fit and how well the independent variables explain the variability of the depenedent variable.

#iv. What are the pvalues ? How many pvalues are reported, why ?What does each pvalue mean? 
#(Hint: pvalues have are related to generalizing from the sample to the population: what is the
#sample here, what is the population?) (1-2 sentences)

#one for the intercept, the "addmissions" , and the "Beds"

#v. Explain R square, pvalues.
#R^2 indicates the proportion of the total variability in "total expense" explained by the combined
#effect of "addmissions" and "Bedss", P-value provides a method to determine the statistical
#significance of each predictor.