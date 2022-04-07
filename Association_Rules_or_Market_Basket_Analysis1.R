# Association Rules is also called Market Basket Analysis or Affinity analysis
# It is used to uncover links btw items by large retailers. It works by searching for combinations of items that happen in transactions together
# It uses such info to recognize customer purchasing patterns, to identify who customers are, to understand why you buy certain items and to know which products are purchased jointly so as to know which products to promote

# We want to find out which items in the dataset are bought together using Association Rules Analysis

mydata <- read.csv("Cosmetics.csv", header = T, colClasses = "factor")   # colClasses = "factor" will change the datatype of all columns in the dataset to factor becuz Association Rules requires factor datatype
str(mydata) 
summary(mydata)

# Finding Association rules
# install.packages("arules")  # arules- association rules
library(arules)

yrdata <- apriori(mydata)   # minlen - min length - min number of items in each rule
summary(yrdata)   # displays number of rules per item- 3 rules with 1 item, 85 rules in 2 items, 10739 rules with 5 items. also displays total number of rules- 68880 rules (which is to much) 
# support column denotes d % of cases that include both 'If' and 'then' items(ie if Nail.Polish was bought, then Brushes were bought), confidence column denotes d % of cases with 'If' that also have 'then' items, lift column denotes confidence column / % of cases with 'then' items


# reducing the number of Rules by using only with specified parameter values
myrules <- apriori(mydata, parameter = list(minlen = 2, maxlen = 3,   # using only rules 2 and 3, supp = 0.7 means we use a min value of 0.7 in support column(from summary(yrdata) above)
                                            supp = 0.7))    # now the rules has been reduced to just 15 rules
inspect(myrules)  # display d rules of all columns which are now values in lhs column(left hand side) which is 'IF' and rhs column(right hand side) which is 'Then' but they are all = NO (i.e which items was not bought- If Nail.Polish was not bought in lhs, Then Brushes was not bought in rhs). Since we want to see which item was bought, dis rule is not helpful. 


# Finding interesting rules- 1 (i.e rules that will display Yes in the lhs and rhs columns)
summary(mydata)   # we see that Foundation column has highest values of Yes among all columns(as such it is d most popular item bought) and Eyebrow.Pencils has highest value for NO

myrules <- apriori(mydata, parameter = list(minlen = 2, maxlen = 3,
                                           conf = 0.7),
                   appearance = list(rhs = c("Foundation=Yes"),   # using Foundation=Yes(mk sure dere is no space btw d word at RHS of = and d word at LHS of =) because Foundation has highest values of Yes among all columns
                                     default = "lhs"))
inspect(myrules)  # display rules with Yes values(ie If Lip.Gloss was bought in lhs, Then Foundation was also bought in rhs) in lhs and rhs columns but there are still rules with No values displayed in lhs so dis rules is not of interest to us


# Graphs and Chart
install.packages("arulesViz")

library(arulesViz)
plot(myrules)   # displays scatterplot chart of d rules using confidence, support and lift values 

plot(myrules, method = "grouped")   # d sizes of d bubbles is determined by support column values while color is lift column values

plot(myrules, method = "graph", control = list(type = "items"))



# Finding interesting rules- 3 (ie displaying only rules with Yes values in lhs and rhs columns)
myrules <- apriori(mydata, parameter = list(minlen = 2, maxlen = 3,
                                           conf = 0.5),
                   appearance = list(rhs = c("Foundation=Yes"), lhs = c("Bag=Yes", "Blush=Yes",
                                                                      "Nail.Polish=Yes", "Brushes=Yes", "Concealer=Yes", "Eyebrow.Pencils=Yes",
                                                                      "Bronzer=Yes", "Lip.liner=Yes", "Mascara=Yes", "Eye.shadow=Yes",
                                                                      "Lip.Gloss=Yes", "Lipstick=Yes", "Eyeliner=Yes"), default = "none"))  # listing all the rules in lhs and rhs columns that have value- Yes
quality(myrules) <- round(quality(myrules), digits = 3)  # dis rounds-up all d values in support, confidence and lift columns to 3 d.p. mk sure to use quality(myrules) as object name and also inside round()
inspect(myrules)             # displays only rules with Yes values in lhs and rhs columns (ie If people bought Lipstick Then they also bought Foundation etc)


# Finding Redundant rules
redun <- is.subset(myrules, myrules)

redun[lower.tri(redun, diag = T)] <- NA

myredun <- colSums(redun, na.rm = T)>= 1

which(myredun)      # displays only the rules that are redundant 


# Removing Redundant rules
yrrules <- myrules[!myredun]  # removes d redundant rules

yrrules <- sort(yrrules, by = "lift")

inspect(yrrules)


