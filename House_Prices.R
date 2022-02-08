rm(list = ls())

library(dplyr)
library(stringr)
library(stats)
library(corrplot)
library(splines)
library(ggplot2)



# -----------------------------------------------------------------------------

# PREPARING DATA SET TO BE USED

# -----------------------------------------------------------------------------

# Importing data

h.prices = read.csv("House_Prices/data.csv")

head(h.prices)

# -----------------------------------------------------------------------------

# Handling dates 

h.prices$date = substr(h.prices$date, start = 1, stop = 10)

head(h.prices)

h.prices$date = as.Date(h.prices$date, format = "%Y-%m-%d")

class(h.prices$date)

# -----------------------------------------------------------------------------

# Is there any NAs?

h.prices.na = na.omit(h.prices)

# There're not NAs

# Are there logic values?

# There are some values with 0 price associated. Placed the median of prices instead

h.prices[h.prices$price == 0,]$price = median(h.prices$price)


# -----------------------------------------------------------------------------

# Rescaling prices to thousands of euros

h.prices$price = h.prices$price/1000

# -----------------------------------------------------------------------------

# Converting Statezip to factor

h.prices$statezip = as.numeric(substr(h.prices$statezip, start = 4, stop = 8))

class(h.prices$statezip)

# -----------------------------------------------------------------------------

# dropping non-interested variables

h.prices = h.prices[,-c(15,18)]

# -----------------------------------------------------------------------------

# Let's see data set

head(h.prices)






# -----------------------------------------------------------------------------

# Visualazing

# -----------------------------------------------------------------------------

# CORRELATION MATRIX

c_matrix = cor(h.prices[,c(-1,-15)])

corrplot(c_matrix, method = "pie")



# PRICE

# Visualazing first variable Price (variable of interest)

ggplot(h.prices, aes(x=price)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5) +
  
  geom_density(alpha=0.6)+
  
  geom_vline(data=h.prices, aes(xintercept=median(price)),
             linetype="dashed")+
  
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  labs(title="Prices histogram plot",x="Indexes", y = "Price")+
  
  theme_classic()

ggplot(h.prices ,aes(y = price)) + 
  
  geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2) +
  
  labs(title = "Boxplot of prices in thousands of euros",
       y = "Thousands of euros") +
  
  theme_classic()


# H.prices has got outliers... let's remove them

iqr = IQR(h.prices$price)

lower_boundary = quantile(h.prices$price, probs = 0.25, na.rm = FALSE) - 1.5 * iqr
upper_boundary = quantile(h.prices$price, probs = 0.75, na.rm = FALSE) + 1.5 * iqr

outliers = as.integer(
        rownames(h.prices[h.prices$price < lower_boundary | h.prices$price > upper_boundary, ]))

# We remove outliers

h.dropout = h.prices[-outliers,]

# Histogram

ggplot(h.dropout, aes(x=price)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.7,
                 bins = 30, fill = "light gray", col = "black") +
  
  geom_density(alpha=0.7, lwd = 2, col = "blue")+
  
  geom_vline(data=h.dropout, aes(xintercept=median(price)),
             linetype="dashed") +
  
  labs(title="Prices",x="Indexes", y = "Prices")+
  
  theme_classic()




# Boxplot

ggplot(h.dropout ,aes(y = price), ) + 
  
  geom_boxplot(outlier.colour="black", outlier.shape=1,
               outlier.size=2,
               fill = "gray") +
  
  labs(title = "Boxplot of prices in thousands of euros",
       y = "Thousands of euros") +
  
  theme_classic()

# QQ-plot

qqnorm(h.dropout$price,
       main = "QQ-plot of prices with outliers ruled out")

qqline(h.dropout$price,
       lwd = 2, col = "red")

# Plot of prices

plot(h.dropout$price,
     pch = 19, cex = 0.7,
     col = "light gray",
     main = "Prices in thousands of euros",
     ylab = "Price in thousands of euros")

abline(h = median(h.dropout$price),
       col = "black",
       lwd = 2)




# How many different days? ----------------------------------------------------

unique(h.dropout$date)

par(mfrow= c(1,1))


tb1 = with(h.dropout, table(h.dropout$date))

barplot(tb1,
        beside = TRUE,
        main = "Prices released vs date",
        col = "light gray",
        ylab = "Prices released",
        xlab = "Date")



# Plotting bedrooms -----------------------------------------------------------

# Converting bedrooms to factors

h.dropout$bedrooms = factor(h.dropout$bedrooms,
                            levels = c(0,1,2,3,4,5,6,7,8,9),
                            labels = c("0b","1b", "2b", "3b", "4b", "5b", "6b", "7b", "8b", "9b"))

# Plotting price vs date in terms of bedrooms

plot(h.dropout$price ~ h.dropout$date,
     pch = 19, cex = 0.7,
     main = "Price vs date",
     type = 'n')


attach(h.dropout)

with(subset(h.dropout, bedrooms == "0b"),
     points(date,price , col = "red"))

with(subset(h.dropout,bedrooms == "1b"),
     points(date,price , col = "blue"))

with(subset(h.dropout,bedrooms == "2b"),
     points(date,price , col = "orange"))

with(subset(h.dropout,bedrooms == "3b"),
     points(date,price , col = "pink"))

with(subset(h.dropout,bedrooms == "4b"),
     points(date,price , col = "purple"))

with(subset(h.dropout,bedrooms == "5b"),
     points(date,price , col = "yellow"))

with(subset(h.dropout,bedrooms == "6b"),
     points(date,price , col = "black"))

with(subset(h.dropout,bedrooms == "7b"),
     points(date,price , col = "green"))

with(subset(h.dropout,bedrooms == "8b"),
     points(date,price , col = "dark green"))

with(subset(h.dropout,bedrooms == "9b"),
     points(date,price , col = "brown"))

legend("topleft",
       legend = c("0b","1b", "2b", "3b", "4b", "5b", "6b", "7b", "8b", "9b"),
       col = c("red", "blue", "orange", "pink","purple", "yellow", "black", "green", "dark green", "brown"),
       pch = c(1,1,1,1,1,1,1,1,1,1))

ggplot(h.dropout ,aes(x = bedrooms, y = price, fill = bedrooms), ) + 
  
  geom_boxplot(outlier.colour="black", outlier.shape=1,
               outlier.size=2) +
  
  labs(title = "Boxplot of prices in thousands of euros",
       y = "Thousands of euros") +
  
  theme_classic()

# As we can see fromt the graph there are just only 3 cases for 0b 8b and 9b. SO discard them

h.dropout[h.dropout$bedrooms == "0b" | h.dropout$bedrooms == "8b" | h.dropout$bedrooms == "9b",]

h.dropout = h.dropout[-c(242,2366,3912),]

unique(h.dropout$bedrooms)

bedrooms = droplevels(h.dropout$bedrooms)

levels(bedrooms)

# Moreover there are outliers in classes 2b and 3b

# Outliers class 2b

subset1 = subset(h.dropout,h.dropout$bedrooms == "2b")

iqr = IQR(subset1$price)

lower_boundary = quantile(subset1$price, probs = 0.25, na.rm = FALSE) - 1.5 * iqr
upper_boundary = quantile(subset1$price, probs = 0.75, na.rm = FALSE) + 1.5 * iqr

outliers = as.integer(
  rownames(subset1[subset1$price < lower_boundary | subset1$price > upper_boundary ,]))

h.dropout = h.dropout[-outliers,]

# Outliers class 3b

subset2 = subset(h.dropout,h.dropout$bedrooms == "3b")

iqr = IQR(subset2$price)

lower_boundary = quantile(subset2$price, probs = 0.25, na.rm = FALSE) - 1.5 * iqr
upper_boundary = quantile(subset2$price, probs = 0.75, na.rm = FALSE) + 1.5 * iqr

outliers = as.integer(
  rownames(subset2[subset2$price < lower_boundary | subset2$price > upper_boundary ,]))

h.dropout = h.dropout[-outliers,]



# Bathrooms -------------------------------------------------------------------

ggplot(h.dropout, aes(x=bathrooms)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,
                 bins = 17, fill = "light gray", col = "black") +
  
  geom_density(alpha=0.6)+
  
  geom_vline(data= h.dropout, aes(xintercept=median(bathrooms)),
             linetype="dashed")+
  
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  labs(title="Prices histogram plot",x="Indexes", y = "Price")+
  
  theme_classic()

# Scatter plot with loess

ggplot(h.dropout, aes(x=bathrooms, y=price)) + 
  geom_point()+
  geom_smooth() +
  labs(title = "Price vs Bathroom")


# Sqlt ------------------------------------------------------------------------

ggplot(h.dropout, aes(x=sqft_above)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,
                 bins = 17, fill = "light gray", col = "black") +
  
  geom_density(alpha=0.6)+
  
  geom_vline(data= h.dropout, aes(xintercept=median(sqft_above)),
             linetype="dashed")+
  
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  labs(title="sqft_above histogram plot",x="Indexes", y = "Price")+
  
  theme_classic()


ggplot(h.dropout, aes(x=sqft_basement)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,
                 bins = 17, fill = "light gray", col = "black") +
  
  geom_density(alpha=0.6)+
  
  geom_vline(data= h.dropout, aes(xintercept=median(sqft_basement)),
             linetype="dashed")+
  
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  labs(title="sqft_basement histogram plot",x="Indexes", y = "Price")+
  
  theme_classic()

ggplot(h.dropout, aes(x=sqft_living)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,
                 bins = 17, fill = "light gray", col = "black") +
  
  geom_density(alpha=0.6)+
  
  geom_vline(data= h.dropout, aes(xintercept=median(sqft_living)),
             linetype="dashed")+
  
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  labs(title="sqft_living histogram plot",x="Indexes", y = "Price")+
  
  theme_classic()

ggplot(h.dropout, aes(x=sqft_lot)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,
                 bins = 17, fill = "light gray", col = "black") +
  
  geom_density(alpha=0.6)+
  
  geom_vline(data= h.dropout, aes(xintercept=median(sqft_lot)),
             linetype="dashed")+
  
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  labs(title="Sqft_lot histogram plot",x="Indexes", y = "Price")+
  
  theme_classic()





# FLOORS ----------------------------------------------------------------------


h.dropout$floors = factor(h.dropout$floors,
                            levels = c(1,1.5,2,2.5,3,3.5),
                            labels = c("1f","1.5f", "2f", "2.5f", "3f", "3.5f"))

levels(h.dropout$floors)

ggplot(h.dropout, aes(x=floors, y=price, color=floors, shape = floors)) +
  geom_point() + 
  labs(title = "Price vs Floors")

# There are houses with one floors excesively expensive....., their might be luxury
# apartments




# WATERPLANTS ------------------------------------------------------------------

yes = length(rownames(h.dropout[h.dropout$waterfront == 1,]))

no = length(rownames(h.dropout[h.dropout$waterfront == 0,]))

slices = c(yes,no)

labels = c("yes", "no")

slices = as.data.frame(slices)

ggplot(slices, aes(x="", y=slices, fill=labels)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() +
  labs(title = "Pie chart of water plant")


# VIEW -------------------------------------------------------------------------

unique(h.dropout$view)

h.dropout$view = factor(h.dropout$view,
                          levels = c(0,1,2,3,4),
                          labels = c("0v","1v","2v","3v","4v"))


ggplot(h.dropout, aes(x=view, y=price, color=view, shape = view)) +
  geom_point() + 
  labs(title = "Price vs View" )



       
# CONDITION -------------------------------------------------------------------

par(mfrow = c(1,2))
       

unique(h.dropout$condition)


ggplot(h.dropout, aes(x=condition, y=price)) +
  geom_point() + 
  labs(title = "Price vs Condition")


h.dropout$condition = factor(h.dropout$condition,
                        levels = c(1,2,3,4,5),
                        labels = c("1","2","3","4","5"))


ggplot(h.dropout, aes(x=condition, y=price, color=condition)) +
  geom_point() + 
  labs(title = "Price vs condition")


# YEARS BUILT/RENOVATED --------------------------------------------------------

par(mfrow  = c(1,2))


ggplot(h.dropout, aes(x=yr_built)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,
                 bins = 17, fill = "light gray", col = "black") +
  
  geom_density(alpha=0.6)+
  
  geom_vline(data= h.dropout, aes(xintercept=median(yr_built)),
             linetype="dashed")+
  
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  labs(title="Years built histogram plot",x="Indexes", y = "Price")+
  
  theme_classic()


ggplot(h.dropout, aes(x=yr_renovated)) +
  
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,
                 bins = 17, fill = "light gray", col = "black") +
  
  geom_density(alpha=0.6)+
  
  geom_vline(data= h.dropout, aes(xintercept=median(yr_renovated)),
             linetype="dashed")+
  
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  
  labs(title="Years renovated histogram plot",x="Indexes", y = "Price")+
  
  theme_classic()





# LOCATION ---------------------------------------------------------------------

par(mfrow = c(1,1))

# Counting houses according to city

c = unique(h.dropout$city) 

n = length(c)

count = as.numeric(n)

for(i in 1:n){
  
  subset = h.dropout[h.dropout$city == c[i],]
  count[i] = nrow(subset)
}

ind = order(count, decreasing = TRUE)

c[ind][c(1:10)]

df = data.frame(
  cities = c[ind][c(1:20)],
  freq = count[ind][c(1:20)]
)

df


# PIE CHART WITH THE BEST 5

slices = count[ind][c(1:5)]
labels = c[ind][c(1:5)]

slices = as.data.frame(slices)

ggplot(slices, aes(x="", y=slices, fill=labels)) +
  geom_bar(stat="identity", width=1, color="white") +
  
  coord_polar("y", start=0) +
  
  theme_void() +
  labs(title = "Pie chart of water plant")


# ------------------------------------------------------------------

# TRAINING AND VALIDATION DATASETS (FORECASTING)

# ------------------------------------------------------------------

# TRAINING SET

var_int = sort(h.dropout$price)

# Training indexes

len = round(0.7*nrow(h.dropout),0)

train = var_int[c(1:len)]

val = var_int[-c(1:len)]


# PREDICTION ------------------------------------------------------------------

# We'll do a grid search for tuning the hyperparameter in a cubic p-spline

# But first we need to get oriendes

x = seq(1,length(train), length = length(train))

p1 = smooth.spline(x,train, cv = TRUE)

which(p1$x - x != 0) # we need to check same indexes

spar = p1$spar

# Grid

len_grid = 1000

grid = seq(spar - 0.5, spar + 0.5, length = len_grid)

# Loop all over the grid

rmsetrai_err = rmseval_err = numeric(len_grid)

n_val = nrow(h.dropout) - length(train)

x_val = seq(len,len + n_val,length = n_val)

for(i in 1:1000){
  
  p = smooth.spline(train, cv = TRUE, spar = grid[i])
  
  # Training erros
  
  rmsetrai_err[i] = sqrt( mean( (train - fitted(p))^2 ))
  
  # Prediction
  
  y.pr = predict(p, x = x_val)$y
  
  rmseval_err[i] = sqrt(mean( (val - y.pr)^2 ))
  
  
}

i.best = which.min(rmseval_err)

p_best = smooth.spline(train, cv = TRUE, spar = grid[i.best])

plot(var_int, pch = 1,
     cex = 0.7,
     main = "Prices Sorted",
     xlab = "Index",
     ylab = "Price in thousand of euros")

points(fitted(p_best), pch = 19,
       cex = 0.4, col = "red")

y.pr = predict(p_best, x = x_val)$y

points(x_val, y.pr, pch = 19,
       cex = 0.7, col = "orange")

rmsetrai_err[i.best]

rmseval_err[i.best]

legend("topright",
       legend = c("Data", "Training pred", "Validation pred"),
       col = c("Black", "red", "orange"),
       pch = c(1,19,19))

