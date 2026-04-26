#Multivariate Statistics for Geographers
#Homework #5 
#Steven Shi
library(mod)
#Read in datafile
sales <- read.csv("homeSales.csv")
names(sales)

#Estimate Model for part a (base model)
base.model <- lm(price ~ bedrooms + bathrooms + sqft_living + yr_built + HS + SC, data = sales)

summary(base.model)

#Perform the diagnostic plots
plot(base.model)


#Independence and right hand side variables
ResidualVsXPlots <- function(mod.in){
	var.names <- names(mod.in$coefficients)
	n.x.vars <- length(var.names)
	mod.e <- residuals(mod.in)

	for (i in 2:n.x.vars){
		plot (mod.in$model[,var.names[i]], mod.e, xlab = var.names[i], ylab = "residuals")
		lines(lowess(mod.in$model[,var.names[i]],mod.e, f=3/4), col="red")
		locator(1)
		}
	}

ResidualVsXPlots(base.model)


#Homoskedasticity and right hand side variables
XScaleLocationPlots <- function(mod.in){
	var.names <- names(mod.in$coefficients)
	n.x.vars <- length(var.names)
	std.residuals <- sqrt(abs(rstandard(mod.in)))

	for (i in 2:n.x.vars){
		plot (mod.in$model[,var.names[i]], std.residuals, xlab = var.names[i], 
			ylab = "Square root of Absolute Standardized Residuals")
		lines(lowess(mod.in$model[,var.names[i]], std.residuals, f=3/4), col="red")
		locator(1)
		}
	}

XScaleLocationPlots(base.model)


#Calculating VIF

#Install the package
install.packages(c("car"))

#Load the package
library(car)

#Calculate VIF
vif(base.model)

base.model2 <- lm(price ~ bedrooms + bathrooms + yr_built + HS + SC, data = sales)
plot(base.model2)
summary(base.model2)
vif(base.model2)

base.model3 <- lm(price ~ bedrooms + yr_built + HS + SC, data = sales)
plot(base.model3)
summary(base.model3)
vif(base.model3)
#Upload to GitHub and paste your link. 
