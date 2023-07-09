library(scatterplot3d)

#Loading data from CSV file into RStudio environment
cyclone_data = read.csv('cyclone_data.csv', row.names = 1)
View(cyclone_data)

#Generating regression coefficients
coeff1 = cor.test(cyclone_data$G, cyclone_data$R)
coeff2 = cor.test(cyclone_data$G, cyclone_data$B)

coeff1
coeff2

#Building multiple regression model
regmodel = lm(G ~ R + B, data = cyclone_data)
regmodel
summary(regmodel)

#Plotting the data points and the regression plane
plot = scatterplot3d(cyclone_data$R, cyclone_data$B, cyclone_data$G,
                     xlab = "Red", ylab = "Blue", zlab = "Green",
                     main = "RGB values for cyclone dataset", 
                     pch = 20, color = "darkorange")

legend("topright", legend = levels(factor(cyclone_data$Label)),
       col = c("darkorange"), pch = 20, inset = -0.1, 
       xpd = TRUE , horiz = TRUE)

plot$plane3d(regmodel, draw_polygon = TRUE, 
             polygon_args = list(col = rgb(.1, .2, .7, .3)))

#Prediction of value 
#Consider the data point (165, 170, 186)  {(R, G, B)}

R = 165
B = 186
G_pred = regmodel$coefficients[2]*R + regmodel$coefficients[3]*B + regmodel$coefficients[1]
G_pred
