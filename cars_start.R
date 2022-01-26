#load the data
cars = read.csv("cars.csv")
names(cars)

#factor some variables to start with
cars$fuel_type <- factor(cars$fuel_type, levels = c("gas", "diesel"))
cars$aspiration <- factor(cars$aspiration, levels = c("std", "turbo"))
cars$num_of_doors <- factor(cars$num_of_doors, levels = c("two", "four"))
cars$engine_type <- na.omit(factor(cars$engine_type, levels = c("dohc","ohc","ohcv","ohcf")))
cars$num_of_cylinders <- factor(cars$num_of_cylinders)
#no specific ordering
cars$body_style <- factor(cars$body_style)
cars$drive_wheels <- factor(cars$drive_wheels)
cars$make <- factor(cars$make)

plot(cars$price ~ cars$fuel_type)

#select data for initial linear model
xy <- cars[ , c("price", "horsepower", "fuel_type", "aspiration", "num_of_doors",
                "body_style", "drive_wheels", "make", "city_mpg", "length")]

