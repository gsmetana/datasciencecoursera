## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(ggplot2)
library(reshape2)

motorSCC <- SCC[grep("Mobile",SCC$EI.Sector, ignore.case=TRUE),"SCC"]

motor_BCMD <- function(year){ sum(NEI[NEI$Pollutant == "PM25-PRI" & NEI$year == year & NEI$fips == "24510" & NEI$SCC %in% motorSCC
                                      , "Emissions"] )  }
motor_LACA <- function(year){ sum(NEI[NEI$Pollutant == "PM25-PRI" & NEI$year == year & NEI$fips == "06037" & NEI$SCC %in% motorSCC
                                      , "Emissions"] )  }
year <- c(1999, 2002, 2005, 2008 )

# find total for each location
motor_BA <- sapply(year, motor_BCMD) 
motor_CA <- sapply(year, motor_LACA) 

# reshape for plotting
data <- data.frame(year = year, Baltimore = motor_BA, LA = motor_CA)
data <- melt(data,id.vars = c("year"))

qp<-qplot(year, value, data=data, color=variable, xlab = "Year", ylab = expression("PM"[2.5]*" emissions [tons]"),
         main = expression(atop(paste("Baltimore City and LA PM"[2.5]),
                                "emissions from motor vehicles")))
qp<-qp+scale_color_discrete(name="Location")
ggsave(filename="plot6.png", height=4.80, width=4.80, dpi=100)