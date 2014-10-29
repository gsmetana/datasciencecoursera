## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(ggplot2)
library(reshape2)

type_BCMD <- function(year, type){ sum(NEI[NEI$Pollutant == "PM25-PRI" & NEI$year == year & NEI$fips == "24510" & NEI$type == type, "Emissions"] )  }
year <- c(1999, 2002, 2005, 2008 )
type <- c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD")

# find total for each source type
total <- lapply( type, function(y) sapply(year, type_BCMD, type = y) )

# reshape for plotting
data <- data.frame(matrix(unlist(total), nrow=4, byrow=F))
names(data) <- type
data$year <- year
data <- melt(data,id.vars = c("year"))

p <-qplot(year, value, data=data, color=variable, xlab = "Year", ylab = expression("PM"[2.5]*" emissions [tons]"),
      main = expression("Baltimore City, MD PM"[2.5]*" emissions"))
p<- p+scale_color_discrete(name="Source type")
ggsave(filename="plot3.png", height=4.80, width=4.80, dpi=100)