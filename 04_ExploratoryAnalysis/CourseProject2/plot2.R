## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

total_BCMD <- function(year){ sum(NEI[NEI$Pollutant == "PM25-PRI" & NEI$year == year & NEI$fips == "24510", "Emissions"] )  }

year <- c(1999, 2002, 2005, 2008 )
totals <- sapply(year, total_BCMD) 

png("plot2.png", height=480, width=480)
par(mfrow = c(1,1))
plot(totals~year, type="p", main=expression("Total Baltimore City, Maryland PM"[2.5]*" emissions"), ylab=expression("PM"[2.5]*" emissions [tons]"), xlab="Year")
dev.off()