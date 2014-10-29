## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

coalSCC <- SCC[grep("Coal",SCC$EI.Sector, ignore.case=TRUE),"SCC"]

coal_US <- function(year){ sum(NEI[NEI$Pollutant == "PM25-PRI" & NEI$year == year & NEI$SCC %in% coalSCC
, "Emissions"] )  }

year <- c(1999, 2002, 2005, 2008 )
coal <- sapply(year, coal_US) 

png("plot4.png", height=480, width=480)
par(mfrow = c(1,1))
plot(coal~year, type="p", main=expression("US PM"[2.5]*" emissions from coal"), ylab=expression("PM"[2.5]*" emissions [tons]"), xlab="Year")
dev.off()