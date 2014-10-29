## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

motorSCC <- SCC[grep("Mobile",SCC$EI.Sector, ignore.case=TRUE),"SCC"]

motor_BCMD <- function(year){ sum(NEI[NEI$Pollutant == "PM25-PRI" & NEI$year == year & NEI$fips == "24510" & NEI$SCC %in% motorSCC
                                   , "Emissions"] )  }

year <- c(1999, 2002, 2005, 2008 )
motor <- sapply(year, motor_BCMD) 

png("plot5.png", height=480, width=480)
par(mfrow = c(1,1))
plot(motor~year, type="p", main=expression("Baltimore City, Maryland PM"[2.5]*" emissions from motor vehicles"), ylab=expression("PM"[2.5]*" emissions [tons]"), xlab="Year")
dev.off()