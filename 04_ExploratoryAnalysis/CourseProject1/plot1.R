data <- read.csv("household_power_consumption.txt", header=TRUE, sep=';', na.strings="?")
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
data<-subset(data, Date =="2007-02-01" | Date=="2007-02-02")
data$Global_active_power <- as.numeric(data$Global_active_power)

png("plot1.png", height=480, width=480)
par(mfrow = c(1,1))
hist(data$Global_active_power, main="Global Active Power",  xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")
dev.off()
