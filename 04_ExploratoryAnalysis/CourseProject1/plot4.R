data <- read.csv("household_power_consumption.txt", header=TRUE, sep=';', na.strings="?")
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
data<-subset(data, Date =="2007-02-01" | Date=="2007-02-02")


data$Global_active_power <- as.numeric(data$Global_active_power)
data$DateTime <- as.POSIXct(paste(data$Date, data$Time))

png("plot4.png", height=480, width=480)
par(mfrow = c(2,2))
plot(data$Global_active_power~data$DateTime, type="l", ylab="Global Active Power", xlab="")

plot(data$Voltage~data$DateTime, type="l", ylab="Voltage", xlab="datetime")

plot(data$Sub_metering_1~data$DateTime, type="n", ylab="Energy sub metering", xlab="")
lines(data$Sub_metering_1~data$DateTime, col="black")
lines(data$Sub_metering_2~data$DateTime, col="red")
lines(data$Sub_metering_3~data$DateTime, col="blue")
legend("topright", lty=c(1,1,1), col = c("black","red","blue"), 
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), bty = "n" )

plot(data$Global_reactive_power~data$DateTime, type="l", ylab="Global_reactive_power", xlab="datetime")


dev.off()