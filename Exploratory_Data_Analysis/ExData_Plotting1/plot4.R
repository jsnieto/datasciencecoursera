filename <- "household_power_consumption.txt"
header <- scan(file = filename, sep = ";", nlines = 1, what = "character")
dataset <- read.csv(file = filename, header = FALSE, sep = ";", skip = 66637, nrows = (69517 - 66637) , col.names = header)
dataset$Time <- paste(dataset$Date, dataset$Time)
dataset$Time <- strptime(dataset$Time, "%d/%m/20%y %H:%M:%S")
png("plot4.png", width = 480, height = 480)
par(mfrow = c(2, 2), mar = c(2, 4, 1, 1))
hist(dataset$Global_active_power, xlab = "Global active power (kilowatts)", main = "", col = "red")
plot(dataset$Time, dataset$Voltage, xlab = "datetime", ylab = "Voltage", type = "l")
plot(dataset$Time, dataset$Sub_metering_1, type = "l", ylab = "Energy sub metering (watt hour)", xlab = "")
lines(dataset$Time, dataset$Sub_metering_2, col = "red", xlab = "")
lines(dataset$Time, dataset$Sub_metering_3, col = "blue", xlab = "")
legend("topright", col = c("black", "red", "blue"), legend = c("sub metering 1", "sub metering 2", "sub metering 3"), lty = 1)
plot(dataset$Time, dataset$Global_reactive_power, type = "l", ylab = "Global reactive power (kilowatt)", xlab = "datetime")
dev.off()