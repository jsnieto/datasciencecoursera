filename <- "household_power_consumption.txt"
header <- scan(file = filename, sep = ";", nlines = 1, what = "character")
dataset <- read.csv(file = filename, header = FALSE, sep = ";", skip = 66637, nrows = (69517 - 66637) , col.names = header)
dataset$Time <- paste(dataset$Date, dataset$Time)
dataset$Time <- strptime(dataset$Time, "%d/%m/20%y %H:%M:%S")
png("plot2.png", width = 480, height = 480)
plot(dataset$Time, dataset$Global_active_power, type = "l", ylab = "Global Active Power (kilowatt)", xlab = "")
dev.off()
