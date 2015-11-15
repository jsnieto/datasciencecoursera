filename <- "household_power_consumption.txt"
header <- scan(file = filename, sep = ";", nlines = 1, what = "character")
dataset <- read.csv(file = filename, header = FALSE, sep = ";", skip = 66637, nrows = (69517 - 66637) , col.names = header)
dataset$Time <- paste(dataset$Date, dataset$Time)
dataset$Time <- strptime(dataset$Time, "%d/%m/20%y %H:%M:%S")
png("plot1.png", width = 480, height = 480)
hist(dataset$Global_active_power, xlab = "Global active power (kilowatts)", main = "", col = "red")
dev.off()