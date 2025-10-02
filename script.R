filepath = "https://www.uio.no/studier/emner/matnat/math/STK1000/data/obligdata/oblig1/blinderndaily.txt"
data = read.table(filepath, header=TRUE, sep=";")
head(data, n=3)

# Koden til å løse oppgave 1a
x <- data$temp

min(x)
max(x)


hist(data$temp,
     col = "#2044cc",
     family = "Georgia",
     xlab = "datapunkter",
     ylab = "observasjoner",
     border = NA,
     breaks = 20,
     main = "Histogram")

# Koden til å løse oppgave 1b
mean(data$temp)
median(data$temp)

min(data$temp)
max(data$temp)

# Koden til å løse oppgave 1c
sd(x)
summary(x)
avgx = mean(x)
sdx = sd(x)
qten = 15.93
qttre = 20.18
IQRX = qttre - qten
IQR(x, na.rm = TRUE)

# Koden til å løse oppgave 1d
qqnorm(x)
qqline(x)

# Koden til å løse oppgave 1e
z = (15 - avgx)/sdx

# Koden til å løse oppgave 1f
p20 = 1 - pnorm(20, mean = avgx, sd = sdx)
print(p20)

# Koden for å finne gjennomsnitt og median til oppgave 1g
ind = which(data$year %in% c(2018, 2025))
data_18_25 = data[ind,] ## data for 2018, 2025
data_not18_25 = data[-ind,] ## data for the 8 other years

n18 = sum(data_18_25$year == 2018)
over18 = sum(data_18_25$temp > 20 & data_18_25$year == 2018)
p18 = if (n18 > 0) over18 / n18 else NA
print(n18)
print(over18)
print(p18)

n25 = sum(data_18_25$year == 2025)
over25 = sum(data_18_25$temp > 20 & data_18_25$year == 2025)
p25 = if (n25 > 0) over25 / n25 else NA
print(n25)
print(over25)
print(p25)

nO = nrow(data_not18_25)
overO = sum(data_not18_25$temp > 20)
pO = overO / nO
print(nO)
print(overO)
print(pO)











