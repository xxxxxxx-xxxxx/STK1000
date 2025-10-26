## McK style
mck_blue      <- "#0033A0"
mck_teal      <- "#00A3A1"
mck_grey      <- "#6D6E71"
mck_lightblue <- "#D9EAF7"

datapath = "https://www.uio.no/studier/emner/matnat/math/STK1000/data/obligdata/oblig2/ozone.txt"
newyork = read.table(datapath, header=TRUE)
head(newyork, n=3)

# Data
aq <- airquality
O3 <- aq$Ozone
Temp <- aq$Temp
Month <- aq$Month
Day <- aq$Day

# 2A

mean_O3   <- mean(O3, na.rm = TRUE)
median_O3 <- median(O3, na.rm = TRUE)
sd_O3     <- sd(O3, na.rm = TRUE)
iqr_O3    <- IQR(O3, na.rm = TRUE)
mean_O3; median_O3; sd_O3; iqr_O3
summary(O3)

# Plots
hist(O3, breaks = "FD", col = mck_blue, border = NA,
     main = "2a: Histogram av Ozone (ppb)", xlab = "Ozone (ppb)", ylab = "Frekvens")
boxplot(O3, horizontal = TRUE, col = mck_teal, main = "2a: Boxplot Ozone")
qqnorm(O3, main = "2a: QQ-plot Ozone"); qqline(O3, col = mck_blue, lwd = 2)



# 2B

t_greater_90 <- t.test(O3, mu = 30, alternative = "greater", conf.level = 0.90)
t_greater_99 <- t.test(O3, mu = 30, alternative = "greater", conf.level = 0.99)
t_greater_90
t_greater_99



# 2C

keep <- !is.na(O3) & !is.na(Month)
O3_keep <- O3[keep]; Month_keep <- Month[keep]

grp <- ifelse(Month_keep %in% c(7,8), "JA", "MJS")
grp <- factor(grp, levels = c("MJS","JA"))

# To-utvalgs t-test (Welch)
tt2 <- t.test(O3_keep ~ grp)
tt2

# Sammenligning
par(mfrow=c(1,2))
boxplot(O3_keep ~ grp, col = c(mck_lightblue, mck_teal),
        main = "2c: Ozone per gruppe", xlab = "Gruppe", ylab = "Ozone (ppb)")
stripchart(O3_keep ~ grp, vertical = TRUE, method = "jitter",
           pch = 16, col = mck_grey, add = TRUE)
par(mfrow=c(1,1))

# Reset
par(op)