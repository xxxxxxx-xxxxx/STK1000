## McK style
mck_blue      <- "#0033A0"
mck_teal      <- "#00A3A1"
mck_grey      <- "#6D6E71"
mck_lightblue <- "#D9EAF7"
op <- par(family = "Arial")

datapath = "https://www.uio.no/studier/emner/matnat/math/STK1000/data/obligdata/oblig2/gene.txt"
genes = read.table(datapath, header=TRUE, sep=";")
Gene.Lengths <- genes$Gene.Lengths

# 1A

mu    <- mean(Gene.Lengths, na.rm = TRUE)
sigma <- sd(Gene.Lengths,   na.rm = TRUE)

cat("1a) mu =", round(mu,2), " sigma =", round(sigma,2), "\n")

hist(Gene.Lengths,
     breaks = "FD",
     col = mck_lightblue, border = NA,
     main = "Oppg 1a: Histogram av genlengder",
     xlab = "Genlengde", ylab = "Frekvens")

qqnorm(Gene.Lengths, main = "Oppg 1a: QQ-plot")
qqline(Gene.Lengths, col = mck_teal, lwd = 2)



# 1B

set.seed(100)
samp100_once <- sample(Gene.Lengths, size = 100, replace = TRUE)
xbar100_once <- mean(samp100_once)
cat("1b) Ett utvalg n=100, xbar =", round(xbar100_once,2), "\n")

B <- 100
means100 <- replicate(B, mean(sample(Gene.Lengths, 100, replace = TRUE)))

cat("1b) Gj.snitt av 100 xbar (n=100) =", round(mean(means100),2),
    "  sd(xbar) =", round(sd(means100),2),
    "  teori sigma/sqrt(n) =", round(sigma/sqrt(100),2), "\n")

hist(means100,
     breaks = "FD",
     col = mck_lightblue, border = NA,
     main = "Oppg 1b: Fordeling av xbar (n=100, 100 rep)",
     xlab = "xbar", ylab = "Frekvens")

qqnorm(means100, main = "Oppg 1b: QQ-plot av xbar (n=100)")
qqline(means100, col = mck_teal, lwd = 2)



# 1C

set.seed(101)
B <- 100
means10 <- replicate(B, mean(sample(Gene.Lengths, 10, replace = TRUE)))

cat("1c) Gj.snitt av 100 xbar (n=10)  =", round(mean(means10),2),
    "  sd(xbar) =", round(sd(means10),2),
    "  teori sigma/sqrt(n) =", round(sigma/sqrt(10),2), "\n")

hist(means10,
     breaks = "FD",
     col = mck_lightblue, border = NA,
     main = "Oppg 1c: Fordeling av xbar (n=10, 100 rep)",
     xlab = "xbar", ylab = "Frekvens")

qqnorm(means10, main = "Oppg 1c: QQ-plot av xbar (n=10)")
qqline(means10, col = mck_teal, lwd = 2)



# 1D

p10   <- 1 - pnorm(3000, mean = mu, sd = sigma/sqrt(10))
p100  <- 1 - pnorm(3000, mean = mu, sd = sigma/sqrt(100))
cat("1d) P(xbar > 3000)  n=10  =", round(p10,4), "\n")
cat("1d) P(xbar > 3000)  n=100 =", round(p100,6), "\n")



# 1E

cat("1e) xbar er ubiasert for mu (bias = 0). Var(xbar) = sigma^2 / n.\n")
par(op)