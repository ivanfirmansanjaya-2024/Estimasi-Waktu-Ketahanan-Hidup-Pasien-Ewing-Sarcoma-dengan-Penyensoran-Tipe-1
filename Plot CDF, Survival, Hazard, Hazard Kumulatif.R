library(readxl)
options(OutDec = ",") 
# biar hasilnya desimal

df     <- read_excel("D:/SEMESTER 4/Analisis Data Uji Hidup/YOK ADUH LAGI.xlsx")

waktu  <- df[["Waktu Ketahanan Hidup (Bulan)"]]
status <- df[["Status Kejadian"]]   
# 1=kejadian
# 0=tersensor

# estimasi parameter
theta  <- sum(waktu) / sum(status) 
# hazard
lambda <- 1 / theta       
# waktu saat S(t)
median_hidup <- log(2) / lambda             

# fungsi untuk t = 0 sampai 24 bulan 
t <- seq(0, 24, length.out = 200)
F <- 100 * (1 - exp(-lambda * t))          
S <- 100 * exp(-lambda * t)                
h <- rep(lambda, 200)                      
H <- lambda * t                            

gambar <- function() {
  par(mfrow = c(2, 2), mar = c(4.2, 5.2, 2.6, 1.2), las = 1)
  xlab <- "Waktu Ketahanan Hidup (Bulan)"
  kotak <- function() abline(v = seq(0, 24, 4), h = axTicks(2), col = "grey88")   

# a) CDF
  plot(t, F, type = "l", lwd = 3.5, col = "#1f4e9c", ylim = c(0, 100), xaxt = "n", panel.first = kotak(),
       main = "Fungsi Distribusi Kumulatif (CDF)", xlab = xlab, ylab = "F(t) (%)", font.lab = 2, cex.lab = 1.15)
  axis(1, at = seq(0, 24, 4))
  points(24, F[200], pch = 19, col = "#c0392b", cex = 1.4)
  text(24, F[200], paste0("F(24) = ", format(round(F[200], 1), nsmall = 1), "%"),
       pos = 2, col = "#c0392b", font = 2)

# b) survival
  plot(t, S, type = "l", lwd = 3.5, col = "#1f4e9c", ylim = c(0, 100), xaxt = "n", panel.first = kotak(),
       main = "Fungsi Survival", xlab = xlab, ylab = "S(t) (%)", font.lab = 2, cex.lab = 1.15)
  axis(1, at = seq(0, 24, 4))
  segments(median_hidup, 0, median_hidup, 50, lty = 2, col = "grey50")   # garis putus median
  segments(0, 50, median_hidup, 50, lty = 2, col = "grey50")
  points(c(median_hidup, 24), c(50, S[200]), pch = 19, col = "#c0392b", cex = 1.4)
  text(median_hidup - 0.5, 6, paste0("Median = ", format(round(median_hidup, 1), nsmall = 1), " bln"),
       adj = c(1, 0), col = "#c0392b", font = 2)
  text(24, S[200] - 6, paste0("S(24) = ", format(round(S[200], 1), nsmall = 1), "%"),
       adj = c(1, 1), col = "#c0392b", font = 2)

# c) hazard
  plot(t, h, type = "l", lwd = 3.5, col = "#1f4e9c", ylim = c(0, 0.08), xaxt = "n", panel.first = kotak(),
       main = "Fungsi Hazard", xlab = xlab, ylab = "h(t)", font.lab = 2, cex.lab = 1.15)
  axis(1, at = seq(0, 24, 4))
  text(12, lambda, paste0("h(t) = ", format(round(lambda, 3), nsmall = 3), " (konstan)"),
       pos = 3, col = "#c0392b", font = 2)

# d) hazard kumulatif
  plot(t, H, type = "l", lwd = 3.5, col = "#1f4e9c", ylim = c(0, 1.2), xaxt = "n", panel.first = kotak(),
       main = "Fungsi Hazard Kumulatif", xlab = xlab, ylab = "H(t)", font.lab = 2, cex.lab = 1.15)
  axis(1, at = seq(0, 24, 4))
  points(24, H[200], pch = 19, col = "#c0392b", cex = 1.4)
  text(24, H[200], paste0("H(24) = ", format(round(H[200], 3), nsmall = 3)),
       pos = 2, col = "#c0392b", font = 2)
}

gambar()

