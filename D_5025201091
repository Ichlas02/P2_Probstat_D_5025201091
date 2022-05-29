# Soal 1
# 1a = Standar Deviasi Selisih X dan Y
arrayX <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
arrayY <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)

selisih <- arrayY - arrayX

paste("1a) Standar Deviasi Selisih X dan Y = ", sd(selisih))

# 1b = nilai T-score
paste("1b) T-score Selisih = ")
t.test(selisih)

# 1c = 
var.test(arrayX, arrayY)
t.test(selisih, alternative = "two.sided", var.equal = TRUE)

# Soal 2
# 2a = Apakah Anda setuju dengan klaim tersebut?
cat("2a) Setuju, karena setelah diuji, ternyata tolak H0 (z>Z.alpha) sehingga rata-rata mobil dikemudikan per tahun lebih dari 20.000km")

# 2b = Jelaskan maksud dari output yang dihasilkan!
cat("2b) Output dari z test adalah, hipotesis alternatif, alternative hypothesis: true mean is greater than 20000km atau H1 diterima sehingga klaim benar")

# 2c = Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
cat("2c) Dikarenakan nilai pvalue < alpha(0,05) maka keputusan yang diambil adalah Tolak H0 sehingga disimpulkan bahwa rata-rata mobil dikemudikan pertahun lebih dari 20.000km")

# Soal 3
library(BSDA)
library(mosaic)
z <- function(xSampel, xPopulasi, s, n) {
  return ((xSampel - xPopulasi) / (s / sqrt(n)))
}
# 3a
H0 <- z(3.64, 0, 1.67, 19)
H1 <- z(2.79, 0, 1.32, 27)
paste("3a) H0 = ", H0, ", H1 = ", H1)

# 3b = Hitung Sampel Statistik
cat("3b)")
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y =2.79 , s.y = 1.32, n.y = 27, alternative = "greater", var.equal = TRUE)

# 3c = Lakukan Uji Statistik (df =2)
paste("3c)", plotDist(dist='t', df=2, col="blue"))

# 3d = Nilai Kritikal
paste("3d)", qchisq(p = 0.05, df = 2, lower.tail=FALSE))

# soal 3e = Keputusan
paste("3e) Karena p-value < a , Hipotesis awal ditolak")

# soal 3f = Kesimpulan
paste("3f) Dengan tingkat keyakinan 95%, diyakini bahwa tidak terdapat perbedaan rata-rata saham pada perusahaan di Bandung dan Bali.")

# Soal 4
# 4a = Pembagian menjadi 3 subjek grup dan membuat
myFile  <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt"))
dim(myFile)
head(myFile)
attach(myFile)

myFile$V1 <- as.factor(myFile$V1)
myFile$V1 = factor(myFile$V1,labels = c("Kucing Oren","Kucing Hitam","Kucing Putih","Kucing Oren"))

class(myFile$V1)

group1 <- subset(myFile, V1=="Kucing Oren")
group2 <- subset(myFile, V1=="Kucing Hitam")
group3 <- subset(myFile, V1=="Kucing Putih")

# 4b = Carilah atau periksalah Homogeneity of variances nya
bartlett.test(Length~V1, data=dataoneway)

# 4c = Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus Grup
qqnorm(group1$Length)
qqline(group1$Length)

# 4d = Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
paste("4d) nilai p adalah 0.0013 dimana kurang dari 0.005, sehingga h0 ditolak")

# 4e = Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD
model1 <- lm(Length~Group, data=myFile)

anova(model1)

TukeyHSD(aov(model1))

# 4f = Visualisasikan data dengan ggplot2
library(ggplot2)

ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")

# Soal 5
# 5a = Buatlah plot sederhana untuk visualisasi data
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

GTL <- read_csv("GTL.csv")
head(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

# variabel untuk anova
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

# 5b = Lakukan uji ANOVA dua arah
anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

# 5c = Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

# 5d = Lakukan uji Tukey
print("Uji Tukey:")
tukey <- TukeyHSD(anova)
print(tukey)

# 5e = Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey
print("Compact Letter Display:")
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)
