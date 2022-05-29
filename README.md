# P2_Probstat_D_5025201091

## Soal 1
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap
kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel
sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat
kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut
diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali
kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden
mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴

![image](https://user-images.githubusercontent.com/88977654/170876430-5bcbd62c-9989-4c8c-b9ee-e430290fd297.png)

Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari
responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah
melakukan aktivitas 𝐴 sebanyak 70.

### 1a = Standar Deviasi Selisih X dan Y
```r
arrayX <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
arrayY <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)

selisih <- arrayY - arrayX

paste("1a) Standar Deviasi Selisih X dan Y = ", sd(selisih))
```
### 1b = nilai T-score
```r
paste("1b) T-score Selisih = ")
t.test(selisih)
```
### 1c = tentukanlah apakah terdapat pengaruh signifikan sebelum dan sesudah melakukan aktivitas 𝐴
```r
var.test(arrayX, arrayY)
t.test(selisih, alternative = "two.sided", var.equal = TRUE)
```

## Soal 2
### 2a = Apakah Anda setuju dengan klaim tersebut?
Setuju, karena setelah diuji, ternyata tolak H0 (z>Z.alpha) sehingga rata-rata mobil dikemudikan per tahun lebih dari 20.000km
### 2b = Jelaskan maksud dari output yang dihasilkan!
seperti yang sudah tertulis diatas nilai 8,974359 merupakan nilai dari zhitung(z2 dalam syntax ini) melebihi nilai ztabel(z.alpha2) sehingga keputusan yang diambil adalah Tolak H0 sehingga disimpulkan bahwa rata-rata mobil dikemudikan pertahun lebih dari 20.000km
```r
install.packages("BSDA")
library(BSDA)
tsum.test(mean.x = 23500, sd(3900), n.x = 100)
```
### 2c = Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
```r
```

## Soal 3
### 3a
### 3b
### 3c
### 3d
### 3e
### 3f

## Soal 4
### 4a
### 4b
### 4c
### 4d
### 4e
### 4f

## Soal 5
### 5a
### 5b
### 5c
### 5d
### 5e
