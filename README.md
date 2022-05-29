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
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun.
Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk
mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata
23.500 kilometer dan standar deviasi 3900 kilometer. (Kerjakan menggunakan library seperti referensi pada modul).
### 2a = Apakah Anda setuju dengan klaim tersebut?
Setuju, karena setelah diuji, ternyata tolak H0 (z>Z.alpha) sehingga rata-rata mobil dikemudikan per tahun lebih dari 20.000km.
### 2b = Jelaskan maksud dari output yang dihasilkan!
seperti yang sudah tertulis diatas nilai 8,974359 merupakan nilai dari zhitung(z2 dalam syntax ini) melebihi nilai ztabel(z.alpha2) sehingga keputusan yang diambil adalah Tolak H0 sehingga disimpulkan bahwa rata-rata mobil dikemudikan pertahun lebih dari 20.000km.
### 2c = Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
Dikarenakan nilai pvalue < alpha(0,05) maka keputusan yang diambil adalah Tolak H0 sehingga disimpulkan bahwa rata-rata mobil dikemudikan pertahun lebih dari 20.000km.

## Soal 3
Diketahui perusahaan memiliki seorang data analyst ingin memecahkan
permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya
didapatkanlah data berikut dari perusahaan saham tersebut.
![image](https://user-images.githubusercontent.com/88977654/170879885-78993852-9619-4a34-b223-dee3c9b9cef3.png)
Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil
diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada
rata-ratanya (α= 0.05)? Buatlah :
### 3a = H0 dan H1
Bikin fungsi untuk menghitung z
```r
z <- function(xSampel, xPopulasi, s, n) {
  return ((xSampel - xPopulasi) / (s / sqrt(n)))
}
```
Masukkan isi variabel untuk menghitung H0 dan H1
```r
H0 <- z(3.64, 0, 1.67, 19)
H1 <- z(2.79, 0, 1.32, 27)
```
Output
![image](https://user-images.githubusercontent.com/88977654/170879304-23a4f9be-d0d9-4e1b-8d1a-a1a315083501.png)

### 3b = Hitung Sampel Statistik
```r
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y =2.79 , s.y = 1.32, n.y = 27, alternative = "greater", var.equal = TRUE)
```
Output
![image](https://user-images.githubusercontent.com/88977654/170879696-e2862c37-35aa-4caa-bd15-611c50977704.png)

### 3c = Lakukan Uji Statistik (df =2)
```r
plotDist(dist='t', df=2, col="blue"
```
Output
![image](https://user-images.githubusercontent.com/88977654/170879766-09585c3a-e582-4058-b455-fd9c0810ef39.png)

### 3d = Nilai Kritikal
```r
qchisq(p = 0.05, df = 2, lower.tail=FALSE)
```
Output
![image](https://user-images.githubusercontent.com/88977654/170879808-b7d29375-7203-4977-a4e2-21aacca2f754.png)

### 3e = Keputusan
Karena p-value < a , Hipotesis awal ditolak

### 3f = Kesimpulan
Dengan tingkat keyakinan 95%, diyakini bahwa tidak terdapat perbedaan rata-rata saham pada perusahaan di Bandung dan Bali.

## Soal 4
Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya
ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
kucing putih dengan panjangnya masing-masing.
Jika :
diketahui dataset https://intip.in/datasetprobstat1
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya
sama
Maka Kerjakan atau Carilah:
### 4a = Buatlah masing masing jenis spesies menjadi 3 subjek "Grup"
### 4b = Carilah atau periksalah Homogeneity of variances nya
### 4c = Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus Grup
### 4d = Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
### 4e = Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD
### 4f = Visualisasikan data dengan ggplot2

## Soal 5
Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk
mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca
pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan
dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil
Eksperimen. Dengan data tersebut:
### 5a = Buatlah plot sederhana untuk visualisasi data
### 5b = Lakukan uji ANOVA dua arah
### 5c = Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan
### 5d = Lakukan uji Tukey
### 5e = Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey
