#langkah pertama adalah memasukkan library yang akan dipakai
library(ggplot2)

#lalu kita mengimport dataset, kali ini saya meminjam dataset dari DQLab Academy
pelanggan <- read.csv("https://academy.dqlab.id/dataset/customer_segments.txt", sep="\t")

#lihat sedikit terhadap data kita
summary(pelanggan)

#terlihat beberapa data yang nantinya akan sering kita pakai, masukkan kedalam variable 
field_yang_digunakan <- c("Jenis.Kelamin","Tipe.Residen","Profesi")
pelanggan[field_yang_digunakan]

#data tersebut adalah data bertipe string yang tidak akan bisa dibaca oleh algoritma, ubah menjadi integer
pelanggan_matrix <- data.matrix(pelanggan[field_yang_digunakan])

#gabungkan data matrix terhadap data asli
pelanggan <- data.frame(pelanggan, pelanggan_matrix)
pelanggan

#cari tahu berapa saja nilai yang sudah di konvert ke matrix, lalu masukkan kedalam variable
Profesi <- unique(pelanggan[c("Profesi","Profesi.1")])
Tipe.Residen <- unique(pelanggan[c("Tipe.Residen","Tipe.Residen.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin","Jenis.Kelamin.1")])

#Terlihat angka nilai belanja terlalu besar sehingga memberatkan kinerja, mari kita normalisasikan
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun/1000000

#model sudah siap untuk kita operasikan pada algoritma kmeans
#setel set.seed untuk penyeragaman daftar nilai acak, ini biasa dilakukan pada algoritma kmeans
set.seed(100)

#buat variable untuk field yang akan kita gunakan
field_yang_digunakan <- c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1","NilaiBelanjaSetahun")

#sebelum kita mengaplikasikan model, kita butuh mencari parameter terbaik
sse <- sapply(1:10, function(param_k){kmeans(pelanggan[field_yang_digunakan], param_k, nstart=25)$tot.withinss})

jumlah_cluster_max <- 10
ssdata <- data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) + 
  geom_line(color="red") + geom_point() + 
  ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") +
  geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) +
  scale_x_discrete(limits=c(1:jumlah_cluster_max))

#terlihat pada plot, titik elbow adalah 3, jadikan sebagai paremeter center, lalu masukkan ke dalam model dan simpan ke dalam variable
segmentasi <- kmeans(x=pelanggan[field_yang_digunakan], centers=3, nstart=25)
segmentasi

#masukkan hasil cluster ke dalam dataframe
pelanggan$cluster <- segmentasi$cluster

#kita bisa menganalisa data segmentasi dengan memfilter data mana saja yang masuk ke dalam cluster ke-1,2, dst
pelanggan[which(pelanggan$cluster == 1),]
length(which(pelanggan$cluster == 1))

#kita namakan tingkatan cluster kita
Segmen.Pelanggan <- data.frame(cluster=c(1,2,3), Nama.Segmen=c("Gen.Z Woman","Gen.Milenial Professional","Gen.X Professional"))

#setelah selesai, kita gabungan semua referensi ke dalam bentuk list, ini berfungsi jika kita ingin menyimpan file ke dalam bentuk RDS
Identitas.Cluster <- list(Profesi=Profesi, Jenis.Kelamin=Jenis.Kelamin, Tipe.Residen=Tipe.Residen, 
                          Segmentasi=segmentasi, Segmen.Pelanggan=Segmen.Pelanggan, field_yang_digunakan=field_yang_digunakan)
saveRDS(Identitas.Cluster,"cluster.rds")
Identitas.Cluster <- readRDS(file="cluster.rds")

#model telah selesai belajar mari kita test dengan data baru
databaru <- data.frame(Customer_ID="CUST-100", Nama.Pelanggan="Raisya Wilamar", Umur=20 ,Jenis.Kelamin="Wanita",Profesi="Pelajar",Tipe.Residen="Cluster",NilaiBelanjaSetahun=3.5)

#Masukkan perintah untuk penggabungan data
databaru <- merge(databaru, Identitas.Cluster$Profesi)
databaru <- merge(databaru, Identitas.Cluster$Jenis.Kelamin)
databaru <- merge(databaru, Identitas.Cluster$Tipe.Residen)
databaru

#menentukan data baru di cluster mana
Identitas.Cluster$Segmen.Pelanggan[which.min(sapply(1:3, function(x) sum((databaru[Identitas.Cluster$field_yang_digunakan] - Identitas.Cluster$Segmentasi$centers[x,])^2))),]

