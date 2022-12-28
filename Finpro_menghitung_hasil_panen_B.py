#!/usr/bin/env python
# coding: utf-8

# In[2]:


print ("|                 HALLO TEMAN TEMAN KU                  |")
print ("+=======================================================+")
print ("|     SELAMAT DATANG DI MESIN HITUNG HASIL PANEN        |")
print ("|_______________________________________________________|")
print ("|     Menghitung Hasil Panen dengan sangat Akurat       |")
print ("+=======================================================+")
# Program untuk menghitung hasil panen
while True:
  # Meminta pengguna untuk memasukkan informasi lahan pertanian
  luas_lahan = float(input("Masukkan luas lahan (dalam hektar): "))
  if luas_lahan == 2:
    break
  jenis_tanaman_buah = input("Masukkan jenis tanaman buah: ")
  hasil_per_hektar = float(input("Masukkan estimasi hasil per hektar: "))

  # Menghitung hasil panen
  hasil_panen = luas_lahan * hasil_per_hektar

  # Menampilkan hasil panen
  print("Hasil panen dari lahan seluas", str(int(luas_lahan)), "hektar dengan tanaman", jenis_tanaman_buah, "adalah sebesar", str(int(hasil_panen)))

  # Meminta pengguna untuk memasukkan apakah ingin mengulangi program
  ulangi = input("Apakah Anda ingin mengulangi program? (y/n) ")
  if ulangi == "n":
    print("terimakasih telah menggunakan program ini")
    break


# In[ ]:




