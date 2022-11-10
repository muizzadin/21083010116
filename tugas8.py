from os import getpid
from time import time, sleep
from multiprocessing import cpu_count, Pool, Process

angka = int(input("Masukan nilai batasan = "))


def cetak(i):
    if (i+1)%2== 0:
       print(i+1, "Genap",  "-punya ID proses", getpid())
       sleep(1)
    else:
       print(i+1, "Ganjil", "-punya ID proses", getpid())
       sleep(1)

print("Sekunsial")
# UNTUK MENDAPATKAN WAKTU SEBELUM EKSEKUSI
sekuensial_awal = time()

# PROSES BERLANGSUNG
for i in range(angka):
    cetak(i)

# UNTUK MENDAPATKAN WAKTU SETELAH EKSEKUSI
sekuensial_akhir = time()

print("multiprocessing.process")

#MULTIPROCESSING DENGAN KELAS PROCESS
process_awal=time()
for i in range(angka):
    p=Process(target=cetak, args=(i, ))
    p.start() 
    p.join()
process_akhir=time()


print("multiprocessing.pool")

# UNTUK MENDAPATKAN WAKTU SEBELUM EKSEKUSI
pool_awal = time()

# PROSES BERLANGSUNG
pool = Pool()
pool.map(cetak, range(angka))
pool.close()

# UNTUK MENDAPATKAN WAKTU SEBELUM EKSEKUSI
pool_akhir = time()



print("Sekuensial :", sekuensial_akhir - sekuensial_awal, "detik")
print("Kelas Process :", process_akhir - process_awal, "detik")
print("Kelas Pool :", pool_akhir - pool_awal, "detik")
