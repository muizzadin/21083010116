#!/bin/sh
#Tugas5.sh
echo "---------------------------------------------------"
echo "|PROGRAM MENGHITUNG NILAI TUGAS SISTEM OPERASI|"
echo "---------------------------------------------------"
echo -n "Masukkan Nama Mahasiswa : "
read nama
echo -n "Masukkan NIM Mahasiswa : "
read nim
echo -n "Masukkan Nilai Tugas 1 : "
read tgs1
echo -n "Masukkan Nilai Tugas 2 : "
read tgs2
echo -n "Masukkan Nilai Tugas 3 : "
read tgs3
echo
echo "---------------------------------------------------"
echo "| DAFTAR NILAI TUGAS |"
echo "---------------------------------------------------"
echo "Nama : $nama"
echo "NIM : $nim"
echo "Nilai Tugas 1 : $tgs1"
echo "Nilai Tugas 2 : $tgs2"
echo "Nilai Tugas 3 : $tgs3"

total=`expr $tgs1 + $tgs2 + $tgs3`
ipk=`echo $total / 3 | bc`

echo "IPS Mahasiswa = $total / 3"
echo "IPK Mahasiswa = $ipk"
