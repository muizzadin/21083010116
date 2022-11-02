#!/bin/bash

echo ">>Menghitung luas persegi panjang<<";
echo

L_persegipanjang() {
 panjang=$p
 lebar=$l
 echo "$panjang"
 echo "$lebar"
}
echo "Masukkan panjang :"
read panjang;
echo "Masukkan lebar :"
read lebar;
let hasil=$panjang*$lebar
echo "Jadi, luas persegi panjang adalah $hasil cm"

printf "\n"
L_persegipanjang $panjang $lebar
