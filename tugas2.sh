#!/bin/bash

echo "masukkan jumlah belanja anda:"
read n

if [ $n -le 100000 ];
then
echo "maaf kamu kurang beruntung"
else
echo "selamat kamu dapat undian berhadiah"
fi
