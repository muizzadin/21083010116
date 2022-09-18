#!/bin/bash

printf "Jajan apa yang kamu suka ?\n"
printf "pentol ?\n"
printf "batagor ?\n"
printf "cireng ?\n"

read jajan

case "$jajan" in
  "pentol")
    echo "pentol buk mah wenak slur!"
    ;;
  "batagor")
    echo "batagore mas budi mantap bat"
    ;;
  "cireng")
    echo "cirenge kantin rasane unch-unch"
    ;;
  *) 
    echo "makanan yang kamu suka gaenak hehe"
    ;;
esac

