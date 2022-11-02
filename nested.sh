#!/bin/bash

#mendeklarasikan fungsi
nama() {
    echo "Siapa namamu?"
    read nama
    npm                              # <-------- memanggil fungsi di dalam fungsi (fungsi bersarang)

}
npm() {
    echo "Sebutkan npm mu"
    read npm
    echo -e "Hai $nama dengan npm $npm, selamat datang \n di praktikum sistem operasi yang seru ini ya!"
}
#memanggil fungsi
nama
