#!/bin/bash

for a in {15..1}
do
   if [ $((a%2)) -eq 1 ];
   then
      echo $a
fi
done
