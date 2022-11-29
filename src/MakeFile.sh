#!/bin/sh
rm restaurant.ali restaurant.o restaurant master.ali master.o
gnatmake restaurant.adb
./restaurant