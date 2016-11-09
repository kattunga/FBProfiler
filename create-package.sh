#!bin/sh
lazbuild fbprofiler.lpr

product_name=fbprofiler
version=1.0.4

rm $product_name*.zip
cat create-package.lst | zip -9 -q -@ $product_name-$version-linux-gtk2-amd64.zip
