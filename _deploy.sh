#!/bin/sh
cp -r figures/ _book/
scp -r _book/* root@zhulab.ucsc.edu:~/wordpress/projects/grassland/