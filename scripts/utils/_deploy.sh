#!/bin/sh

set -ev

ssh root@zhulab.ucsc.edu "rm -rf ~/wordpress/projects/grassland/*"

cp -r figures/ _book/
scp -r _book/* root@zhulab.ucsc.edu:~/wordpress/projects/grassland/
