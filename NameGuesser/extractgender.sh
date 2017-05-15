#!/bin/bash

sed -e 's/!<\/.*//' -e "s/<b>It's a //" -e "s/\t//g" $1
