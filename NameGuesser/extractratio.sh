#!/bin/bash
sed -e 's/^.* it is <b>//' -e 's/ times .*//' $1
