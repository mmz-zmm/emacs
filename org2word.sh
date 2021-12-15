#!/usr/bin/env bash

date=`date -u`
pandoc_bin=pandoc
src=$1
prefix=`echo $src | awk -F/ '{print $NF}' | cut -d. -f1`
dft_dst=${prefix}.docx

org_exp_fmt=./org_exp_fmt.org
sed -i 's/#+TITLE:.*/#+TITLE:    '$prefix'/g' $org_exp_fmt 
sed -i 's/#+DATE:.*/#+DATE:    '"$date"'/g' $org_exp_fmt 

dst=${2:-$dft_dst}
#template=./Org-Template.docx
template=./org-reference.docx
$pandoc_bin  -f org <(cat $org_exp_fmt) <(cat $src) -t docx  -o $dst --reference-doc=$template
