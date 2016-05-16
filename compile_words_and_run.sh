#!/bin/bash
awk '{ \ 
DANE=substr($0,2,length($0)-2); \ 
k=split(DANE,T,","); \
j=0; \
for(i=1;i<=k;i++ ) \
   if((T[i]*1<=0) && (T[i]!=0) ) \ 
    { \
     printf("%s ",toupper(T[i])); \ 
     j++; \ 
     if(j==4) {printf("\n");j=0;} \ 
    } \ 
    else{j=0; printf("%04X\n",T[i])}; \ 
}' < $1 > .compile_tmp | ./Sextium_III/assemble .compile_tmp
./Sextium_III/sextium .compile_tmp.sextium