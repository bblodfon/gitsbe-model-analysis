#!/bin/bash                                                                     
                                                                                
files=`ls | grep gitsbe`                                                        
                                                                                
ss0=0                                                                           
ss1=0                                                                           
ss2=0                                                                           
for file in ${files}; do                                                        
  ss_num=`cat $file | grep stablestate | wc -l`                                 
  if [ ${ss_num} == 0 ]                                                         
  then                                                                          
          ss0=$((ss0+1))                                                        
  elif [ ${ss_num} == 1 ]                                                       
  then                                                                          
          ss1=$((ss1+1))                                                        
  elif [ ${ss_num} == 2 ]                                                       
  then                                                                          
          ss2=$((ss2+1))                                                        
  fi                                                                            
done                                                                            
                                                                                
echo "0 ss: $ss0"                                                               
echo "1 ss: $ss1"                                                               
echo "2 ss: $ss2"

