#!/bin/bash

for i in $(seq 1 5); do

    input="TK_EPX_Setpoint_ind_${i}.in"
	
    echo "Lancement de $input..."
	
    ./mcsim.m_TK_EPX "$input" 
	
done