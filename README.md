# R_for_Gorilla_Analysis
R scripts for analysing data collected using Gorilla online experiment builder (https://gorilla.sc/)

- An open repository of quesionnaires and tasks on Gorilla is linked here: https://gorilla.sc/openmaterials/50646

Gorilla Data (https://gorilla.sc/support/reference/faq/metrics#accessingyourdata) 
usually requires some level of data filtering/cleaning & analysis (https://gorilla.sc/support/walkthrough/RStudio),
with R & RStudio best placed to do this analysis. 

Please also see Gorilla to tidy data tutorial here: https://emljames.github.io/GorillaR/index.html

This repository contains R scripts to tidy and perform initial basic analysis of csv files collected. 

 - **ConcussionStudy_2020_DataTidy_Gorilla.R**
 
      - _This script is commented in detail to explain its use, input & outputs._ 
      - _But in brief, it combines relevant data from 8 questionnaires and 2 tasks (but 4 files, as task order was counterbalanced) into 6 output files (key participants data (for completed an started); demographics only; analysis only; Full combined data; full combined data with quantised questionnaire data)._ 
      - _It performs analysis to extract reaction times and percent correct for relevant variables in the tasks._ 
      - _It also combines two different versions of data (where the experiment version has been updated from v19 to v20 in the middle of collection creating csv files for v19 and some for v20)._ 
      - _As such, it is a good example script for various aspects of data tidying and initial analysis required for Gorilla data._ 

- **Brief_IAT_Analyse.R**
 
     - _This script is commented in detail to explain its use, input & outputs._ 
     - _But in brief, it analyses the output from the Brief-IAT task on Gorilla according to Nosek BA et al. (2014) Understanding and Using the Brief Implicit Association Test: Recommended Scoring Procedures. PLoS ONE 9(12): e110938. https://doi.org/10.1371/journal.pone.0110938._
     - _It outputs block order, total trials, total trials trimmed (for RT extremes), whether to include the data based on QA (no. trials & fast RT), D, errors (general & RT too slow), means & STD for M1 and M2 in formula for D, Mean ^ STD across blocks, and for all blocks individually, D for blocks 1 & 2 (D1), D for blocks 3 & 4 (D2), and STD for blocks 1 & 2, and blocks 3 & 4._  
