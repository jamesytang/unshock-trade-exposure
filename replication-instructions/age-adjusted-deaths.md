Instructions for creating age-adjusted death rates 
Individual level cause of death data available from NCHS via application

1. Follow data application instructions from https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm#anchor_1553801979 to receive restricted-use death files
  - Set up virtual machine or other secured device to receive data 
2. Create folders called "MCD_files" and "interim" 
3. Save MCD .txt files to "MCD_files" folder
4. Rename raw multiple cause of death files to "MULTyearUS.AllCnty.txt"
  - ex. For the year 2000, the file name is "MULT2000US.AllCnty.txt" 
  - Different years have different naming conventions
5. Run r file "data_format_vm.R" 
  - Reformatted MCD files are saved to "interim" folder as "mort_year.csv" (ex. for 2000, the file will appear as "mort_2000.csv" 
7. I recommend zipping these mort_year files to save space 
8. Run r file "data_aggregate.R"
  
 
