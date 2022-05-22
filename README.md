# unshock-trade-exposure
Code, data, and instructions to reproduce undergraduate honor's thesis

Author: James Tang  
Advisor: Brian Beach  
Thesis Title: "Unshocking a shock - using stock market reactions to measure labor market outcomes from variations in trade exposure" 

Abstract: This paper examines the effect of protectionist trade policy, namely the 2018 trade war, and its ability to “unshock” deteriorating labor market outcomes following the establishment of permanent normal trade relations (PNTR) with China in 2000. Using stock market returns to measure county-level trade exposure, I find that, while negatively exposed counties see a sharp rise in unemployment during PNTR, the trade war failed to produce any significant short-term results aimed at reversing these adverse employment trends. These results demonstrate the asymmetry of trade policy outcomes and suggest the potential permanence of trade-related labor displacement.


Table of contents
- Data application materials
- Public use datasets
- Self-created datasets
- Code
- Thesis draft


Replication Instructions 
- Note: These instructions are not exact and will require some debugging/understanding of R filepaths, etc. 
1. Run "external_datasets.R" 
2. Unzip any zipped files and download linked files into specified folders


1. Unzip file "CBP_1990adj_dorn.dta.zip" in folder "Pierce_schott" 
2. Unzip file "efsy_cbp_2013.csv.zip" in folder "county_business_patterns"
3. Run R file "AAR_pntr.R"
4. Run R file "AAR_tradewar.R" 
5. Completed industry and county level AAR score datasets should be found in "final_datasets" as "AAR_pntr_county.csv", "AAR_pntr_industry.csv", "AAR_tradewar_county.csv", "AAR_tradewar_industry.csv"


Instructions for creating age-adjusted death rates 
Individual level cause of death data available from NCHS via application

1. Follow data application instructions from https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm#anchor_1553801979 to receive restricted-use death files
  - Set up virtual machine or other secured device to receive data 
2. Create folders called "MCD_files", "interim", and "final_datasets" 
3. Save MCD .txt files to "MCD_files" folder
4. Rename raw multiple cause of death files to "MULTyearUS.AllCnty.txt"
  - ex. For the year 2000, the file name is "MULT2000US.AllCnty.txt" 
  - Different years have different naming conventions
5. Run r file "data_upload_format.R" 
  - Reformatted MCD files are saved to "interim" folder as "mort_year.csv" (ex. for 2000, the file will appear as "mort_2000.csv" 
7. I recommend zipping these mort_year files to save space 
8. Run r file "data_aggregation.R"
9. Completed county-level age-adjusted death rates for the trade war and PNTR are separately found in the "final_datasets" folder
