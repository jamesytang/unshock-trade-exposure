Code, data, and instructions to reproduce undergraduate honor's thesis

# "Unshocking a shock - using stock market reactions to measure labor market outcomes from variations in trade exposure" 
Author: James Tang  
Advisor: Brian Beach  

# Abstract
This paper examines the effect of protectionist trade policy, namely the 2018 trade war, and its ability to “unshock” deteriorating labor market outcomes following the establishment of permanent normal trade relations (PNTR) with China in 2000. Using stock market returns to measure county-level trade exposure, I find that, while negatively exposed counties see a sharp rise in unemployment during PNTR, the trade war failed to produce any significant short-term results aimed at reversing these adverse employment trends. These results demonstrate the asymmetry of trade policy outcomes and suggest the potential permanence of trade-related labor displacement.


# Table of contents
- Data application materials
- Public use datasets
- Self-created datasets
- Code
- Thesis draft


# Replication Instructions 
Note: These instructions are not exact and will require some debugging/understanding of R filepaths, etc. 
1. Create/download folders "interim", "final_datasets", "xwalks", "downloaded_data", "MCD_files" with appropriate sub-folders
2. Unzip any zipped files and download linked files into specified folders
3. Edit filepaths to your personal device 
4. Run R file "external_datasets.R" 
5. Run R file "AAR_pntr.R"
6. Run R file "AAR_tradewar.R" 
7. Run R file "AAR_pntr_prep.R" 
8. Run R file "AAR_pntr_viz.R"
9. Run R file "AAR_tradewar_viz.R" 
10. Run R file "reg_pntr_labormkt.R"
11. Run R file "ushock_labormkt.R"
12. Follow data application instructions from https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm#anchor_1553801979 to receive restricted-use death files
13. Save restricted use MCD .txt files to "MCD_files" folder
14. Rename raw MCD files to "MULTyearUS.AllCnty.txt"
  - ex. For the year 2000, the file name is "MULT2000US.AllCnty.txt" 
  - Different years have different naming conventions
15. Run R file "death_data_upload_format.R" 
  - Reformatted MCD files are saved to "interim" folder as "mort_year.csv" (ex. for 2000, the file will appear as "mort_2000.csv")
  - I recommend zipping these mort_year files to save space 
16. Run R file "death_data_aggregation.R"
17. Run R file "reg_pntr_deaths.R"



