# GettingCleaningData
## Overview
The R script **run_analysis.R** generates a tiny data set from the input meta-data file *activity_labels.txt* and *features.txt*, and data in the *test* and *train* directories. It creates for each activity and each subject the average for each variable. The result is written to the file *tiny_data.txt*.

## How to run?
You can run it from R studio or from command line. 

### Running from R studio
If you run from R studio, make sure you setwd() to the directory that contains the meta-data files and the train/data directories. From R studio, open the script *run_analysis.R* and click on "source"  


### Running from command line
Change to the directory where the *run_analysis.R* script along with the meta-data files and train/data directories. From the command line, invoke  **Rscript run_analysis.R**


## Description of output
For an explanation of the columns in the output (tiny_data.txt), the file CodeBook.md. The units are shown in parenthesis.