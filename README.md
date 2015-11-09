# domestic_oil_production

README for Oil Production Analysis Project

This project contains R code for estimating a mathematical model to fit
the relationship between rig counts and subsequent crude oil production.

The model is based on the following assumptions:

1.  Each operating rig adds light tight oil production that will come on line some number of weeks in the future.
2.  The amount of production per rig has been increasing over time.
3.  Once an LTO oil well commences production, it experiences a constant rate of decay in its production level.
4.  The level of oil produced from conventional sources remains unchanged throughout the analysis period.
5.  At the beginning of the analysis period (1/1/2014), 50% of oil production came from conventional sources, and 50% came from light tight oil.

Thus, the model for production in week k is represented by the following equations:

       new LTO production = (# rigs in week (k minus production delay)) * 
          (initial LTO production * efficiency_growth^(k-1))
       total LTO production for week k = new LTO production + 
          well_decline_rate * (total LTO production for week k-1)
       total production = total LTO production + conventional production

The steps to use the code in this project are as follows:

1.  Install R on your computer.  You will also need to install the following R
packages: zoo, ggplot2, readxl

2.  Download the R source code for this project into a directory.  

3.  Create a subdirectory named "data"

4.  Download the excel spreadsheet of rig count data from Baker Hughes at:
	http://www.bakerhughes.com/rig-count
	http://phx.corporate-ir.net/phoenix.zhtml?c=79687&p=irol-reportsother
	
  This file is in xlsb format and cannot be read directly by R.
  Save it into the "data" subdirectory.

5.  Convert the Baker Hughes spreadsheet to an xlsx file.  

  Open the file in Excel.  Save it in xlsx format to the "data" directory with the name
   	Baker_Hughes_Rig_Count_YYYYMMDD.xlsx
  where YYYYMMDD represents today's date.

6.  Launch R.  Be sure that the working directory is the same as the
  directory where the oil production analysis script is located.  (You can use
  the setwd() command in R to change directory.)

7.  Load the oil production analysis script:
	source("oil_production.R")

8.  Load the BakerHughes data:
	bakerhughes <- read_baker_hughes()

9.  The oil production data is made available by EIA at:
	http://www.eia.gov/petroleum/supply/weekly/
	http://ir.eia.gov/wpsr/psw01.xls

  This can be downloaded onto your computer by executing the following R command:
    download_eia_production()

10.  Load the EIA oil production data into R:
          eia <- read_eia_production()

11.  Combine the Baker Hughes and EIA data into a single data.frame:
         bheia <- combine_baker_hughes_and_eia(bakerhughes, eia)

12.  Search for the best fitting models:
        bheia.models <- find_production_parameters_by_week(bheia.models)

  On my MacBook Pro laptop, this took about 20 minutes to run.

  For each possible value of the production delay in weeks, this function finds the
  model that best fits that particular production delay.  The result is a data.frame 
  with 36 rows, where each row represents the parameters that were found for
  a particular production delay.  You might want to inspect the resulting data.frame
  to get an idea of the values that are found.

13.  To plot the fit of the model for the production delay of 17 weeks versus
the historical data, enter the command:

  plot_forecast(bheia, bheia.models[17,])

  Replace the number 17 with a different number see the plot for
  a different production delay.

14.  To see the forecast through the end of 2016, enter the command:

  plot_forecast(bheia, bheia.models[17,], forecast_date=as.Date("2016-12-31"))

15.  To plot multiple models on a single graph, enter the command:

  multiplot_forecast(bheia, bheia.models[c(13,17,21,25),])

  (This plots the models for production delays of 13, 17, 21 and 25 weeks.)

16.  To see the date at which production reaches its minimum for a given model, enter the command:

  find_production_minimum(bheia, bheia.models[17,])

17.  To see the chart that compares "conventional wisdom" to actual production, enter:

  plot_adhoc_model(bheia)

18.  To see the chart of the best fitting model, enter:

  plot_best_model(bheia, bheia.models)

19.  To see the chart of the top best fitting models, enter:

  plot_top_models(bheia, bheia.models[c(13,17,21,25),])

Improvements to this code are welcomed!

Matthew Clegg
matthewcleggphd@gmail.com
November 8, 2015
