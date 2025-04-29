This project creates a reproducible manuscript for the knots dynamic models of choice (dmc) project using the R package papaja().

This project incorporates the main evidence accumulation analyses performed by Sam.

We also provide code to run the manifest analysis of reaction time and accuracy separately.

Samantha Parker, Emily S. Cross, & Richard Ramsey. Evidence accumulation modelling offers new insights into the cognitive mechanisms that underlie linguistic and action-based training.

Preprint: 

# Basic components of the workflow #

- [renv()](https://rstudio.github.io/renv/articles/renv.html) to manage R package versions
- [git](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control) for version control
- [GitHub](https://github.com/) for collaborating and sharing coding
- The [Tidyverse](https://www.tidyverse.org/) ecosystem for data wrangling and visualisation 
- The [papaja()](https://frederikaust.com/papaja_man/) package for writing reproducible manuscripts

# What's the easiest way to access the project? #

If you want to see and work with the code, then:

1. Clone or download the project from github to your local machine.
2. Open the knot_dmc.Rproj file and renv() will automatically bootstrap itself.
3. renv() will then ask if you want use renv::restore() to install all of the packages. Say yes.
4. At this point, you can use the project with the same package versions that were stored in the renv.lock file.
5. You must also download and place in the project folder the /dmc/ folder and /tutorial/ folder after downloading the dmc zip file from here: [https://osf.io/pbwx8/](https://osf.io/pbwx8/)

# Aims #

This project serves two complementary aims, which are as follows:

1. Make the entire analysis workflow available via R scripts.

2. Produce the manuscript in a computationally reproducible from using the papaja() package in R.

# A note on code and raw data availability #

All of the analysis code used in this paper are available with this repository.

However, given that the data were collected several years before 2012, it was not 
yet routine to request explicit consent to share data publicly.
Therefore, the raw data and model objects (which include the raw data) are only available from the authors upon request.
Please email Samantha Parker (samantha.parker [[at]] students.mq.edu.au) or 
Richard Ramsey (richard.ramsey [[at]] hest.ethz.ch) and we will share the data and model objects with 
you, as long as it is for research purposes.

# A note on using the R Markdown files listed below #

Some files, such as wrangle and effects files, can be executed chunk-by-chunk or in one entire script.

In contrast, the models files are not written to be executed in one go. 
This is because some of the modelling steps take a long time, so we suggest running them chunk-by-chunk.

# Organisation of files and folders #

## files ##

At the top level of the folder, there are several files.

- There is one R project file:

**knot_dmc.Rproj**. 

- There are several R markdown files:

**effects_lba.Rmd**. This file reads in the main DMC model, wrangles parameter estimates and creates some plots.

**effects_lba_summary.Rmd**. This file reads in the main DMC model, wrangles parameter estimates and creates some summary plots.

**effects_rt_acc.Rmd**. This file reads in the main brms model, wrangles parameter estimates and creates some plots.

**model_lba.Rmd**. This file builds the main evidence accumulartion model (LBA model) using the DMC software.

**model_rt_acc.Rmd**. This file builds Bayesian regression models of rt and accuracy seperately using the brms package. 

**wrangle.Rmd**. This file wrangles the data, makes summary plots and writes out data for subsequent analyses.

**sim_data.Rmd**. This file takes fitted regression models from the manifest analysis and uses them to simulate RT and accuracy data that has similar properties to the real data.

- There is one renv.lock file

**renv.lock**. renv() produces a plain text file that records all package versions.

## folders ##

There are also folders, with self-explanatory titles: 

**/figures/**

This is where the figures are stored.

**/tables/**

This is where the tables are stored.

**/manuscript/**

This is where the manuscript .Rmd file and .pdf files are stored, along with the .bib files that contain references. 
For more information on using papaja() for manuscripts, see the [papaja manual](https://frederikaust.com/papaja_man/)

The /supplementary/ subfolder contains the corresponding .Rmd and .pdf files for supplementary materials.

**/packages/**

Information on installing R packages is stored in this folder.

## subfolders ##

In various folders, there are subfolders, which denote:

**/ea/**. evidence accumulation

**/manifest/**. manifest analyses of rt and accuracy separately

**/descriptive/**. descriptive plots of rt and accuracy.

**/simulated/**. simulated data and plots.


# How do I use the files? #

## DMC software ##

To use the DMC software, please see the [DMC website](https://osf.io/pbwx8/) and download the current release zip file. 

Then place the /dmc/ and /tutorial/ folders at the top level of your analysis folder.

## Add some additional folders ##

First, create a folder called **/data/** in the top level directory.

Within the data folder, create a subfolder: 

**/processed/**. This is where you would place the processed data if requested from the authors.

Second, create a folder called **/models/** in the top level directory.

Within the models folder, create two subfolders: 

**/ea/**. This is where you would place the evidence accumulation models if requested from the authors or this is where the model_lba.Rmd script would write to.

**/manifest/**. This is where you would place the brms regression models if requested from the authors or this is where the model_rt_acc.Rmd script would write to.

If you want reproduce the entire workflow, then start with the wrangle file, then the models file and then the effects files.
