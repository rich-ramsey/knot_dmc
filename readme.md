This project creates a reproducible manuscript for the knots dynamic models of choice (dmc) project using the R package papaja().

This project is supplementary to the main analyses performed by Sam.
As such, the main analysis, data, models and code are stored on the open science framework: [https://osf.io/ea6gk/](https://osf.io/ea6gk/)

The purpose of this project is just to produce the manuscript in papaja() and create any figures that are additional to those that are already compiled and available on the OSF.

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

As stated above, the main analysis, data, models and code are stored on the [open science framework](https://osf.io/ea6gk/).

This project serves two complementary aims, which are as follows:

1. Produce the manuscript using papaja() package in R.

2. Produce a summary plot the of main findings, which illustrates main findings using a decision model plotting format.

# Organisation of files and folders #

## files ##

At the top level of the folder, there are several files.

- There is one R project file:

**knot_dmc.Rproj**. 

- There is one R markdown file:

**effects.Rmd**. This file reads in the main DMC model, wrangles parameter estimates and creates some plots.

- There is one renv.lock file

**renv.lock**. renv() produces a plain text file that records all package versions.

## folders ##

There are also folders, with self-explanatory titles: 

**/figures/**

This is where the figures are stored.

**/models/**

The main model object, which is available upon request from the authors.
Please email Samantha Parker (samantha.parker@students.mq.edu.au) or Richard Ramsey (richard.ramsey@hest.ethz.ch)

**/manuscript/**

This is where the manuscript .Rmd file and .pdf files are stored, along with the .bib files that contain references. 
For more information on using papaja() for manuscripts, see the [papaja manual](https://frederikaust.com/papaja_man/)

**/packages/**

Information on installing R packages is stored in this folder.