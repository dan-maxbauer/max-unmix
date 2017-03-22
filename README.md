# max-unmix

The documents and data within this git-hub folder form the basis for the MAX UnMix application that can be accessed online at www.irm.umn.edu/maxunmix. The code for the application can be found in the documents “server.R” and “ui.R”. 

Anyone using this code should acknowledge and cite:

Maxbauer, D.P., Feinberg, J.M. and Fox, D.L., 2016. MAX UnMix: A web application for unmixing magnetic coercivity distributions. Computers & Geosciences http://dx.doi.org/10.1016/j.cageo.2016.07.009

Instructional videos are available online in the ‘Resources’ tab of the MAX UnMix webpage.

If you will be processing a large amount of data using MAX UnMix - it is adventageous to run the program locally (offline). To run MAX UnMix locally on your own computer follow the steps below.

##########
1. Download BOTH R and R-studio to your computer. Instructions for downloads can be found at:
	R = https://www.r-project.org/
	R studio = https://www.rstudio.com/products/rstudio/download/

2. Save the MAX UnMix folder to your computer 

3. Open R-studio

4. In “Console” enter the following text and hit return: 
	
	install.packages("shiny") # load the shiny package into R
	install.packages("shinyBS")
	install.packages("fGarch")
	install.packages("MESS")
	library("shiny") # this need to be entered each time R-studio is re-launched to run program (contains runApp() function below)
	setwd("enter file path to max-unmix folder") #tells R where to find app
	runApp("max-unmix") #will launch app in browser or local R-studio window

############
If you encounter problems in operating MAX UnMix locally or online, please send all questions, comments, and concerns to Dan Maxbauer (maxba001@umn.edu).
