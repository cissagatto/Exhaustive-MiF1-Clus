##################################################################################################
# Exhaustive Partitions Micro F1                                                                 #
# Copyright (C) 2021                                                                             #
# JUNTA VALIDAÇÃO E TREINO                                                                       #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #  
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #     
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################

##################################################################################################
# Script 
##################################################################################################

##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/Exhaustive-MicroF1-TVT", sep="")
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/Exhaustive-MicroF1-TVT", sep="")
}
setwd(FolderRoot)
FolderScripts = paste(FolderRoot, "/scripts", sep="")


##################################################################################################
# LOAD EXTERNAL LIBRARIES                                                                        #
##################################################################################################

#library("googledrive") 
library("rJava", quietly = TRUE) 
library("RWeka", quietly = TRUE) 
library("readr", quietly = TRUE) 
library("foreign", quietly = TRUE) 
library("stringr", quietly = TRUE) 
library("mldr", quietly = TRUE) 
library("utiml", quietly = TRUE)
library("plyr", quietly = TRUE) 
library("dplyr", quietly = TRUE) 
library("AggregateR", quietly = TRUE) 
library("parallel", quietly = TRUE) 
library("foreach", quietly = TRUE) 
library("doParallel", quietly = TRUE) 

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
