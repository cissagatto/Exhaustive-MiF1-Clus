cat("\n\n################################################################################################")
cat("\n# START EXECUTE EXHAUSTIVE MICRO F1                                                              #")
cat("\n##################################################################################################\n\n") 

rm(list=ls())

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
# LOAD MAIN.R                                                                                     #
##################################################################################################
setwd(FolderScripts)
source("libraries.R") 

setwd(FolderScripts)
source("utils.R") 

setwd(FolderScripts)
source("validation.R")

setwd(FolderScripts)
source("test.R")


##################################################################################################
# Options Configuration                                                                          #
##################################################################################################
options(java.parameters = "-Xmx32g")
options(show.error.messages = TRUE)
options(scipen=30)



##################################################################################################
# Read the dataset file with the information for each dataset                                    #
##################################################################################################
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets.csv"))



##################################################################################################
# ARGS COMMAND LINE                                                                              #
##################################################################################################
cat("\nGet Args")
args <- commandArgs(TRUE)



##################################################################################################
# Get dataset information                                                                        #
##################################################################################################
ds <- datasets[args[1],]


##################################################################################################
# Get dataset information                                                                        #
##################################################################################################
number_dataset <- as.numeric(args[1])
cat("\nBPC \t number_dataset: ", number_dataset)



##################################################################################################
# Get the number of cores                                                                        #
##################################################################################################
number_cores <- as.numeric(args[2])
cat("\nBPC \t cores: ", number_cores)



##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
number_folds <- as.numeric(args[3])
cat("\nBPC \t folds: ", number_folds)



##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
folderResults <- toString(args[4])
cat("\nBPC \t  folder: ", folderResults)



##################################################################################################
# Get dataset name                                                                               #
##################################################################################################
dataset_name <- toString(ds$Name) 
cat("\nBPC \t nome: ", dataset_name)



##################################################################################################
# DON'T RUN -- it's only for test the code
# ds <- datasets[29,]
# dataset_name = ds$Name
# number_dataset = ds$Id
# number_cores = 10
# number_folds = 10
# folderResults = "/dev/shm/res"
##################################################################################################



##################################################################################################
# cat("\n\nCopy FROM google drive \n")
# destino = paste(FolderRoot, "/datasets/", dataset_name, sep="")
# origem = paste("cloud:elaine/Datasets/CrossValidation_WithValidation/", dataset_name, sep="")
# comando = paste("rclone -v copy ", origem, " ", destino, sep="")
# print(system(comando))


##################################################################################################
diretorios = directories(dataset_name, folderResults)


##################################################################################################
cat("\nCreate Folder")
if(dir.exists(folderResults)==FALSE){
  dir.create(folderResults)
}


##################################################################################################
# cat("\n Copy partitions from google drive")
# destino = diretorios$folderBellPartDataset
# origem = paste("cloud:elaine/[2021]ResultadosExperimentos/BellPartitions/", dataset_name, sep="")
# comando0 = paste("rclone -v copy ", origem, " ", destino, sep="")
# print(system(comando0))


##################################################################################################
# Get the number of bell partitions                                                              # 
##################################################################################################
setwd(diretorios$folderBellPartDataset)
str_ = paste(dataset_name, "-groupsPerPartitions.csv", sep="")
bell = data.frame(read.csv(str_))
n_bell = nrow(bell)

 
##################################################################################################
# VALIDATION                                                                                     # 
##################################################################################################
cat("\n\n################################################################################################")
cat("\nSTART VALIDATE EXHAUSTIVE PARTITIONS                                                             #")
timeVAl = system.time(resVAl <- validateRUN(number_dataset, number_cores, number_folds, folderResults))

cat("\nVALIDATION: Save results in RS format\n")
str1a <- paste(dataset_name, "-all-results-val.rds", sep="")
setwd(diretorios$folderReVal)
save(resVAl, file = str1a)

#cat("\nVALIDATION: Copy to google drive")
#destino = paste("cloud:elaine/[2021]ResultadosExperimentos/Exhaustive-MicroF1/Train-Test/Micro-F1/", dataset_name, "/Validation/", sep="")
#comando = paste("rclone -v copy ", diretorios$folderReVal, " ", destino, sep="")
#print(system(comando))

cat("\nVALIDATION: Delete all files")
str0 = paste("rm -r ", diretorios$folderResults, sep="")
system(str0)

cat("\nEND VALIDATE EXHAUSTIVE PARTITIONS                                                               #")
cat("\n##################################################################################################")


##################################################################################################
# TEST                                                                                           # 
##################################################################################################

cat("\n\n################################################################################################")
cat("\nSTART TEST EXHAUSTIVE PARTITIONS                                                                 #")

if(dir.exists(folderResults)==FALSE){
  dir.create(folderResults)
}

diretorios <- directories(dataset_name, folderResults)


timeTEST = system.time(resTest <- testRUN(number_dataset, number_cores, number_folds, folderResults))


cat("\nTEST: Save results in RS format\n")
str2a <- paste(dataset_name, "-all-results-test.rds", sep="")
setwd(diretorios$folderReTest)
save(resTest, file = str2a)

#cat("\nTEST: Copy to google drive")
#destino = paste("cloud:elaine/[2021]ResultadosExperimentos/Exhaustive-Clus/Train-Test/MicroF1/", dataset_name, "/Test/", sep="")
#comando = paste("rclone -v copy ", diretorios$folderReTest, " ", destino, sep="")
#print(system(comando))

cat("\nVALIDATION: Delete all files")
str0 = paste("rm -r ", diretorios$folderResults, sep="")
system(str0)


cat("\nEND TEST EXHAUSTIVE PARTITIONS                                                                   #")
cat("\n##################################################################################################")

#cat("\nREPORTS: Copy to google drive")
#destino = paste("cloud:elaine/[2021]ResultadosExperimentos/Exhaustive-Clus/Train-Test/Micro-F1/", dataset_name, sep="")
#comando2 = paste("rclone -v copy ", diretorios$folderReportsDataset, " ", destino, sep="")
#print(system(comando2))

cat("\nTEST: delete folders")

# apagando a pasta reports do dataset
str4 = paste("rm -r ", diretorios$folderReportsDataset, sep="")
print(system(str4))

# apagando a pasta resultados na pasta temporaria
str7 = paste("rm -r ", diretorios$folderResults, sep="")
print(system(str7))

# apagando a pasta das partições BELL do dataset
str8 = paste("rm -r ", diretorios$folderBellPartDataset, sep="")
print(system(str8))

# apagando a pasta do cross validation
str9 = paste("rm -r ", diretorios$folderDatasets, sep="")
print(system(str9))

cat("\n##################################################################################################")
cat("\n# END OF EXHAUSTIVE PARTITIONS. Thanks God!                                                      #") 
cat("\n##################################################################################################")
cat("\n\n\n\n") 

gc()

rm(list=ls())

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
