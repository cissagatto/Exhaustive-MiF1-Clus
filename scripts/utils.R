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
# FUNCTION DIRECTORIES                                                                           #
#   Objective:                                                                                   #
#      Creates all the necessary folders for the project. These are the main folders that must   # 
#      be created and used before the script starts to run                                       #  
#   Parameters:                                                                                  #
#      None                                                                                      #
#   Return:                                                                                      #
#      All path directories                                                                      #
##################################################################################################
directories <- function(dataset_name, folderResults){
  
  retorno = list()
  
  #############################################################################
  # RESULTS FOLDER:                                                           #
  # Parameter from command line. This folder will be delete at the end of the #
  # execution. Other folder is used to store definitely the results.          #
  # Example: "/dev/shm/res"                                                   #
  #############################################################################
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  }
  
  
  folderReports = paste(FolderRoot, "/Reports", sep="")
  if(dir.exists(folderReports) == TRUE){
    setwd(folderReports)
    dir_folderReports = dir(folderReports)
    n_folderReports = length(dir_folderReports)
  } else {
    dir.create(folderReports)
    setwd(folderReports)
    dir_folderReports = dir(folderReports)
    n_folderReports = length(dir_folderReports)
  }
  
  folderReportsDataset = paste(folderReports, "/", dataset_name, sep="")
  if(dir.exists(folderReportsDataset) == TRUE){
    setwd(folderReportsDataset)
    dir_folderReportsDataset = dir(folderReportsDataset)
    n_folderReportsDataset = length(dir_folderReportsDataset)
  } else {
    dir.create(folderReportsDataset)
    setwd(folderReportsDataset)
    dir_folderReportsDataset = dir(folderReportsDataset)
    n_folderReportsDataset = length(dir_folderReportsDataset)
  }
  
  folderReVal = paste(folderReportsDataset, "/Validation", sep="")
  if(dir.exists(folderReVal) == TRUE){
    setwd(folderReVal)
    dir_folderReVal = dir(folderReVal)
    n_folderReVal = length(dir_folderReVal)
  } else {
    dir.create(folderReVal)
    setwd(folderReVal)
    dir_folderReVal = dir(folderReVal)
    n_folderReVal = length(dir_folderReVal)
  }
  
  folderReTest = paste(folderReportsDataset, "/Test", sep="")
  if(dir.exists(folderReTest) == TRUE){
    setwd(folderReTest)
    dir_folderReTest = dir(folderReTest)
    n_folderReTest = length(dir_folderReTest)
  } else {
    dir.create(folderReTest)
    setwd(folderReTest)
    dir_folderReTest = dir(folderReTest)
    n_folderReTest = length(dir_folderReTest)
  }
  
  folderUtils = paste(FolderRoot, "/utils", sep="")
  if(dir.exists(folderUtils) == TRUE){
    setwd(folderUtils)
    dir_folderUtils = dir(folderUtils)
    n_folderUtils = length(dir_folderUtils)
  } else {
    dir.create(folderUtils)
    setwd(folderUtils)
    dir_folderUtils = dir(folderUtils)
    n_folderUtils = length(dir_folderUtils)
  }
  
  folderBellPart = paste(FolderRoot, "/BellPartitions", sep="")
  if(dir.exists(folderBellPart) == TRUE){
    setwd(folderBellPart)
    dir_folderBellPart = dir(folderBellPart)
    n_folderBellPart = length(dir_folderBellPart)
  } else {
    dir.create(folderBellPart)
    setwd(folderBellPart)
    dir_folderBellPart = dir(folderBellPart)
    n_folderBellPart = length(dir_folderBellPart)
  }
  
  folderBellPartDataset = paste(folderBellPart, "/", dataset_name, sep="")
  if(dir.exists(folderBellPartDataset) == TRUE){
    setwd(folderBellPartDataset)
    dir_folderBellPartDataset = dir(folderBellPartDataset)
    n_folderBellPartDataset = length(dir_folderBellPartDataset)
  } else {
    dir.create(folderBellPartDataset)
    setwd(folderBellPartDataset)
    dir_folderBellPartDataset = dir(folderBellPartDataset)
    n_folderBellPartDataset = length(dir_folderBellPartDataset)
  }
  
  folderDatasets = paste(FolderRoot, "/datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  }
  
  folderDatasetX = paste(folderDatasets, "/", dataset_name, sep="")
  if(dir.exists(folderDatasetX) == TRUE){
    setwd(folderDatasetX)
    dir_folderDatasetX = dir(folderDatasetX)
    n_folderDatasetX = length(dir_folderDatasetX)
  } else {
    dir.create(folderDatasetX)
    setwd(folderDatasetX)
    dir_folderDatasetX = dir(folderDatasetX)
    n_folderDatasetX = length(dir_folderDatasetX)
  }
  
  folderCV = paste(folderDatasetX, "/CrossValidation", sep="")
  if(dir.exists(folderCV) == TRUE){
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  } else {
    dir.create(folderCV)
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  }
  
  folderCVTR = paste(folderCV, "/Tr", sep="")
  if(dir.exists(folderCVTR) == TRUE){
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  } else {
    dir.create(folderCVTR)
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  }
  
  folderCVTS = paste(folderCV, "/Ts", sep="")
  if(dir.exists(folderCVTS) == TRUE){
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  } else {
    dir.create(folderCVTS)
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  }
  
  folderCVVL = paste(folderCV, "/Vl", sep="")
  if(dir.exists(folderCVVL) == TRUE){
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  } else {
    dir.create(folderCVVL)
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  }
  
  folderLabelSpace = paste(folderDatasetX, "/LabelSpace", sep="")
  if(dir.exists(folderLabelSpace) == TRUE){
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  } else {
    dir.create(folderLabelSpace)
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  }
  
  folderNamesLabels = paste(folderDatasetX, "/NamesLabels", sep="")
  if(dir.exists(folderNamesLabels) == TRUE){
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  } else {
    dir.create(folderNamesLabels)
    setwd(folderNamesLabels)
    dir_folderLabelSpace = dir(folderNamesLabels)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  }
  
  
  # return folders
  retorno$folderResults = folderResults
  retorno$folderReports = folderReports
  retorno$folderReportsDataset = folderReportsDataset
  retorno$folderReVal = folderReVal
  retorno$folderReTest = folderReTest
  retorno$folderUtils = folderUtils
  retorno$folderDatasets = folderDatasets
  retorno$folderBellPart = folderBellPart
  retorno$folderBellPartDataset = folderBellPartDataset
  retorno$folderCV = folderCV
  retorno$folderCVTR = folderCVTR
  retorno$folderCVTS = folderCVTS
  retorno$folderCVVL = folderCVVL
  retorno$folderLabelSpace = folderLabelSpace
  retorno$folderNamesLabels = folderNamesLabels
  
  # return of folder contents
  retorno$dirResults = dir_folderResults
  retorno$dirReports = dir_folderReports
  retorno$dirReports = dir_folderReportsDataset
  retorno$dirReVal = dir_folderReVal
  retorno$dirReTest = dir_folderReTest
  retorno$dirUtils = dir_folderUtils
  retorno$dirDatasets = dir_folderDatasets
  retorno$dirBellPartitions = dir_folderBellPart
  retorno$dirBellPartitionsDataset = dir_folderBellPartDataset
  retorno$dirCV = dir_folderCV
  retorno$dirCVTR = dir_folderCVTR
  retorno$dirCVTS = dir_folderCVTS
  retorno$dirCVVL = dir_folderCVVL
  retorno$dirLabelSpace = dir_folderLabelSpace
  retorno$dirNamesLabels = dir_folderNamesLabels
  
  # return of the number of objects inside the folder
  retorno$nResults = n_folderResults
  retorno$nReports = n_folderReports
  retorno$nReportsDataset = n_folderReportsDataset
  retorno$nReVal = n_folderReVal
  retorno$nReTest = n_folderReTest
  retorno$nUtils = n_folderUtils
  retorno$nDatasets = n_folderDatasets
  retorno$nBellPartitions = n_folderBellPart
  retorno$nBellPartitionsDataset = n_folderBellPartDataset
  retorno$nCV = n_folderCV
  retorno$nCVTR = n_folderCVTR
  retorno$nCVTS = n_folderCVTS
  retorno$nCVVL = n_folderCVVL
  retorno$nLabelSpace = n_folderLabelSpace
  retorno$nNamesLabels = n_folderNamesLabels
  
  return(retorno)
  gc()
}


##################################################################################################
# FUNCTION CONVERT TO ARFF                                                                       #
#     Objective:                                                                                 #
#        Convert csv file correctly to arff file                                                 #
#     Parameters                                                                                 #
#        arg 1: existing csv file name                                                           #
#        arg 2: name of the arff file to be created                                              #
#        arg 3: specific number of labels that are part of the file. Example: starts at label    # 
#        30 and ends at label 50.                                                                #
#     Return:                                                                                    #
#        The arff file in the specific folder                                                    #
##################################################################################################
converteArff <- function(arg1, arg2, arg3, FolderUtils){  
  str = paste("java -jar ", diretorios$FolderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  print(system(str))
  cat("\n\n")  
}



##################################################################################################
# FUNCTION INFO DATA SET                                                                         #
#  Objective                                                                                     #
#     Gets the information that is in the "datasets.csv" file.                                   #  
#  Parameters                                                                                    #
#     dataset: the specific dataset                                                              #
#  Return                                                                                        #
#     Everything in the spreadsheet                                                              #
##################################################################################################
infoDataSet <- function(dataset){
  retorno = list()
  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$domain = dataset$Domain
  retorno$instances = dataset$Instances
  retorno$attributes = dataset$Attributes
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$MeanIR
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  return(retorno)
  gc()
}


##################################################################################################
# FUNCTION INFO PARTITIONS                                                                       #
#  Objective                                                                                     #
#
#  Parameters                                                                                    #
#
#  Return                                                                                        #
#
##################################################################################################
infoPartitions <- function(id_part,dataset_name, folderResults){
  
  retorno = list()
  
  # setando o diretório específico
  diretorios <- directories(dataset_name, folderResults)
  
  # obtendo a lista de partições
  setwd(diretorios$folderBellPartDataset)
  nome = paste(dataset_name, "-partitions.csv", sep="")
  bell = data.frame(read.csv(nome))
  
  # obtendo o total de grupos por cada partição
  nome2 = paste(dataset_name, "-groupsPerPartitions.csv", sep="")
  groupsPerPartitions = data.frame(read.csv(nome2))
  
  # obtendo o formato da partição
  specificPartition = bell %>% filter(., bell$part == id_part)
  
  # obtendo o total de grupos da partição
  groupSecificPartition = groupsPerPartitions %>% filter(., groupsPerPartitions$part == id_part)
  
  # obtendo o número da partição
  numeroDaParticao = groupSecificPartition$part
  
  # obtendo o número de grupos da partição
  numeroDeGruposDaParticao = groupSecificPartition$totalGroups
  
  # criando a pasta específica da partição
  FolderPartition = paste(diretorios$folderResults, "/Partition-", numeroDaParticao, sep="")
  
  retorno$bell = bell
  retorno$groupsPerPartitions = groupsPerPartitions 
  retorno$specificPartition = specificPartition
  retorno$specificPartition = specificPartition
  retorno$numberOfPartition = numeroDaParticao
  retorno$numberGroupsOfPartition = numeroDeGruposDaParticao
  retorno$FolderPartition = FolderPartition
  
  return(retorno)
  
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################