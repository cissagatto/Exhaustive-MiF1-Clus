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
# LOAD INTERNAL LIBRARIES                                                                        #
##################################################################################################
setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")


##################################################################################################
#                                                                               
##################################################################################################
validationEP <- function(id_part, ds, dataset_name, number_folds, namesLabels, folderResults){
  
  diretorios <- directories(dataset_name, folderResults)
  
  retorno = list()
  
  # get bell partition information
  info <- infoPartitions(id_part, dataset_name, folderResults)
  
  # create the specific folder 
  FolderPartition = paste(diretorios$folderResults, "/Partition-", info$numberOfPartition, sep="")
  
  if(dir.exists(FolderPartition)==FALSE){
    dir.create(FolderPartition)
  }
  
  # start build partitions
  # from fold 1 to last partition
  f = 1
  buildBellPartitions <- foreach(f = 1:number_folds) %dopar% {
    
    cat("\nFold: ", f)   
    
    ############################################################################################################
    # LOAD LIBRARIES
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    library("foreign")
    
    ############################################################################################################
    # SET WORKSPACE
    sistema = c(Sys.info())
    FolderRoot = ""
    if (sistema[1] == "Linux"){
      FolderRoot = paste("/home/", sistema[7], "/Exhaustive-MicroF1-TVT", sep="")
    } else {
      FolderRoot = paste("C:/Users/", sistema[7], "/Exhaustive-MicroF1-TVT", sep="")
    }
    setwd(FolderRoot)
    FolderScripts = paste(FolderRoot, "/scripts", sep="")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ############################################################################################################
    FolderSplit = paste(FolderPartition, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){
      dir.create(FolderSplit)
    }
    
    ############################################################################################################
    converteArff <- function(arg1, arg2, arg3, FolderUtils){  
      str = paste("java -jar ", diretorios$folderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      print(system(str))
      cat("\n\n")  
    }
    
    # get bell partition information
    info = infoPartitions(id_part, dataset_name, folderResults)
    
    g = 1
    while(g<=info$numberGroupsOfPartition){
      
      ####################################################################################
      library("foreign")
      
      ####################################################################################
      nome_particao = paste("partition_", info$numberOfPartition, sep="")
      nome_grupo = paste("group_", g, sep="")
      cat("\n\tPartition: ", nome_particao)
      cat("\n\tGroup: ", nome_grupo)
      
      ####################################################################################
      nomeTR = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
      nomeVL = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")
      
      ####################################################################################
      nome_grupo_2 = paste("Group-", g, sep="")
      FolderGroup = paste(FolderSplit , "/", nome_grupo_2, sep="")
      if(dir.exists(FolderGroup)==FALSE){
        dir.create(FolderGroup)  
      } 
      
      ####################################################################################
      # get the labels of this group
      specificPartition = info$specificPartition
      specificGroup = specificPartition %>% filter(., specificPartition$group == g)
      
      ####################################################################################
      # total de rótulos neste grupo
      totalLabelsThisGr = nrow(specificGroup)
      
      ####################################################################################
      cat("\n\t\tTRAIN: Creating File\n")
      setwd(diretorios$folderCVTR)
      nomeTr2 = paste(diretorios$folderCVTR, "/", nomeTR, sep="")
      arquivoTR = data.frame(read.csv(nomeTr2))
      atributosTR = arquivoTR[ds$AttStart:ds$AttEnd]
      classesTR = select(arquivoTR, specificGroup$labels)
      thisGroupTR = cbind(atributosTR, classesTR)
      
      ####################################################################################
      # TRAIN: Save CSV
      nomeCsTr = paste(FolderGroup, "/grupo_Tr_", g, ".csv", sep="")
      nomeArTr = paste(FolderGroup, "/grupo_Tr_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupTR, nomeCsTr, row.names = FALSE)
      
      ####################################################################################
      # Targets
      inicio = ds$LabelStart
      fim = ncol(thisGroupTR)
      ifr = data.frame(inicio, fim)
      setwd(FolderGroup)
      write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
      
      ####################################################################################
      # TRAIN: Convert CSV to ARFF
      setwd(FolderGroup)
      arg1Tr = nomeCsTr
      arg2Tr = nomeArTr
      arg3Tr = paste(inicio, "-", fim, sep="")
      converteArff(arg1Tr, arg2Tr, arg3Tr, diretorios$folderUtils)
      
      ####################################################################################
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nomeArTr, sep="")
      print(system(str0))
      
      ####################################################################################
      cat("\n\t\tVALIDATION: Creating File\n")
      setwd(diretorios$folderCVVL)
      nomeVl2 = paste(diretorios$folderCVVL, "/", nomeVL, sep="")
      arquivoVL = data.frame(read.csv(nomeVl2))
      atributosVL = arquivoVL[ds$AttStart:ds$AttEnd]
      classesVL = select(arquivoVL, specificGroup$labels)
      thisGroupVL = cbind(atributosVL, classesVL)
      
      ####################################################################################
      #VALIDATION: Save CSV
      nomeCsVl = paste(FolderGroup, "/grupo_Vl_", g, ".csv", sep="")
      nomeArVl = paste(FolderGroup, "/grupo_Vl_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupVL, nomeCsVl, row.names = FALSE)
      
      ####################################################################################
      # TEST: Convert CSV to ARFF
      setwd(FolderGroup)
      arg1Vl = nomeCsVl
      arg2Vl = nomeArVl
      arg3Vl = paste(inicio, "-", fim, sep="")
      converteArff(arg1Vl, arg2Vl, arg3Vl, diretorios$folderUtils)
      
      ####################################################################################
      str1 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nomeArVl, sep="")
      print(system(str1))
      
      ####################################################################################
      # Creating .s file for Clus
      setwd(FolderGroup)
      
      nome_config = paste("grupo_", g, ".s", sep="")
      sink(nome_config, type = "output")
      
      cat("[General]")          
      cat("\nCompatibility = MLJ08")
      
      cat("\n")
      cat("\n[Data]")
      
      nome_arquivo_2 = paste("grupo_Tr_", g, ".arff", sep="")
      cat(paste("\nFile = ", nome_arquivo_2, sep=""))
      
      nome_arquivo_3 = paste("grupo_Vl_", g, ".arff", sep="")
      cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))
      
      cat("\n")
      cat("\n[Attributes]")
      cat("\nReduceMemoryNominalAttrs = yes")
      
      cat("\n")
      cat("\n[Attributes]")
      cat(paste("\nTarget = ", inicio, "-", fim, sep=""))
      cat("\nWeights = 1")
      
      cat("\n")
      cat("\n[Tree]")
      cat("\nHeuristic = VarianceReduction")
      cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
      
      cat("\n")
      cat("\n[Model]")
      cat("\nMinimalWeight = 5.0")
      
      cat("\n")
      cat("\n[Output]")
      cat("\nWritePredictions = {Test}")
      cat("\n")
      sink()
      
      ####################################################################################
      # Execute CLUS
      nome_config2 = paste(FolderGroup, "/", nome_config, sep="")
      setwd(FolderGroup)      
      str = paste("java -jar ", diretorios$folderUtils, "/Clus.jar ", nome_config2, sep="")
      print(system(str))
      
      ####################################################################################
      # Open inicioFimRotulos.csv
      targets = data.frame(read.csv("inicioFimRotulos.csv"))
      
      ####################################################################################
      # Open predictions
      setwd(FolderGroup)
      #library("foreign")
      namae2 = paste(FolderGroup, "/grupo_", g, ".test.pred.arff", sep="")
      predicoes = data.frame(read.arff(namae2))
      
      ####################################################################################
      # Split Y True and Y Predicts
      
      if(targets$inicio == targets$fim){
        
        library("foreign")
        cat("\n\t\t\tOnly one label in this group\n")
        
        ####################################################################################
        # Save Y_True
        setwd(FolderGroup)
        classes = data.frame(predicoes[,1])
        names(classes) = colnames(predicoes)[1]
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ####################################################################################
        # Save Y_Predict
        rot = paste("Pruned.p.", colnames(predicoes)[1], sep="")
        pred = data.frame(predicoes[,rot])
        names(pred) = colnames(predicoes)[1]
        setwd(FolderGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        ####################################################################################
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        
        gc()
        
      } else {
        
        library("foreign")
        
        ####################################################################################
        # More than one label in this group
        comeco = 1+(targets$fim - targets$inicio)
        
        ####################################################################################
        # Save Y_true
        classes = data.frame(predicoes[,1:comeco])
        setwd(FolderGroup)
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ####################################################################################
        # Save Y_Predict
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        nomeColuna = c()
        t = 1 
        while(t <= n_r){
          nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep="")
          t = t + 1
          gc()
        }
        pred = data.frame(predicoes[nomeColuna])
        names(pred) = rotulos
        setwd(FolderGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        gc()
      } # END ELSE
      
      ########################################################################################################################
      nome1 = paste("grupo_Tr_", g, ".arff", sep="")
      nome2 = paste("grupo_Vl_", g, ".arff", sep="")
      nome3 = paste("grupo_Tr_", g, ".csv", sep="")
      nome4 = paste("grupo_Vl_", g, ".csv", sep="")
      nome5 = paste("grupo_", g, ".model", sep="")
      nome6 = paste("grupo_", g, ".s", sep="")
      
      setwd(FolderGroup)
      unlink(nome1, recursive = TRUE)
      unlink(nome2, recursive = TRUE)
      unlink(nome3, recursive = TRUE)
      unlink(nome4, recursive = TRUE)
      unlink(nome5, recursive = TRUE)
      unlink(nome6, recursive = TRUE)
      
      ########################################################################################################################
      # count
      g = g + 1
      
      gc()
    } # END GROUP
    
    gc()
  }
  
  ########################################################################################################################
  # return
  retorno$id_part = id_part
  retorno$ds = ds
  retorno$dataset_name = dataset_name
  retorno$number_folds = number_folds
  retorno$namesLabels = namesLabels
  retorno$infoPartition = info
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# Build and Test Partitions: END                                                                 #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GATHER PREDICTS HYBRID PARTITIONS                                                     #
#   Objective                                                                                    #
#      From the file "test.pred.arff", separates the real labels and the predicted labels to     # 
#      generate the confusion matrix to evaluate the partition.                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       true labels and predicts labels                                                          #
##################################################################################################
gather <- function(id_part, ds, dataset_name, n, namesLabels, folderResults){
  
  retorno = list()
  
  diretorios <- directories(dataset_name, folderResults)
  
  info <- infoPartitions(id_part,dataset_name, folderResults)
  
  # start build partitions
  # do fold 1 até o último fold
  f = 1
  gatherParal <- foreach(f = 1:number_folds) %dopar%{
    
    # data frame
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)  
    
    cat("\nFold: ", f)
    
    FolderSplit = paste(diretorios$folderResults, "/Partition-", info$numberOfPartition, "/Split-", f, sep="")
    
    g = 1
    while(g<=info$numberGroupsOfPartition){
      
      cat("\n\tGroup: ", g)
      
      FolderGroup = paste(FolderSplit, "/Group-", g, sep="")
      
      #cat("\nGather y_true ", g, "\n")
      setwd(FolderGroup)
      y_true_gr = data.frame(read.csv("y_true.csv"))
      y_true = cbind(y_true, y_true_gr)
      #print(nrow(y_true))
      
      #cat("\nGather y_predict ", g, "\n")
      y_pred_gr = data.frame(read.csv("y_predict.csv"))
      y_pred = cbind(y_pred, y_pred_gr)
      #print(nrow(y_pred))
      
      # deleting files
      unlink("y_true.csv", recursive = TRUE)
      unlink("y_predict.csv", recursive = TRUE)
      unlink("inicioFimRotulos.csv", recursive = TRUE)
      
      g = g + 1
      gc()
    }
    
    #cat("\nSave files ", g, "\n")
    FolderSplit = paste(diretorios$folderResults, "/Partition-", info$numberOfPartition, "/Split-", f, sep="")
    setwd(FolderSplit)
    y_pred = y_pred[,-1]
    y_true = y_true[,-1]
    write.csv(y_pred, "y_predict.csv", row.names = FALSE)
    write.csv(y_true, "y_true.csv", row.names = FALSE)
    
    gc()
  } # fim do foreach
  
  retorno$id_part = id_part
  retorno$ds = ds
  retorno$dataset_name = dataset_name
  retorno$number_folds = number_folds
  retorno$namesLabels = namesLabels
  retorno$infoPartition = info
  
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# Gather Predicts: END                                                                           #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
} # fim da função




##################################################################################################
# FUNCTION EVALUATION HYBRID PARTITIONS                                                          #
#   Objective                                                                                    #
#      Evaluates the hybrid partitions                                                           #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Assessment measures for each hybrid partition                                            #
##################################################################################################
eval <- function(id_part, ds, dataset_name, number_folds, namesLabels, folderResults){
  
  retorno = list()
  
  diretorios = directories(dataset_name, folderResults)
  
  info <- infoPartitions(id_part, dataset_name, folderResults)
  
  # from fold = 1 to number_folder
  f = 1
  evalParal <- foreach(f = 1:number_folds) %dopar%{  
    
    library("mldr")
    library("utiml")
    
    cat("\nFold: ", f)
    
    # data frame
    apagar = c(0)
    confMatPartitions = data.frame(apagar)
    partitions = c()
    
    # specifyin folder for the fold
    FolderSplit = paste(diretorios$folderResults, "/Partition-", info$numberOfPartition, "/Split-", f, sep="")
    
    # get the true and predict lables
    setwd(FolderSplit)
    y_true = data.frame(read.csv("y_true.csv"))
    y_pred = data.frame(read.csv("y_predict.csv"))
    
    # compute measures multilabel
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    #cat("\n\t\tSave Confusion Matrix")
    setwd(FolderSplit)
    salva3 = paste("Conf-Mat-Fold-", f, ".txt", sep="")
    sink(file=salva3, type="output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    # creating a data frame
    confMatPart = multilabel_evaluate(confmat)
    confMatPart = data.frame(confMatPart)
    names(confMatPart) = paste("Fold-", f, sep="")      
    namae = paste("Split-", f,"-Evaluated.csv", sep="")
    write.csv(confMatPart, namae)  
    
    # delete files
    setwd(FolderSplit)
    unlink("y_true.csv", recursive = TRUE)
    unlink("y_predict.csv", recursive = TRUE)
    
    gc()
  } # end folds
  
  retorno$id_part = id_part
  retorno$ds = ds
  retorno$dataset_name = dataset_name
  retorno$number_folds = number_folds
  retorno$namesLabels = namesLabels
  retorno$infoPartition = info
  
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# Evaluation Folds: END                                                                          #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GATHER EVALUATIONS                                                                    #
#   Objective                                                                                    #
#       Gather metrics for all folds                                                             #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Assessment measures for all folds                                                        #
##################################################################################################
gatherEvaluation <- function(id_part, ds, dataset_name, number_folds, namesLabels, folderResults){  
  
  retorno = list()
  
  diretorios = directories(dataset_name, folderResults)
  
  info <- infoPartitions(id_part, dataset_name, folderResults)
  
  FolderPartition = paste(diretorios$folderResults, "/Partition-", info$numberOfPartition, sep="")
  
  # vector with names
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  # data frame
  apagar = c(0)
  avaliado4 = data.frame(apagar)
  folds = c(0)
  nomesFolds = c(0)
  
  # from fold = 1 to number_folders
  f = 1
  while(f<=number_folds){  
    
    cat("\nFold: ", f)
    
    # specifying folder for the fold
    FolderSplit = paste(FolderPartition, "/Split-", f, sep="")
    setwd(FolderSplit)
    str = paste("Split-", f, "-Evaluated.csv", sep="")
    avaliado = data.frame(read.csv(str))
    names(avaliado)[1] = "medidas"
    avaliado2 = data.frame(avaliado[order(avaliado$medidas, decreasing = FALSE),])
    avaliado3 = data.frame(avaliado2[,-1])
    avaliado4 = cbind(avaliado4, avaliado3)
    #names(avaliado4)[f+1] = paste("Fold-", f, sep="")
    nomesFolds[f] = paste("Fold-", f, sep="")
    
    setwd(FolderSplit)
    unlink(str)
    
    f = f + 1
    gc()
    
  } # end folds
  
  #cat("\nSAVE MEASURES")
  avaliado4$apagar = measures
  colnames(avaliado4) = c("measures", nomesFolds)
  
  setwd(info$FolderPartition)
  nome3 = paste(dataset_name, "-Partition-", info$numberOfPartition, "-Evaluated.csv", sep="")
  write.csv(avaliado4, nome3, row.names = FALSE)
  
  setwd(diretorios$folderResults)
  write.csv(avaliado4, nome3, row.names = FALSE)
  
  setwd(diretorios$folderReVal)
  write.csv(avaliado4, nome3, row.names = FALSE)
  
  retorno$id_part = id_part
  retorno$ds = ds
  retorno$dataset_name = dataset_name
  retorno$number_folds = number_folds
  retorno$namesLabels = namesLabels
  retorno$infoPartition = info
  retorno$evaluated10F = avaliado4
  
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# Evaluated Partition: END                                                                       #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             # 
##################################################################################################
validation <- function(number_dataset, number_cores, number_folds, id_part, folderResults){
  
  diretorios <- directories(dataset_name, folderResults)
  
  if(number_cores == 0){
    cat("\nZero is a disallowed value for number_cores. Please choose a value greater than or equal to 1.")
  } else {
    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n################################################################################################")
      cat("\n# Running Sequentially!                                                                          #")
      cat("\n##################################################################################################\n\n") 
    } else {
      cat("\n\n################################################################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!                                             #")
      cat("\n##################################################################################################\n\n") 
    }
  }
  cl = cl
  
  retorno = list()
  
  cat("\n\n################################################################################################")
  cat("\n# Run Validation: Get dataset information: ", number_dataset, "                                  #")
  ds = datasets[number_dataset,]
  names(ds)[1] = "Id"
  dataset_name = toString(ds$Name)
  cat("\n# Dataset: ", dataset_name, "                                                                     #")   
  cat("\n\n#################################################################################################")
  
  setwd(diretorios$folderNamesLabels)
  arquivo = paste(dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  
  cat("\n\n################################################################################################")
  cat("\nRun Validation: Get bell partition information                                                   #")
  info <- infoPartitions(id_part, dataset_name, folderResults)
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n# Run Validation: Build and Test Bell Partitions                                                 #")
  timeComPart = system.time(resPart <- validationEP(id_part, ds, dataset_name, number_folds, namesLabels, folderResults)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n# Run Validation: Matrix Correlation                                                             #")
  timeGather = system.time(resGather <- gather(id_part, ds, dataset_name, number_folds, namesLabels, folderResults)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n# Run Validation: Evaluation Fold                                                                #")
  timeEval = system.time(resEval <- eval(id_part, ds, dataset_name, number_folds, namesLabels, folderResults)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n# Run Validation: Gather Evaluation                                                              #")
  timeGE = system.time(resGE <- gatherEvaluation(id_part, ds, dataset_name, number_folds, namesLabels, folderResults)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n# Run Validation: Save Runtime                                                                   #")
  Runtime = rbind(timeComPart, timeGather, timeEval, timeGE)
  setwd(info$FolderPartition)
  name2 = paste("Partition-", info$numberOfPartition ,"-Runtime-Validation-Exhaustive.csv", sep="")
  write.csv(Runtime, name2, row.names = FALSE)
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n# Run Validation: Stop Parallel                                                                  #")
  parallel::stopCluster(cl) 
  cat("\n\n################################################################################################")
  
  gc()
}


validateRUN <- function(number_dataset, number_cores, number_folds, folderResults){
  
  diretorios <- directories(dataset_name, folderResults)
  
  # from partition 2 to last partition (n)
  count = 2
  id_part = 2
  while(id_part<=n_bell){
    
    cat("\nPARTITION: ", id_part, "\n")
    
    cat("\nVALIDATION: Execute exhaustive experiment\n")
    timeEP = system.time(resEPVAL <- validation(number_dataset, number_cores, number_folds, id_part, folderResults))
    print(timeEP)
    
    info <- infoPartitions(id_part, dataset_name, folderResults)
    
    cat("\nVALIDATION: Save results in RS format\n")
    str1a <- paste(dataset_name, "-Partition-", id_part, "-Results-Validation.rds", sep="")
    setwd(info$FolderPartition)
    save(resEPVAL, file = str1a)
    
    cat("\nVALIDATION: Save results in RDATA format\n")
    str2a <- paste(dataset_name, "-Partition-", id_part, "-Results-Validation.RData", sep="")
    setwd(info$FolderPartition)
    save(resEPVAL, file = str2a)
    
    cat("\nVALIDATION: Compress folders and files")
    str3a <- paste("tar -zcvf ", diretorios$folderResults, "/", dataset_name, "-Partition-", id_part, "-validation-results.tar.gz ", info$FolderPartition, sep="")
    system(str3a)
    
    cat("\nVALIDATION: Copy zip file to the root results folder\n")
    str5a = paste("cp ", diretorios$folderResults, "/", dataset_name, "-Partition-", id_part, "-validation-results.tar.gz ", diretorios$folderReVal, sep="")
    system(str5a)
    
    cat("\nVALIDATION: Delete all files")
    str3 = paste("rm -r ", diretorios$folderResults, "/Partition-",id_part , sep="")
    system(str3)
    
    # count
    id_part = id_part + 1
    count = count + 1
    
    cat("\n")
    gc()
  }
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################