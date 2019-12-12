library(udpipe)
library(dplyr)
library(readr)
library(stringr)
library(tabulizer)
library(tm)

create_quality_df <- function(material_characterisation) {
  #this function create the dataframe quality_evaluation that store the informations
  #of the quality evalution assessment
  
  #nb_technical_aspect<-length(material_characterisation)
  technical_aspect<-names(material_characterisation)
  names_columns<-c("Article_name", technical_aspect)
  
  
  quality_evaluation_df<-as.data.frame(matrix(ncol=length(names_columns), 
                                              byrow=TRUE))
  
  colnames(quality_evaluation_df)<-names_columns
  
  return(quality_evaluation_df)
}

init_quality_df <- function(quality_evaluation_df, pdf_name) {
  #create empty dataframe is such a pain in R
  #initiate the quality_df dataframe, i.e., add zero everywhere and the pdf name,
  #for the first iteration of the loop
  quality_evaluation_df[["Article_name"]]<-pdf_name
  quality_evaluation_df[is.na(quality_evaluation_df)] = 0
  
  return(quality_evaluation_df)
}

is_space <- function(word) {
  res<-str_split(word, "\\s")[[1]]
  if (length(res)==2) {
    return(TRUE)
  }
  return(FALSE)
}

cut_word <- function(word) {
  res<-str_split(word, "\\s")[[1]]
  return(res)
}

attribute_ranking_split_word <- function(x, i, quality_evaluation_df, technical_aspect, word) {
  res<-cut_word(word)
  first_word<-res[1]
  second_word<-res[2]
  idx<-which(x$lemma==first_word) #firstword
  
  if (length(idx)>0) { #if the word is present as lemma
    for (id in idx) {   #if several times the word, simply
      if (x[(id+1),]$lemma==second_word) {
        quality_evaluation_df[i,][[technical_aspect]]<-1
    }
    }
    }
  
  return(quality_evaluation_df)
}

attribute_ranking <- function(x, i, quality_evaluation_df, technical_aspect, word) {
  idx<-which(x$lemma==word)
  if (length(idx)>0) { #if the word is present as lemma
    quality_evaluation_df[i,][[technical_aspect]]<-1
    # if (word=="size") {
    #   print(x[idx,]$sentence)
    # }
  }
  return(quality_evaluation_df)
}

quality_assessment <- function(x, i, pdf_name, quality_evaluation_df, ontology_material_characterisation) {
  if (first_iteration(i)) {
    quality_evaluation_df<-init_quality_df(quality_evaluation_df, pdf_name)
  } else {
    quality_evaluation_df<-rbind(quality_evaluation_df, c(0))
    quality_evaluation_df[i,]$Article_name<-pdf_name
  }
  for (technical_aspect in names(ontology_material_characterisation)) { #Size, surface area, etc
    for (word in ontology_material_characterisation[[technical_aspect]]) { #Diameter
      if (is_space(word)) {
        quality_evaluation_df<-attribute_ranking_split_word(x, i, quality_evaluation_df, technical_aspect, word)
      }
      else {
        quality_evaluation_df<-attribute_ranking(x, i, quality_evaluation_df, technical_aspect, word)
      }
    }
  }
  
  return(quality_evaluation_df)
}

first_iteration <- function(i) {
  if (i==1) {
    return(TRUE)
  }
  return(FALSE)
}

biological_condition <- function(x, i, quality_evaluation_df, condition, word) {
  idx<-which(x$lemma==word)
  if (length(idx)>0) { #if the word is present as lemma
    quality_evaluation_df[i,][[condition]]<-"Yes"
    # if (word=="size") {
    #   print(x[idx,]$sentence)
    # }
  }
  return(quality_evaluation_df)
}

biological_condition_split_word <- function(x, i, quality_evaluation_df, condition, word) {
  res<-cut_word(word)
  first_word<-res[1]
  second_word<-res[2]
  idx<-which(x$lemma==first_word) #firstword
  
  if (length(idx)>0) { #if the word is present as lemma
    for (id in idx) {   #if several times the word in the article
      if (x[(id+1),]$lemma==second_word) {
        quality_evaluation_df[i,][[condition]]<-"Yes"
      }
    }
  }
  
  return(quality_evaluation_df)
}

bioligical_condition_assessment <- function(x, i, pdf_name, quality_evaluation_df, ontology_in_vitro_in_vivo) {
  if (first_iteration(i)) {
    quality_evaluation_df$In_vitro<-"No"
    quality_evaluation_df$In_vivo<-"No"
  } else {
    quality_evaluation_df[i,]$In_vitro<-"No"
    quality_evaluation_df[i,]$In_vivo<-"No"
  }
  
  for (condition in names(ontology_in_vitro_in_vivo)) { 
    for (word in ontology_in_vitro_in_vivo[[condition]]) { 
      if (is_space(word)) {
        
        quality_evaluation_df<-biological_condition_split_word(x, i, quality_evaluation_df, condition, word)
      }
      else {
        quality_evaluation_df<-biological_condition(x, i, quality_evaluation_df, condition, word)
      }
    }
  }
  
  return(quality_evaluation_df)
}


#ontology of terms
ontology_material_characterisation <- list( Size = c("diameter", "size", "dimension", "radius", "nm"), 
                                   Surface_area = c("surface area"), 
                                   Surface_charge = c("zeta potential", "surface charge", "mv"),
                                   Chemical_composition = c("chemical composition", "coating", "core",
                                                            "shell", "content", "configuration", 
                                                            "molecular ratio"),
                                   Concentration = c("final concentration", "dilution", "diluted", "mM"),
                                   Aggregation = c("aggregation", "aggregate", "polydisperse", "monodisperse",
                                                   "stability", "agglomeration", "stable", "agglomerate"),
                                   Wavelenght = c("wavelenght", "excitation wavelenght", "excitation", "emission",
                                                  "emission wavelenght")
                                   )

#cells/ml
#cells per wll
#% of CO2
#light
#dose in mg/kg

ontology_in_vitro_in_vivo <- list( In_vitro = c("incubation temperature", "incubator", "incubated", "viability",
                                                "cells/ml", "cells/mL",
                                                "cultured", "culture", "medium", "media", "penicillin", 
                                                "streptomycin", "well plates", "cell line", "cells"
                                                ), 
                                   
                                  In_vivo = c("age", "year old", "month old", "year-old", "month-old",
                                              "cage", "holding rooms", "housed", "facility", "diet", "ad libitum",
                                              "dark cycle", "euthanized", "acclimate", "acclimatized", 
                                              "body weight", "administration route")
                                                )

quality_evaluation_df<-create_quality_df(ontology_material_characterisation)

rds_list<-list.files(path="Material_and_Methods_Section/", pattern = "\\.rds$")

run_assesment <- function(rds_list, ontology_material_characterisation, ontology_in_vitro_in_vivo) {
  i<-0
  quality_evaluation_df<-create_quality_df(ontology_material_characterisation)
  for (rds in rds_list) {
    i<-i+1
    x<-readRDS(file = paste0("Material_and_Methods_Section/", rds))
    pdf_name<-str_replace_all(rds, ".rds", "")
    quality_evaluation_df<-quality_assessment(x, i, pdf_name, quality_evaluation_df, ontology_material_characterisation)
    quality_evaluation_df<-bioligical_condition_assessment(x, i, pdf_name, quality_evaluation_df, ontology_in_vitro_in_vivo) 
  }
  #compute the score
  quality_evaluation_df$Ranking<-rowSums(quality_evaluation_df[, names(ontology_material_characterisation)])
  return(quality_evaluation_df)
}

quality_evaluation_df<-run_assesment(rds_list, ontology_material_characterisation, ontology_in_vitro_in_vivo)


