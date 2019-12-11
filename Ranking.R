library(udpipe)
library(dplyr)
library(readr)
library(stringr)
library(tabulizer)
library(tm)


#nm and nm^2 ?

#do pdf name

material_characterisation <- list( Size = c("diameter", "size", "dimension", "radius"), 
                                   Surface_area = c("surface area"), 
                                   Surface_charge = c("zeta potentiual", "surface charge", "mv"))

#charge the udpipe dataframe
x<-readRDS(file = "Material_and_Methods_Section/Abrams, M T et al 2010.pdf.rds")

print(head(unique(x$sentence), 10))
print(tail(unique(x$sentence), 10))


#the func must know pdf_name
#for loop with it of row
i<-1
pdf_name<-"Abrams, M T et al 2010"
quality_evaluation_df[i]$Article_name<-pdf_name

quality_evaluation_df #add a row for pdf name
for (technical_aspect in names(material_characterisation) ) { #Size, surface area, etc

  for (word in material_characterisation[[technical_aspect]]) { #Diameter
    if (is_space(word)) {
      print(word)
      print("do nothing")
      }
    else {
    print(word)
    idx<-which(x$lemma==word)
    if (length(idx)>0) { #if there is something
      quality_evaluation_df[[technical_aspect]]<-1
    }
  }
  }
}



quality_evaluation_df<-create_quality_df(material_characterisation)

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


is_space <- function(word) {
  res<-str_split(word, "\\s")[[1]]
  if (length(res)==2) {
    return(TRUE)
  }
  return(FALSE)
}



