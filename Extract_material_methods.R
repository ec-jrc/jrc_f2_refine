library(udpipe)
library(dplyr)
library(readr)
library(stringr)

options(java.parameters = "-Xmx8000m") ### important to load before library(rJava)
library(tabulizer)

prepare_poppler_output <- function(pdf_name) {
  
  poppler_output <- read_lines(paste0(pdf_name, ".output_poppler.txt"))
  #this command remove the lines that indicate change of page ("page 1/5") and the "----"
  poppler_output<-poppler_output[-which(nchar(poppler_output)<12)]
}

annotate_txt_pdf<- function(txt_pdf) {
  #This function create the adequate NLP datastructure from the text of the pdf
  #Load the model to do the annotation of the text.
  ud_model_gum <- udpipe_load_model(file = "english-gum-ud-2.4-190531.udpipe" )
  #x is the standard name of the dataframe in the documentation of udpipe
  #annotate the txt file
  x <- udpipe_annotate(ud_model_gum, x = txt_pdf)
  x <- as.data.frame(x)
  return(x)
}

repair_df <- function(df) {
  #the function fix the variable inside the df dataframe
  #the unlisting is necessary because of the continuous addition of dataframe inside the global df
  #during the sapply(poppler_output, read_outpout_poppler)
  #also the size of the font must be converted to a numericc
  df$Word<-unlist(df$Word)
  df$Font<-unlist(df$Font)
  df$Size<-unlist(df$Size)
  df$Size<-as.numeric(df$Size)
  return(df)
}

extract_word <- function(poppler_output) {
  #read lines and ouput word and its font
  word<-str_extract(poppler_output, "\\[.*\\]")
  word<-str_replace_all(word, "\\[", "")
  word<-str_replace_all(word, "\\]", "")
  return(word)
}

extract_font <- function(poppler_output) {
  #read lines and ouput word and its font
  font<-str_extract(poppler_output, "fontname=.* fontsize")
  font<-str_replace_all(font, "fontname=", "")
  font<-str_replace_all(font, " fontsize", "")
  
  return(font)
}

extract_font_size <- function(poppler_output) {
  #read lines and ouput word and its font
  font_size<-str_extract(poppler_output, "fontsize=.* wmode")
  font_size<-str_replace_all(font_size, "fontsize=", "")
  font_size<-str_replace_all(font_size, " wmode", "")
  
  return(font_size)
}

read_poppler_line <- function(poppler_line) {
  #Please pay attention to the use of the "<<-"
  #It is the way to assign global variable in R.
  word<-extract_word(poppler_line)
  font<-extract_font(poppler_line)
  font_size<-extract_font_size(poppler_line)
  
  df_local<-as.data.frame(matrix(list(word, font, font_size), ncol=3, byrow=TRUE))
  colnames(df_local)<-c("Word", "Font", "Size")
  df_poppler<<-rbind(df_poppler, df_local)
}

read_outpout_poppler <- function(pdf_name) {
  #read the poppler_output
  #read one line of poppler_output and extract word, font, size of the font
  #read_poppler_line assign value to a global variable behind the scenes
  #Please pay attention to the "<<-"
  poppler_output<-prepare_poppler_output(pdf_name)  
  
  df_poppler<<-data.frame()
  sapply(poppler_output, read_poppler_line)
  df_poppler<<-repair_df(df_poppler)
  return(df_poppler)
}

identify_font <- function(df_poppler) {
  #this function identify the fonts use for the section titles
  #it first try to identify where are the words References and Acknowledgements in the poppler output
  #after, in order, it will try to identify if there is a section named references non empty
  #and then identify the font use for it
  #if references is emptym, it will try to identify if there is a section name Acknowledgement
  
  #needed in this function, but would be redo outside
  reference_df<-df_poppler[which(df_poppler$Word %in% c("References")),]
  ack_df<-df_poppler[which(df_poppler$Word %in% c("Acknowledgements", "Acknowledgments")),]
  
  if (dim(reference_df)[1]>0) {#if Reference exist
    reference_df<-reference_df[which(reference_df$Size==max(reference_df$Size)),]
    font_sections<-unique(reference_df$Font)
    #sections_titles<-rbind(reference_df, sections_titles)
  } else { #if Acknowledgement exist
    if (dim(ack_df)[1]>0) {
      font_sections<-unique(ack_df$Font)
    }}
  
  return(font_sections)
}

find_section_titles <- function(vector_title, font_section) {
    assumed_title_df<-df_poppler[which(df_poppler$Word %in% vector_title),]
    if (dim(assumed_title_df)[1] > 0) { #if section exist
      assumed_title_df<-assumed_title_df[which(assumed_title_df$Size==max(assumed_title_df$Size)),]
      if (dim(assumed_title_df)[1] > 1) {#if there is several words with same size
        assumed_title_df<-assumed_title_df[which(assumed_title_df$Font==font_section),]}
      if (assumed_title_df$Font == font_section){
        return(assumed_title_df)
    }}
}

create_section_title_df <- function(font_section, list_of_sections) {
  #create and return a dataframe with the section, their font and the size of the font like this :
  #Word                         Font                 Size
  #293     Introduction VMUQDX+ITCStoneSans-Semibold 10.0
  #1321         Results VMUQDX+ITCStoneSans-Semibold 10.0
  section_title_df<-data.frame()
  for (vector_title in list_of_sections) {
    section_title_df<-rbind(section_title_df, find_section_titles(vector_title, font_section))
  }
  section_title_df <- section_title_df[order(as.numeric(row.names(section_title_df))),]
  return(section_title_df)}

filter_first_lemma <- function(x, index){
  #this function select lemma/tokens that are the first element of a sentence
  #x[index,] return a token and all the associated data : lemma, but also sentence and doc_id
  occurrence<-x[index,] #x[index,], where x is the dataframe of annotation generated by udpipe
  lemma<-x[index,]$lemma 
  sentence_id<-occurrence$sentence_id
  #the following line query the first lemma of the sentence in the good document
  first_lemma<-x[which(x$sentence_id==sentence_id)[1],]$lemma
  if (first_lemma==lemma) {return(TRUE)} 
  return(FALSE)
}

filter_association_first_token <- function(x, index, section_title_df){
  #update tabulizer : tolower can lead to unexpected results
  #doubling the if is a quick and safe fix
  #this function looks if the first token is already in section_title_df
  #x[index,] return a token and all the associated data : lemma, but also sentence and doc_id
  occurrence<-x[index,] #x[index,], where x is the dataframe of annotation generated by udpipe
  sentence_id<-occurrence$sentence_id
  #the following line query the first lemma of the sentence in the good document
  first_token<-x[which(x$sentence_id==sentence_id)[1],]$token
  if (first_token %in% section_title_df$Word) {return(TRUE)}
  if (capitalize_first_letter(first_token) %in% section_title_df$Word) {return(TRUE)} 
  return(FALSE)
}

NLP_filter_second_member_section_title <- function(x, occurrences, section_title_df){
  #this function return the first occurrence passed as input for which the first token of the sentence
  #already appear in the dataframe section_title_df
  #Typically, if there is a Material and Methods section, and several time the words methods in the article,
  #The others functions cannot distinguished which occurrence to select (since it is not the first lemma of a sentence)
  #Please refer to the rmarkdown documents Materials_methods_EDA_gum.html and EDA_part2.html for more details.
  for (index in occurrences){
    if (filter_association_first_token(x, index, section_title_df)){return(index)}
  }}

NLP_filter_section_title <- function(x, occurrences){
  #this function return the first occurrence passed as input which is the first lemma of a sentence. 
  #Typically, it distinguish between sentence "northen blot as discribe in Materials and Methods" and 
  #sentence like "Materials and Methods", the second one beeing the materials and methods section.
  #Please refer to the rmarkdown documents Materials_methods_EDA_gum.html and EDA_part2.html for more details.
  for (index in occurrences){
    if (filter_first_lemma(x, index)){return(index)}
  }}

subset_occurrences <- function(occurrences, positions_sections_df){
  #what is going on in this function ? simply speaking, it reduce the search of section names to portion
  #of the article after the already annotated sections title
  #in Abram et al 2010, Introduction is at position 338, Results at 1501, Discussion at 5975
  #there is occurrences of "Materials" at 1799, 2747, 3816, 7055. The three first one are likely part of the
  #Result section.
  #Following : if it is not the first iteration of the loop
  #i.e. if a section has already been positionned
  if (dim(positions_sections_df)[1]>0) {
    occurrences<-occurrences[which(occurrences > max(positions_sections_df$occurrences))]
    return(occurrences)}
  else{return(occurrences)}}

reduce_occurrences<- function(x, occurrences, positions_sections_df, section_title_df){
  if (length(occurrences)>1){ #if several time the section name in the article
    occurrences<-subset_occurrences(occurrences, positions_sections_df)}
  if (length(occurrences)>1){ #if there is still several time the section name in the article
    occurrences_NLP<-NLP_filter_section_title(x, occurrences)
    if (length(occurrences_NLP)>0){ #if not NULL, like for Methods in Materials and Methods
      occurrences<-occurrences_NLP}}
  if (length(occurrences)>1){ #if there is still several time the section name in the article
    occurrences<-NLP_filter_second_member_section_title(x, occurrences, section_title_df)}
  return(occurrences)}

locate_sections_position <- function(x, section_title_df){
  #this function create and return the a dataframe with the name of the section and it start position inside x
  #reduce_occurrences() use the order of the sections inside the document and NLP approach to reduce the number
  #of occurrences to one, i.e. to select among the different occurrences of a section title which one 
  #correspond to the section title. More description in them. For below :
  #https://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop/13442634
  #Update tabulizer : after using tabulizer to read the pdf, some gotchas seem to appear in some section title :
  #in Abrams et al 2010, "Introduction" became "IntroductIon". Token was impossible to find. 
  #Tolower() is added to keep using token. Lemma would require playing with the plurals.
  
  positions_sections_df<-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("section", "occurrences"))
  
  for (section in section_title_df$Word){
    occurrences<-which(x$token %in% section)
    if (length(occurrences)>1){ #if several time the section name in the article
      occurrences<-subset_occurrences(occurrences, positions_sections_df)}
    if (length(occurrences)==0) {#if there no hits anym because tabulizer funny caps in section title
      occurrences<-which(capitalize_first_letter(x$token) %in% section) #replace by a more elaborate function ?
      #to lower but not for first letter
    }
    occurrences<-reduce_occurrences(x, occurrences, positions_sections_df, section_title_df)
    positions_sections_df<-rbind(positions_sections_df, data.frame(section, occurrences))
  }
  return(merging_section(positions_sections_df))}

merging_section <- function(positions_sections_df) {
  #this function aims to merge really close section in positions_section_df
  #the problem arise from section title like "results and discussion", or even simplier, "materials and methods"
  #the way the results of the poppler is read does not discriminate for section in the same sentence
  #at the moment, a section "results and discussion" would create two entries in positions_sections_df
  #this function is a patch that will gather together section in positions_sections_df that occured at a distance
  #of two words or so.
  #Please refer to the Toc_extraction.html document, "Exraction of the table of content of pdf files"
  positions_sections_df$section<-as.character(positions_sections_df$section)
  for (i in 1:(length(positions_sections_df$occurrences)-1)){
    gap=positions_sections_df$occurrences[[i+1]]-positions_sections_df$occurrences[[i]]
    if (gap<4){
      new_section<-paste(positions_sections_df[i,]$section, "and", positions_sections_df[i+1,]$section)
      positions_sections_df[i,]$section<-new_section
      positions_sections_df<-positions_sections_df[-(i+1),]
      return(merging_section(positions_sections_df))
    }}
  return(positions_sections_df)
}

clean_section_title <- function(positions_sections_df) {
  #this function just pass to lower to help the extractions functions
  #for example, "aterial" cannot be find in "MATERIAL".

  positions_sections_df$section<-as.character(positions_sections_df$section)
  positions_sections_df$section<-tolower(positions_sections_df$section)
  return(positions_sections_df)
}

extract_material_and_method_section <- function(x, positions_sections_df) {
  #This function is a refunt from the previous one.
  #The problem was that the previous function to extract the material and methods was looking inside sections
  #for something named "Materials" or "Material", etc.
  #This introduce a problem with the merging function that combine close section into a section "X and Y"
  #Material and Methods would not be match, or it would require to wrote all the possibility in advance
  #It was also quite bug prone, and would require to think ahead all combinaisonm especially to extend this
  #function to result.
  positions_sections_df<-clean_section_title(positions_sections_df)
  for (i in 1:(length(positions_sections_df$section)-1)){
    if (grepl("material", positions_sections_df$section[i])){ 
      idx<-i
      break
    }
    if (grepl("method", positions_sections_df$section[i])){
      idx<-i
      break
    }
  }
  beginning_section<-positions_sections_df[idx,]$occurrences
  end_section<-positions_sections_df[idx+1,]$occurrences
  material_and_method_section<-x[beginning_section:(end_section-1),]
  return(material_and_method_section)
}

capitalize_first_letter <- function(section) {
  #https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
  #"MAterIAls"->"Materials" #
  #use to correct strange behavior of tabulizer for section title
  #must be used on x$token !
  section<-paste0(toupper(substring(section, 1,1)), tolower(substring(section, 2)))
  return(section)
}

locate_sections_position_debug<- function(x, section_title_df){
  ##### DEBUG VERSION
  ##########
  
  positions_sections_df<-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("section", "occurrences"))
  
  for (section in section_title_df$Word){
    occurrences<-which(x$token %in% section)
    print(section)
    print(occurrences)
    if (length(occurrences)>1){ #if several time the section name in the article
      occurrences<-subset_occurrences(occurrences, positions_sections_df)}
    print(occurrences)
    if (length(occurrences)==0) {#if there no hits anym because tabulizer funny caps in section title
      occurrences<-which(capitalize_first_letter(x$token) %in% section) #replace by a more elaborate function ?
      #to lower but not for first letter
    }
    print(occurrences)
    occurrences<-reduce_occurrences_debug(x, occurrences, positions_sections_df, section_title_df)
    print(occurrences)
    positions_sections_df<-rbind(positions_sections_df, data.frame(section, occurrences))
  }
  return(merging_section(positions_sections_df))}

reduce_occurrences_debug<- function(x, occurrences, positions_sections_df, section_title_df){
  ### DEBUG
  print("entering reduce reduce_occurrences_debug")
  if (length(occurrences)>1){ #if several time the section name in the article
    occurrences<-subset_occurrences(occurrences, positions_sections_df)}
  print(occurrences)
  if (length(occurrences)>1){ #if there is still several time the section name in the article
    occurrences_NLP<-NLP_filter_section_title(x, occurrences)
    if (length(occurrences_NLP)>0){ #if not NULL, like for Methods in Materials and Methods
      occurrences<-occurrences_NLP}}
  print(occurrences)
  if (length(occurrences)>1){ #if there is still several time the section name in the article
    occurrences<-NLP_filter_second_debug(x, occurrences, section_title_df)}
  print(occurrences)
  return(occurrences)}

NLP_filter_second_debug<- function(x, occurrences){
  #this function return the first occurrence passed as input which is the first lemma of a sentence. 
  print("Entering NLP second filter debug")
  for (index in occurrences){
    if (filter_association_first_token_debug(x, index)){return(index)}
  }}

filter_association_first_token_debug<- function(x, index){
  print("filter_association_first_token_debug")
  occurrence<-x[index,] #x[index,], where x is the dataframe of annotation generated by udpipe
  print(occurrence)
  sentence_id<-occurrence$sentence_id
  #the following line query the first lemma of the sentence in the good document
  first_token<-x[which(x$sentence_id==sentence_id)[1],]$token
  if (first_token %in% section_title_df$Word) {
    print(first_token)
    return(TRUE)}
  if (capitalize_first_letter(first_token) %in% section_title_df$Word) {
    print(capitalize_first_letter(first_token))
    return(TRUE)} 
  return(FALSE)
}

# 
#####

#pdf_name<-"Abrams, M T et al 2010.pdf"  #check, passed with tabulizer
#pdf_name<-"Al Faraj A, Fauvelle F et al 2011.pdf" #bug at conclusion
#pdf_name<-"Al Zaki, A et al 2015.pdf" #check, work with if/tolower
pdf_name<-"Al-Bairuty, G et al 2013.pdf"

txt_pdf <-tabulizer::extract_text(pdf_name) #read the text from the pdf

x<-annotate_txt_pdf(txt_pdf)   #create the dataframe for NLP using udpipe


#read the output from poppler and create the dataframe with words, font and fontsize
df_poppler<-read_outpout_poppler(pdf_name)

#identify the font of the section, first by looking at references and then at Acknowledgement
font_section<-identify_font(df_poppler)

#the sections what the script will try to identify in the doppler output
list_of_sections <- list(c("Introduction", "INTRODUCTION"),
                         c("Materials", "Material", "materials", "material", "MATERIALS", "MATERIAL"),
                         c("Methods", "Method", "methods", "method", "METHODS", "METHOD"),
                         c("Acknowledgements", "Acknowledgments", "ACKNOWLEDGEMENTS", "ACKNOWLDGEMENTS"),
                         c("References", "REFERENCES"),
                         c("Results", "RESULTS"),
                         c("Discussion", "DISCUSSION"),
                         c("Abstract", "ABSTRACT"),
                         c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS"),
                         c("Background", "BACKGROUND")
)

#dataframe with Section name (word), font of the section, size of the of the font inside the poppler documents
section_title_df<-create_section_title_df(font_section, list_of_sections)

#dataframe with the Sections title in order of appereance in the article, and their position in x
#positions_sections_df<-locate_sections_position(x, section_title_df)
positions_sections_df<-locate_sections_position_debug(x, section_title_df)

material_and_method_section<-extract_material_and_method_section(x, positions_sections_df)

saveRDS(material_and_method_section, file = paste0("Material_and_Methods_Section/" , paste0(pdf_name, ".rds")))


print(head(unique(material_and_method_section$sentence), 15))
print(tail(unique(material_and_method_section$sentence), 15))

##########

# pdf_name<-"Abrams, M T et al 2010.pdf"  #check, passed with tabulizer
# pdf_name<-"Al Faraj A, Fauvelle F et al 2011.pdf" #bug at conclusion
# pdf_name<-"Al Zaki, A et al 2015.pdf" #check, work with if/tolower

jgc <- function(){
  .jcall("java/lang/System", method = "gc")
}    

pdf_list<- c("Abrams, M T et al 2010.pdf", "Al Faraj A, Fauvelle F et al 2011.pdf",
             "Al Zaki, A et al 2015.pdf",
             "Al-Bairuty, G et al 2013.pdf",
             "An, W et al 2017.pdf")

extract_material_and_methods <- function(pdf_name) {
  
  #txt_pdf <-tabulizer::extract_text(pdf_name) #read the text from the pdf
  txt_pdf <-extract_text(pdf_name)
  x<-annotate_txt_pdf(txt_pdf)   #create the dataframe for NLP using udpipe

  #read the output from poppler and create the dataframe with words, font and fontsize
  df_poppler<-read_outpout_poppler(pdf_name)

  #identify the font of the section, first by looking at references and then at Acknowledgement
  font_section<-identify_font(df_poppler)

  #the sections what the script will try to identify in the doppler output
  list_of_sections <- list(c("Introduction", "INTRODUCTION"),
                           c("Materials", "Material", "materials", "material", "MATERIALS", "MATERIAL"),
                           c("Methods", "Method", "methods", "method", "METHODS", "METHOD"),
                           c("Acknowledgements", "Acknowledgments", "ACKNOWLEDGEMENTS", "ACKNOWLDGEMENTS"),
                           c("References", "REFERENCES"),
                           c("Results", "RESULTS"),
                           c("Discussion", "DISCUSSION"),
                           c("Abstract", "ABSTRACT"),
                           c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS"),
                           c("Background", "BACKGROUND")
  )

  section_title_df<-create_section_title_df(font_section, list_of_sections)
  print(section_title_df)
  positions_sections_df<-locate_sections_position(x, section_title_df)
  print(positions_sections_df)
  material_and_method_section<-extract_material_and_method_section(x, positions_sections_df)
  #name<-strsplit(string, "/" )[[1]] #seriously R ?
  #saveRDS(material_section, file = paste0("Material_and_Methods_Section/" , paste0(name[3], ".rds")))
  saveRDS(material_and_method_section, file = paste0("Material_and_Methods_Section/" , paste0(pdf_name, ".rds")))
  print(head(unique(material_and_method_section$sentence), 15))
  print(tail(unique(material_and_method_section$sentence), 15))
  }

run_tests <- function(pdf_list) {
  for (pdf_name in pdf_list) {
    jgc()
    print(pdf_name)
    extract_material_and_methods(pdf_name)
  }}

run_tests(pdf_list)

