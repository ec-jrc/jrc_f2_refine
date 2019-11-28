library(udpipe)
library(dplyr)
library(readr)
library(Rpoppler)
library(stringr)

#for pdf/apply pdf
#require "Abrams, M T et al 2010.pdf"
#require "Abrams, M T et al 2010.pdf.output_poppler.txt"


pdf_name<-"Abrams, M T et al 2010.pdf"

txt_pdf <- PDF_text(pdf_name) #works very well
poppler_output <- read_lines(paste0(pdf_name, ".output_poppler.txt"))

#this command remove the lines that indicate change of page ("page 1/5") and the "----"
poppler_output<-poppler_output[-which(nchar(poppler_output)<12)]

annotate_txt_pdf<- function(txt_pdf) {
  #This function create the adequate NLP datastructure from the text of the pdf
  txt_pdf<-paste(txt_pdf, collapse = '') 
  #the collapse argument does the trick, otherwise you got pages in between
  #https://stackoverflow.com/questions/2098368/concatenate-a-vector-of-strings-character
  #Load the model to do the annotation of the text.
  ud_model_gum <- udpipe_load_model(file = "english-gum-ud-2.4-190531.udpipe" )
  #x is the standard name of the dataframe in the documentation of udpipe
  #annotate the txt file
  x <- udpipe_annotate(ud_model_gum, x = txt_pdf)
  x <- as.data.frame(x)
  return(x)
}

x<-annotate_txt_pdf(txt_pdf)

#### Poppler reading

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

read_outpout_poppler <- function(poppler_output) {
  #read the poppler_output
  #read one line of poppler_output and extract word, font, size of the font
  #data is one line of the the poppler_output variable
  word<-extract_word(poppler_output)
  font<-extract_font(poppler_output)
  font_size<-extract_font_size(poppler_output)
  
  df_local<-as.data.frame(matrix(list(word, font, font_size), ncol=3, byrow=TRUE))
  colnames(df_local)<-c("Word", "Font", "Size")
  #this line add the dataframe to a global dataframe
  #please pay attention to the use of the "<<-" 
  df_poppler<<-rbind(df_poppler, df_local)
  return()
}


#this dataframe is populated or used as a global variable in other function
#read_output_poppler, 
df_poppler<-data.frame()

#there is no direct output to the following line
#inside the sapply the elements are added to the df_poppler dataframe df declared above

sapply(poppler_output, read_outpout_poppler) #not direct output

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

df_poppler<-repair_df(df_poppler)
sections_titles<-data.frame()

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
font_section<-identify_font(df_poppler)

find_section_titles <- function(vector_title, font_section) {
  assumed_title_df<-df_poppler[which(df_poppler$Word %in% vector_title),]
  assumed_title_df<-assumed_title_df[which(assumed_title_df$Size==max(assumed_title_df$Size)),]
  if (assumed_title_df$Font==font_section){
    return(assumed_title_df)
  }
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

list_of_sections <- list(c("Introduction"), 
                         c("Materials", "Material"),
                         c("Acknowledgements", "Acknowledgments"),
                         c("References"),
                         c("Results"),
                         c("Discussion")
                         )

section_title_df<-create_section_title_df(font_section, list_of_sections)

##########
filter_first_lemma <- function(index){
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

NLP_filter_section_title <- function(occurrences){
  #this function return the first occurrence passed as input which is the first lemma of a sentence. 
  #Typically, it distinguish between sentence "northen blot as discribe in Materials and Methods" and 
  #sentence like "Materials and Methods", the second one beeing the materials and methods section.
  #Please refer to the rmarkdown documents Materials_methods_EDA_gum.html and EDA_part2.html for more details.
  for (index in occurrences){
    if (filter_first_lemma(index)){return(index)}
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

reduce_occurrences<- function(occurrences, positions_sections_df){
  if (length(occurrences)>1){ #if several time the section name in the article
    occurrences<-subset_occurrences(occurrences, positions_sections_df)}
  if (length(occurrences)>1){ #if there is still several time the section name in the article
    occurrences<-NLP_filter_section_title(occurrences)}
  return(occurrences)}

locate_sections_position <- function(section_title_df){
  #this function create and return the a dataframe with the name of the section and it start position inside x
  #reduce_occurrences() use the order of the sections inside the document and NLP approach to reduce the number
  #of occurrences to one, i.e. to select among the different occurrences of a section title which one 
  #correspond to the section title. More description in them. For below :
  #https://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop/13442634
  
  positions_sections_df<-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("section", "occurrences"))
  
  for (section in section_title_df$Word){
    occurrences<-which(x$token==section)
    occurrences<-reduce_occurrences(occurrences, positions_sections_df)
    positions_sections_df<-rbind(positions_sections_df, data.frame(section, occurrences))
  }
  return(positions_sections_df)}


positions_sections_df<-locate_sections_position(section_title_df)


##Extract automatically material and or method until next section 