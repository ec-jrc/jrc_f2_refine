library(udpipe)
library(dplyr)
library(readr)
library(stringr)
library(tabulizer)
library(tm)

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
  #addition of capitalize() to be able to identify "REFERENCES"
  
  font_sections<-"void" #so the fonction can send something back if there is nothing
                        #after the function will rerun reidentify font if there is a problem
  
  reference_df<-df_poppler[which(capitalize_first_letter(df_poppler$Word) %in% c("References")),]
  ack_df<-df_poppler[which(capitalize_first_letter(df_poppler$Word) %in% 
                             c("Acknowledgements", "Acknowledgments",
                               "Acknowledgement", "Acknowledgment")),]
  
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

find_section_titles <- function(vector_title, font_section, df_poppler) {
  
    assumed_title_df<-df_poppler[which(df_poppler$Word %in% vector_title),]
    
    if (dim(assumed_title_df)[1] > 0) { #if section exist #select the word with max size
      assumed_title_df<-assumed_title_df[which(assumed_title_df$Size==max(assumed_title_df$Size)),]
      
      if (dim(assumed_title_df)[1] > 1) {##if there is several words with same max size #which has font section
        assumed_title_df<-assumed_title_df[which(assumed_title_df$Font==font_section),]}
      
      if (dim(assumed_title_df)[1] > 0){#if there were indeed a section title
        if (assumed_title_df$Font == font_section){
          return(assumed_title_df)
        }}
      
      if (dim(assumed_title_df)[1] == 0){ #if there is nothing, retry but with the font of text removed
        clean_df_poppler<-clean_font_txt(df_poppler)
        rm(df_poppler)
        df_poppler<-clean_df_poppler
        print("***** Clean_font_txt_() has been called in find_section_titles *****")
        return(find_section_titles(vector_title, font_section, df_poppler))
        
      }}
}

create_section_title_df <- function(font_section, list_of_sections, df_poppler) {
  #create and return a dataframe with the section, their font and the size of the font like this :
  #Word                         Font                 Size
  #293     Introduction VMUQDX+ITCStoneSans-Semibold 10.0
  #1321         Results VMUQDX+ITCStoneSans-Semibold 10.0
  section_title_df<-data.frame()
  for (vector_title in list_of_sections) {
    section_title_df<-rbind(section_title_df, find_section_titles(vector_title, font_section, df_poppler))
  }
  
  section_title_df <- re_identify_font_section(df_poppler, section_title_df, list_of_sections)
  section_title_df <- section_title_df[order(as.numeric(row.names(section_title_df))),]
  return(section_title_df)}

filter_first_lemma <- function(x, index){
  #DEPRECATED
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

filter_association_first_token <- function(x, index, positions_sections_df){
  #update tabulizer : tolower can lead to unexpected results
  #doubling the if is a quick and safe fix
  #this function looks if the first token is already in section_title_df
  #x[index,] return a token and all the associated data : lemma, but also sentence and doc_id
  occurrence<-x[index,] #x[index,], where x is the dataframe of annotation generated by udpipe
  token_id<-occurrence$token_id
  token_id<-as.numeric(token_id)
  sentence_id<-occurrence$sentence_id
  first_tokens<-x[which(x$sentence_id==sentence_id)[1:(token_id-1)],]$token
  for (token in first_tokens) {
    if (token %in% positions_sections_df$section) {
      return(TRUE)}
    if (capitalize_first_letter(token) %in% positions_sections_df$section) {
      return(TRUE)}}
  return(FALSE)
}

NLP_filter_second_member_section_title <- function(x, occurrences, positions_sections_df){
  #this function return the first occurrence passed as input for which the first token of the sentence
  #already appear in the dataframe section_title_df
  #Typically, if there is a Material and Methods section, and several time the words methods in the article,
  #The others functions cannot distinguished which occurrence to select (since it is not the first lemma of a sentence)
  #Please refer to the rmarkdown documents Materials_methods_EDA_gum.html and EDA_part2.html for more details.
  for (index in occurrences){
    if (filter_association_first_token(x, index, positions_sections_df)){return(index)}
  }}

NLP_filter_section_title <- function(x, occurrences){
  #this function return the first occurrence passed as input which is the first lemma of a sentence. 
  #Typically, it distinguish between sentence "northen blot as discribe in Materials and Methods" and 
  #sentence like "Materials and Methods", the second one beeing the materials and methods section.
  #Please refer to the rmarkdown documents Materials_methods_EDA_gum.html and EDA_part2.html for more details.
  for (index in occurrences){
    #if (filter_first_lemma(x, index)){return(index) #Old
    if (recursive_filter_first_lemma(x, index, lemma_nb=1)){return(index)}
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
    occurrences_NLP<-NLP_filter_second_member_section_title(x, occurrences, positions_sections_df)
    if (length(occurrences_NLP)>0){ #if not NULL, like for Methods in Materials and Methods
      occurrences<-occurrences_NLP}}
  if (length(occurrences)>1){ #if there is still several time the section name in the article
    occurrences_NLP<-NLP_filter_section_title(x, occurrences)
    if (length(occurrences_NLP)>0){ #if not NULL, like for Methods in Materials and Methods
      occurrences<-occurrences_NLP}}
  # if (length(occurrences)>1){ #if there is still several time the section name in the article
  #   occurrences<-NLP_filter_second_member_section_title(x, occurrences, section_title_df)}
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

  for (section in section_title_df$Word) {
    occurrences<-which(lower_but_first_letter(x$token) %in% section)
    if (length(occurrences)==0){ #if several time the section name in the article
      occurrences<-which(lower_but_first_letter(x$token) %in% lower_but_first_letter(section))} 
    occurrences<-eliminate_cf_occurrences(x, occurrences)
    occurrences<-missing_first_letter_section(x, section, occurrences)
    occurrences<-subset_occurrences(occurrences, positions_sections_df)
    occurrences<-handle_typos(x, section, occurrences)
    occurrences<-is_summary_box(x, section, occurrences, section_title_df)
    if (length(occurrences)>1){ #if several time the section name in the article
      occurrences<-subset_occurrences(occurrences, positions_sections_df)}
    if (length(occurrences)>1){ #if several time the section name in the article
      occurrences<-reduce_occurrences(x, occurrences, positions_sections_df, section_title_df)}
    positions_sections_df<-rbind(positions_sections_df, data.frame(section, occurrences))
    positions_sections_df<-multiple_section_fix(positions_sections_df)
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
  #for (i in 1:(length(positions_sections_df$section)-1)){
  for (i in 1:(length(positions_sections_df$section))){
    if (grepl("material", positions_sections_df$section[i])){ 
      idx<-i
      break
    }
    if (grepl("method", positions_sections_df$section[i])){
      idx<-i
      break
    }
    if (grepl("experimental", positions_sections_df$section[i])){
      idx<-i
      break
    }
    if (grepl("experiment", positions_sections_df$section[i])){
      idx<-i
      break
    }
    if (grepl("methodology", positions_sections_df$section[i])){
      idx<-i
      break
    }
  }
  beginning_section<-positions_sections_df[idx,]$occurrences
  
  if (length(positions_sections_df$occurrences)>=idx+1) {
    end_section<-positions_sections_df[idx+1,]$occurrences
  } else { #if the methods section is the last one of the article, just go until the end of x
    end_section<-dim(x)[1]
  }
  material_and_method_section<-x[beginning_section:(end_section-1),]
  return(material_and_method_section)
}

capitalize_first_letter <- function(section) {
  #https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
  #"MAterIAls"->"Materials" 
  #use to correct strange behavior of tabulizer for section title
  #must be used on x$token !
  section<-paste0(toupper(substring(section, 1,1)), tolower(substring(section, 2)))
  return(section)
}

Elsevier_correction <- function(x, section) {
  #in one of article of Elsevier, "Al-Bairuty, G et al 2013.pdf"
  #"Acknowledgements" became "cknowledgements", and "Reference", "eference"
  #but this only occurre in the NLP data structure (UDpipe)
  section<-tolower(substring(section, 2))
  occurrences<-which(x$token %in% section)
  if (length(occurrences)>0){ #send back only if it exist
    return(occurrences)
  }}

missing_first_letter_section<- function(x, section, occurrences) {
  # In one of article of Elsevier, "Al-Bairuty, G et al 2013.pdf"
  #"Acknowledgements" became "cknowledgements", and "Reference", "eference"
  # This only occurre in the NLP data structure (UDpipe)
  # This function send back truncated name of the section title, to find the tokens that corresponding to it.
  # Then if there is occurrences for truncated version, send back to the main functions the new occurrences.
  
  section<-tolower(substring(section, 2))
  new_occurrences<-which(x$token %in% section)
  if (length(new_occurrences)>0){ #send back only if it exist
    occurrences<-c(occurrences, new_occurrences)
    return(occurrences)}
  return(occurrences)
}

regex_correction <- function(x, section) {
  #in "Attia, AB et al 2013.pdf"
  #"Acknowledgements" became "group.Acknowledgments" 
  #but this only occurre in the NLP data structure (UDpipe)
  
  #something to look for section inside tokens
  #section = "Acknowledgments"
  #which token has a section inside him
  occurrences<-which(!is.na(str_extract((x$token), section)))
  if (length(occurrences)>0){ #send back only if it exist
    return(occurrences)}}

clean_font_txt <- function(df_poppler) {
  #Baker, G L et al 2008.pdf" show that the word of the section can have a size smaller than the text
  #Probably because for differents scale is not the same
  #Following line make a frequency of the fonts :
  
  fonts<-as.data.frame(table(df_poppler$Font)) #dataframe of fonts freq
  #Fond the most abundant font, the one of the text
  font_text<-fonts$Var1[which(fonts$Freq==max(fonts$Freq))] #font the most used
  clean_df_poppler<-df_poppler[-which(df_poppler$Font==font_text),]
  return(clean_df_poppler)
}

is_summary_box <- function(x, section, occurrences, section_title_df) {
  # "Berce, C et al 2016.pdf" show a problematic case when there is the a short summary in a box at the beginning
  # of the article with sections names. For similar script can perform the extraction of the section without any
  # problems, because the introduction is after this little box of summary and then the script look for the other
  # section title only after the introduction and the little box is ignored.
  # The goal of this function is to check if there is the signature of the a summary box
  
  if (section == "Introduction" & length(occurrences)>1) {
  
    putative_summary_box<-x[occurrences[1]:occurrences[2],]
    
    occur_conclusion<-which(capitalize_first_letter(putative_summary_box$token) %in% 
                              c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS"))
  
    if (length(occur_conclusion)==1 ) {
      if(putative_summary_box[occur_conclusion+1,]$token == ":" |
          putative_summary_box[occur_conclusion+1,]$token == ":"){
          occurrences<-occurrences[2]
          print("***** is_summary_box() has been called ****")
          return(occurrences)}
      }
  }
  return(occurrences)
  }

recursive_filter_first_lemma <- function(x, index, lemma_nb){
  #lemma_nb is 1 for the first call
  #If =1, it correspond to a normal call of filter_first_lemma()
  #print("****** ****recursive****_filter_first_lemma_debug ********")
  occurrence<-x[index,] #x[index,], where x is the dataframe of annotation generated by udpipe
  lemma<-x[index,]$lemma 
  sentence_id<-occurrence$sentence_id
  first_lemma<-x[which(x$sentence_id==sentence_id)[lemma_nb],]$lemma
  if (str_detect(first_lemma, "\\d")){
    lemma_nb<-lemma_nb+1
    return(recursive_filter_first_lemma(x, index, lemma_nb))
  }
  if (str_detect(first_lemma, "[:punct:]")){
    lemma_nb<-lemma_nb+1
    return(recursive_filter_first_lemma(x, index, lemma_nb))
  }
  if (str_detect(capitalize_first_letter(first_lemma), "Animals|Animal")){ #"De Jong, WH et al 2008.pdf"
    lemma_nb<-lemma_nb+1
    return(recursive_filter_first_lemma(x, index, lemma_nb))
  }
  if (first_lemma==lemma) {
    return(TRUE)
  }
  return(FALSE)
}

re_identify_font_section <- function(df_poppler, section_title_df, list_of_sections) {
  #correction made for "Campagnolo, L et al 2013.pdf"
  #When Abstract and references has a different font than the other section
  #This function look if the section_df is too small, and restart with the font of Introduction, and
  #merge the two datafrane
  
  if (length(section_title_df$Word)<=2) {
    introduction_df<-df_poppler[which(capitalize_first_letter(df_poppler$Word) %in% 
                                        c("Introduction")),]
    abstract_df<-df_poppler[which(capitalize_first_letter(df_poppler$Word) %in% 
                                        c("Abstract")),]
    background_df<-df_poppler[which(capitalize_first_letter(df_poppler$Word) %in% 
                                    c("Background")),]
    
    if (dim(introduction_df)[1]==1) {#if Introduction exist ONE time
      new_font_section<-introduction_df$Font
      #section_title_df<-create_section_title_df(font_section, list_of_sections, df_poppler)
      for (vector_title in list_of_sections) {
        section_title_df<-rbind(section_title_df, find_section_titles(vector_title, new_font_section, df_poppler))
      }
    } 
    if (dim(introduction_df)[1]!=1 & dim(abstract_df)[1]==1) {#if Introduction exist ONE time
      new_font_section<-abstract_df$Font
      #section_title_df<-create_section_title_df(font_section, list_of_sections, df_poppler)
      for (vector_title in list_of_sections) {
        section_title_df<-rbind(section_title_df, find_section_titles(vector_title, new_font_section, df_poppler))
      }
    } 
    if (dim(introduction_df)[1]!=1 & dim(abstract_df)[1]!=1 & dim(background_df)[1]==1) {#if Introduction exist ONE time
      new_font_section<-background_df$Font
      #section_title_df<-create_section_title_df(font_section, list_of_sections, df_poppler)
      for (vector_title in list_of_sections) {
        section_title_df<-rbind(section_title_df, find_section_titles(vector_title, new_font_section, df_poppler))
      }
    } 
    }
  return(section_title_df)
}

handle_typos <- function(x, section, occurrences) {
  # This function solve various problem encoutered in section title due to the conversion from pdf
  # NB : this problems only occur in the in the NLP data structure (UDpipe)
  # Please refer to each function for more details
  if (length(occurrences)==0){ #is first caps is missing
    #When "Acknowledgements" became "cknowledgements", and "Reference", "eference"
    occurrences<-Elsevier_correction(x, section)
  }
  if (length(occurrences)==0) {
    #"MAterIAls"->"Materials", correct strange behavior of tabulizer for section title
    occurrences<-which(capitalize_first_letter(x$token) %in% section)
  }
  if (length(occurrences)==0) {
    #in "Attia, AB et al 2013.pdf" "Acknowledgements" became "group.Acknowledgments" 
    occurrences<-regex_correction(x, section)
  }
  if (length(occurrences)==0) {
    print("warning in handle_typo()")
  }
  
  return(occurrences)
}

repair_txt <- function(txt_pdf) {
  #"To gather all these points, iodine-based nano-\nemulsions, have recently been developed showing huge
  #stabil-\nity, high biocompatibility and great potential in medical ap-\nplications, such as image-guided 
  #surgery, advanced diagnosis\n(e.g., to recognize tumor regions), personalized medicine or\ntheragnostics."
  
  #"To gather all these points, iodine-based nanoemulsions, have recently been developed showing huge stability,
  #high biocompatibility and great potential in medical applications, such as image-guided surgery, advanced 
  #diagnosis\n(e.g., to recognize tumor regions), personalized medicine or\ntheragnostics."
  txt_pdf<-gsub("\\b\\-\n", "", txt_pdf, perl=TRUE)
  
  #As shown with "Chung, E J et al 2015.pdf", the section title can be merge with the dot and the prevous section.
  #This problem was already partially addressed by regex_correction()
  #Here the section Result is not found because merge with the end of the previous section as following :
  #"significant.Results".
  #Instead of solving this issue in the NLP structure when the occurrences of the section is zero, 
  #This function repair the text to avoid any other downstrean trouble.
  #https://stackoverflow.com/questions/58936991/how-to-split-two-words-connected-by-a-dot-in-r
  #Example of the regex :
  #> txt<-"significant.Results"
  #> gsub("(?<=\\p{L})\\.(?=\\p{L})", ". ", txt, perl=TRUE)
  #[1] "significant. Results"
  
  txt_pdf<-gsub("\\b\\.\\b", ". ", txt_pdf, perl=TRUE)
  
  #"Elsabahy, M et al 2013.pdf" show a similar problem, but with a coma
  #Example of the regex :
  #"SCKs,Results and discussion" ->  "SCKs. Results"
  txt_pdf<-gsub("\\b\\,\\b", ", ", txt_pdf, perl=TRUE)
  #txt_pdf<-gsub("\\b[[:graph:]]\\b", ". ", txt_pdf, perl=TRUE)
  
  #https://stackoverflow.com/questions/26896971/add-space-between-two-letters-in-a-string-in-r
  #https://stringr.tidyverse.org/articles/regular-expressions.html
  #"methods2.1." -> "methods 2.1"
  #don't cut "2014"
  txt_pdf<- gsub("([A-z])([1-9])", "\\1 \\2", txt_pdf)
  
  #"31443144References" ->  "31443144 References"
  #"Yu, J et al 2014.pdf"
  txt_pdf<-gsub("([1-9])([A-z])", "\\1 \\2", txt_pdf, perl=TRUE)
  
  #remove non graphical caracter :
  #https://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
  txt_pdf<-str_replace_all(txt_pdf,"[^[:graph:]]", " ")
  
  #"Zhang, J et al 2013.pdf"
  #View Article OnlineResults and discussion Characterization of nanoTiO 2 As shown in Fig.
  txt_pdf<-str_replace_all(txt_pdf,"Online", "Online ")
  
  return(txt_pdf)
}

lower_but_first_letter<- function(token) {
  #"MAterIAls"->"Materials", correct strange behavior of tabulizer for section title
  #The goal is to lower all the letter BUT the first one
  #That way, looking for section titles inside token would still be discricimative
  #For example, the token MAterIAls would be find when looking for Material 
  #But avoid to lower case of capitalize the first letter for everything
  #Example :
  #token<-"MAterIAls"
  #first_letter<-substring(token, 1,1)
  #body_word<-tolower(substring(token, 2))
  #token<-paste0(first_letter, body_word)
  #> token
  #"Materials"
  
  first_letter<-substring(token, 1,1)
  body_word<-tolower(substring(token, 2))
  token<-paste0(first_letter, body_word)
  return(token)
}

clean_title_journal <- function(pdf_name, section_title_df) {
  #Guo, J et al 2014.pdf highlight a problem : the title of the journal is wrotte with the
  #same font that the sections titles
  #this function check if there is not a section repeated the same number of pages of the pdf or 
  #the same number of page menos 1
  res<-table(section_title_df$Word)
  res<-as.data.frame(res)
  
  nb_page<-tabulizer::get_n_pages(pdf_name)
  title_mistaken<-which(res$Freq==nb_page | res$Freq==(nb_page-1))
  
  if (length(title_mistaken)>0) { #if exist
    title_journal<-res[title_mistaken,]$Var1
    section_title_df<-section_title_df[-(which(section_title_df$Word==title_journal)),]
    print("############## clean_title_journal has been called")
  }
  return(section_title_df)
}

eliminate_cf_occurrences <- function(x, occurrences) {
  #This function remove the occurrences of section title name that are token preceeded by word like "see" or 
  #"cf". The function will probably grow over time.
  occurrences_without_cf<-c()
  for (occur in occurrences) {
    word_before<-x[(occur-1),]$token
    if (word_before=="see") {
      next
    }
    if (word_before=="cf") {
      next
    }
    if (word_before=="in" | word_before=="the") {
      head_token<-grep_head_token(x, occur)
      if (head_token=="discussed") {
        next
      }
      if (head_token=="described") {
        next
      }
    }
    #if not next addition to the occurrences without the 
    #entries that are just references to a section
    occurrences_without_cf<-c(occurrences_without_cf, occur)
  }
  occurrences<-occurrences_without_cf
  return(occurrences)
}

grep_head_token <- function(x, index){
  #catch the lemma corresponding to the head_token_id of the token at the entry "index" of x
  #x[index,] return a token and all the associated data : lemma, but also sentence and doc_id
  occurrence<-x[index,] #x[index,], where x is the dataframe of annotation generated by udpipe
  head_token_id<-occurrence$head_token_id
  head_token_id<-as.numeric(head_token_id)
  if (head_token_id==0) {
    lemma_head_token_id<-x[index,]$lemma
    return(lemma_head_token_id)
  }
  sentence_id<-occurrence$sentence_id
  #the following line query the lemma of the head_token_id based on the previous parameters
  lemma_head_token_id<-x[which(x$sentence_id==sentence_id)[head_token_id],]$token
  return(lemma_head_token_id)
}

ad_hoc_reorder <- function(section_title_df) {
  # in Smulders, S et al 2015.pdf the following problem when using poppler was encountered :
  # > section_title_df
  # Word                   Font   Size
  # 543      Introduction CLNCIJ+AdvOT18499c10.B 7.9702
  # 1089        Materials CLNCIJ+AdvOT18499c10.B 7.9702
  # 1091          Methods CLNCIJ+AdvOT18499c10.B 7.9702
  # 1787          Results CLNCIJ+AdvOT18499c10.B 7.9702
  # 2263       Discussion CLNCIJ+AdvOT18499c10.B 7.9702
  # 3447 Acknowledgements CLNCIJ+AdvOT18499c10.B 7.9702
  # 3449       Conclusion CLNCIJ+AdvOT18499c10.B 7.9702
  # 3624    Supplementary CLNCIJ+AdvOT18499c10.B 7.9702
  # 3642       References CLNCIJ+AdvOT18499c10.B 7.9702
  #
  #This lead to crash downstream since Conclusion is placed before for a human reader and for the tabulizer
  
  pos_conclusion<-which(capitalize_first_letter(section_title_df$Word) %in% c("Conclusion", "Conclusions"))
  pos_ack<-which(capitalize_first_letter(section_title_df$Word) %in% c("Acknowledgements", "Acknowledgments"))
  

  if (length(pos_conclusion)>0 & length(pos_ack>0)){
    if (pos_ack<pos_conclusion) {
    #it is swapping time
    section_title_df$Word[c(pos_conclusion,pos_ack)]=section_title_df$Word[c(pos_ack,pos_conclusion)]
    print("ad_hoc_reorder(section_title_df) has been called")
    }
  }
  return(section_title_df)
}

check_sections_df <- function(positions_sections_df) {
  df<-as.data.frame(table(positions_sections_df$section))
  if (max(df$Freq)>1) {
    print("ALERT Section in double here")
  }
}

remove_reference_section<- function(section_title_df) {
  #this function remove the references section and the one that came after in the section title df to avoid
  #crash due to looking for inexisting section
  idx<-which(lower_but_first_letter(section_title_df$Word) %in% c("References"))
  if (length(idx)==1) {
    section_title_df<-section_title_df[1:(idx-1),]
  }
  return(section_title_df)
}

remove_bibliography<- function(x, section_title_df) {

  occurrences<-which(lower_but_first_letter(x$token) %in% c("References"))
  
  # the following line is retaken from remove_reference_section() to ensure both are call at the same time
  # this function must be called before remove_reference_section()
  idx<-which(lower_but_first_letter(section_title_df$Word) %in% c("References"))
  
  if (length(occurrences)==1 & length(idx)==1) {
    x<-x[1:(occurrences+1),]
  }
  return(x)
}

multiple_section_fix <- function(positions_sections_df) {
  # > positions_sections_df
  # section occurrences
  # 1 Introduction         238
  # 2      Results        1091
  # 3   Discussion        4839
  # 4   Discussion        9786
  # 5   Discussion        9805
  #to
  # 1 Introduction         238
  # 2      Results        1091
  # 3   Discussion        4839
  
  df<-as.data.frame(table(positions_sections_df$section))
  if (max(df$Freq)>1) {
    new_size<-length(unique(positions_sections_df$section))
    positions_sections_df<-positions_sections_df[1:new_size,]
  }
  return(positions_sections_df)
}

extract_result_section <- function(x, positions_sections_df) {
  #Clone of the function extract material and method
  positions_sections_df<-clean_section_title(positions_sections_df)
  
  for (i in 1:(length(positions_sections_df$section))){
    if (grepl("result", positions_sections_df$section[i])){ 
      idx<-i
      break
    }
    if (grepl("results", positions_sections_df$section[i])){
      idx<-i
      break
    }
  }
  beginning_section<-positions_sections_df[idx,]$occurrences
  
  if (length(positions_sections_df$occurrences)>=idx+1) {
    end_section<-positions_sections_df[idx+1,]$occurrences
  } else { #if the methods section is the last one of the article, just go until the end of x
    end_section<-dim(x)[1]
  }
  result_section<-x[beginning_section:(end_section-1),]
  return(result_section)
}





# Debug func

locate_sections_position_debug<- function(x, section_title_df){
  ##### DEBUG VERSION
  ##########

  positions_sections_df<-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("section", "occurrences"))

  for (section in section_title_df$Word) {
    print("***** New section")
    print(section)
    occurrences<-which(lower_but_first_letter(x$token) %in% section)
    if (length(occurrences)==0){ #if several time the section name in the article
      occurrences<-which(lower_but_first_letter(x$token) %in% lower_but_first_letter(section))}
    print(occurrences)
    occurrences<-eliminate_cf_occurrences(x, occurrences)
    print(occurrences)
    occurrences<-missing_first_letter_section(x, section, occurrences)
    print("missing first letter section :")
    print(occurrences)
    occurrences<-subset_occurrences(occurrences, positions_sections_df)
    print("handle typo :")
    occurrences<-handle_typos(x, section, occurrences)
    print(occurrences)
    print("summary box :")
    occurrences<-is_summary_box(x, section, occurrences, section_title_df)
    print(occurrences)
    if (length(occurrences)>1){ #if several time the section name in the article
      occurrences<-subset_occurrences(occurrences, positions_sections_df)}
    print(occurrences)
    if (length(occurrences)>1){ #if several time the section name in the article
      occurrences<-reduce_occurrences_debug(x, occurrences, positions_sections_df, section_title_df)}
    print(occurrences)
    positions_sections_df<-rbind(positions_sections_df, data.frame(section, occurrences))
    positions_sections_df<-multiple_section_fix(positions_sections_df)
  }
  return(merging_section(positions_sections_df))}

reduce_occurrences_debug<- function(x, occurrences, positions_sections_df, section_title_df){
  ### DEBUG
  print("entering reduce_occurrences_debug")
  if (length(occurrences)>1){ #if several time the section name in the article
    occurrences<-subset_occurrences(occurrences, positions_sections_df)}
  print(occurrences)
  if (length(occurrences)>1){ #if there is still several time the section name in the article
    occurrences_NLP<-NLP_filter_second_debug(x, occurrences, positions_sections_df)
    if (length(occurrences_NLP)>0){ #if not NULL, like for Methods in Materials and Methods
    occurrences<-occurrences_NLP}}
  if (length(occurrences)>1){ #if there is still several time the section name in the article
    occurrences_NLP<-NLP_filter_section_title(x, occurrences)
    print("NLP_filter_section_title")
    print(occurrences_NLP)
    if (length(occurrences_NLP)>0){ #if not NULL, like for Methods in Materials and Methods
      occurrences<-occurrences_NLP}}
  print(occurrences)
  # if (length(occurrences)>1){ #if there is still several time the section name in the article
  #   occurrences<-NLP_filter_second_debug(x, occurrences, section_title_df)}
  print(occurrences)
  return(occurrences)}

NLP_filter_second_debug<- function(x, occurrences, positions_sections_df){
  #this function return the first occurrence passed as input which is the first lemma of a sentence.
  print("Entering NLP second filter debug")
  for (index in occurrences){
    if (filter_association_first_token_debug(x, index, positions_sections_df)){return(index)}
  }}

filter_association_first_token_debug<- function(x, index, positions_sections_df){
  print("filter_association_first_token_debug")
  occurrence<-x[index,] #x[index,], where x is the dataframe of annotation generated by udpipe
  token_id<-occurrence$token_id
  token_id<-as.numeric(token_id)
  print(occurrence)
  sentence_id<-occurrence$sentence_id
  #the following line query previous token in the sentencd
  first_tokens<-x[which(x$sentence_id==sentence_id)[1:(token_id-1)],]$token
  print("first tokens :")
  print(first_tokens)
  for (token in first_tokens) {
    print(token)
    if (token %in% positions_sections_df$section) {
      print("first condition is true")
      return(TRUE)}
    if (capitalize_first_letter(token) %in% positions_sections_df$section) {
      print("second condition is true")
      return(TRUE)}}
  return(FALSE)
}

create_section_title_df_debug <- function(font_section, list_of_sections, df_poppler) {
  #create and return a dataframe with the section, their font and the size of the font like this :
  #Word                         Font                 Size
  #293     Introduction VMUQDX+ITCStoneSans-Semibold 10.0
  #1321         Results VMUQDX+ITCStoneSans-Semibold 10.0
  section_title_df<-data.frame()
  print("Font_section :")
  print(font_section)
  for (vector_title in list_of_sections) {
    print(vector_title)
    section_title_df<-rbind(section_title_df, find_section_titles_debug(vector_title, font_section, df_poppler))
  }
  section_title_df <- re_identify_font_section(df_poppler, section_title_df, list_of_sections)
  section_title_df <- section_title_df[order(as.numeric(row.names(section_title_df))),]
  return(section_title_df)}

find_section_titles_debug <- function(vector_title, font_section, df_poppler) {

  assumed_title_df<-df_poppler[which(df_poppler$Word %in% vector_title),]
  print("***** Entering find_section_titles_debug *****")
  print(assumed_title_df)

  #fonts<-as.data.frame(table(df_poppler$Font)) #dataframe of fonts freq
  #Fond the most abundant font, the one of the text
  #font_text<-fonts$Var1[which(fonts$Freq==max(fonts$Freq))] #font the most used
  #assumed_title_df<-assumed_title_df[which(!assumed_title_df$Font==font_text),]
  if (dim(assumed_title_df)[1] > 0) { #if section exist #select the word with max size
    assumed_title_df<-assumed_title_df[which(assumed_title_df$Size==max(assumed_title_df$Size)),]
    print(assumed_title_df)

    if (dim(assumed_title_df)[1] > 1) { #if there is several words with same max size #which has font section
      assumed_title_df<-assumed_title_df[which(assumed_title_df$Font==font_section),]
      print(assumed_title_df)}

    if (dim(assumed_title_df)[1] > 0){ #if there were indeed a section title
      if (assumed_title_df$Font == font_section){
        print(assumed_title_df)
        return(assumed_title_df)
      }

      }

    if (dim(assumed_title_df)[1] == 0){ #if there is nothing, retry but with the font of text removed
      clean_df_poppler<-clean_font_txt(df_poppler)
      rm(df_poppler)
      df_poppler<-clean_df_poppler
      print("recursive call")
      return(find_section_titles_debug(vector_title, font_section, df_poppler))
    }
    }
}


#######


#pdf_name<-"Abrams, M T et al 2010.pdf"

pdf_name<-"Jensen, A I et al 2017.pdf"

txt_pdf <- tabulizer::extract_text(pdf_name) #read the text from the pdf
txt_pdf <- repair_txt(txt_pdf)

x<-annotate_txt_pdf(txt_pdf)   #create the dataframe for NLP using udpipe

#read the output from poppler and create the dataframe with words, font and fontsize
df_poppler<-read_outpout_poppler(pdf_name)

#identify the font of the section, first by looking at references and then at Acknowledgement
font_section<-identify_font(df_poppler)

#the sections what the script will try to identify in the doppler output
list_of_sections <- list(c("Introduction", "INTRODUCTION"),
                         c("Materials", "Material", "materials", "material", "MATERIALS", "MATERIAL"),
                         c("Methods", "Method", "methods", "method", "METHODS", "METHOD"),
                         c("Acknowledgements", "Acknowledgments", "ACKNOWLEDGEMENTS", "ACKNOWLEDGMENTS",
                           "Acknowledgement", "Acknowledgment", "ACKNOWLEDGEMENT", "ACKNOWLEDGMENT"),
                         c("References", "REFERENCES"),
                         c("Results", "RESULTS"),
                         c("Discussion", "DISCUSSION", "discussion"),
                         c("Abstract", "ABSTRACT"),
                         c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS"),
                         c("Background", "BACKGROUND"),
                         c("Experimental", "EXPERIMENTAL", "Experiment"), #Experiment :Yu, Z et al 2013.pdf
                         c("Supplementary", "SUPPLEMENTARY"),
                         c("Methodology"), #"Meng, H et al 2007.pdf"
                         c("Appendix"),
                         c("Section", "SECTION")
)

#dataframe with Section name (word), font of the section, size of the of the font inside the poppler documents

df_poppler<-clean_font_txt(df_poppler)

#section_title_df<-create_section_title_df_debug(font_section, list_of_sections, df_poppler)
section_title_df<-create_section_title_df(font_section, list_of_sections, df_poppler)
section_title_df<-clean_title_journal(pdf_name, section_title_df)
section_title_df<-ad_hoc_reorder(section_title_df)


#dataframe with the Sections title in order of appereance in the article, and their position in x
x<-remove_bibliography(x, section_title_df)
section_title_df<-remove_reference_section(section_title_df)

#positions_sections_df<-locate_sections_position_debug(x, section_title_df)
positions_sections_df<-locate_sections_position(x, section_title_df)
check_sections_df(positions_sections_df)

material_and_method_section<-extract_material_and_method_section(x, positions_sections_df)
result_section<-extract_result_section(x, positions_sections_df)

#write.table(unique(result_section$sentence), "Result_Elgharabawy, R M et al 2018.txt", append = FALSE, sep = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
#saveRDS(material_and_method_section, file = paste0("Material_and_Methods_Section/" , paste0(pdf_name, ".rds")))


print(head(unique(material_and_method_section$sentence), 10))
print(tail(unique(material_and_method_section$sentence), 10))

##########



pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)

#pdf_list<-rev(pdf_list)

extract_material_methods <- function(pdf_name) {
  
  #txt_pdf <-tabulizer::extract_text(pdf_name) #read the text from the pdf
  txt_pdf <- extract_text(pdf_name)
  txt_pdf <- repair_txt(txt_pdf)
  
  x<-annotate_txt_pdf(txt_pdf)   #create the dataframe for NLP using udpipe
  
  #read the output from poppler and create the dataframe with words, font and fontsize
  df_poppler<-read_outpout_poppler(pdf_name)
  
  #identify the font of the section, first by looking at references and then at Acknowledgement
  font_section<-identify_font(df_poppler)
  
  #the sections what the script will try to identify in the doppler output
  list_of_sections <- list(c("Introduction", "INTRODUCTION"),
                           c("Materials", "Material", "materials", "material", "MATERIALS", "MATERIAL"),
                           c("Methods", "Method", "methods", "method", "METHODS", "METHOD"),
                           c("Acknowledgements", "Acknowledgments", "ACKNOWLEDGEMENTS", "ACKNOWLEDGMENTS",
                             "Acknowledgement", "Acknowledgment", "ACKNOWLEDGEMENT", "ACKNOWLEDGMENT"),
                           c("References", "REFERENCES"),
                           c("Results", "RESULTS"),
                           c("Discussion", "DISCUSSION", "discussion"),
                           c("Abstract", "ABSTRACT"),
                           c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS"),
                           c("Background", "BACKGROUND"),
                           c("Experimental", "EXPERIMENTAL", "Experiment"), #Experiment :Yu, Z et al 2013.pdf
                           c("Supplementary", "SUPPLEMENTARY"),
                           c("Methodology"), #"Meng, H et al 2007.pdf"
                           c("Appendix"),
                           c("Section", "SECTION")
  )
  
  
  df_poppler<-clean_font_txt(df_poppler)
  section_title_df<-create_section_title_df(font_section, list_of_sections, df_poppler)
  section_title_df<-clean_title_journal(pdf_name, section_title_df)
  section_title_df<-ad_hoc_reorder(section_title_df)

  x<-remove_bibliography(x, section_title_df)
  section_title_df<-remove_reference_section(section_title_df)
  
  positions_sections_df<-locate_sections_position(x, section_title_df)
  check_sections_df(positions_sections_df)
  
  material_and_method_section<-extract_material_and_method_section(x, positions_sections_df)
  #result_section<-extract_result_section(x, positions_sections_df)
  
  saveRDS(material_and_method_section, file = paste0("Material_and_Methods_Section/" , paste0(pdf_name, ".rds")))
  #saveRDS(result_section, file = paste0("Result_Section/" , paste0(pdf_name, ".rds")))
  
  
}

extract_results <- function(pdf_name, path) {
  
  #txt_pdf <-tabulizer::extract_text(pdf_name) #read the text from the pdf
  txt_pdf <- extract_text(pdf_name)
  txt_pdf <- repair_txt(txt_pdf)
  
  x<-annotate_txt_pdf(txt_pdf)   #create the dataframe for NLP using udpipe
  
  #read the output from poppler and create the dataframe with words, font and fontsize
  df_poppler<-read_outpout_poppler(pdf_name)
  
  #identify the font of the section, first by looking at references and then at Acknowledgement
  font_section<-identify_font(df_poppler)
  
  #the sections what the script will try to identify in the doppler output
  list_of_sections <- list(c("Introduction", "INTRODUCTION"),
                           c("Materials", "Material", "materials", "material", "MATERIALS", "MATERIAL"),
                           c("Methods", "Method", "methods", "method", "METHODS", "METHOD"),
                           c("Acknowledgements", "Acknowledgments", "ACKNOWLEDGEMENTS", "ACKNOWLEDGMENTS",
                             "Acknowledgement", "Acknowledgment", "ACKNOWLEDGEMENT", "ACKNOWLEDGMENT"),
                           c("References", "REFERENCES"),
                           c("Results", "RESULTS"),
                           c("Discussion", "DISCUSSION", "discussion"),
                           c("Abstract", "ABSTRACT"),
                           c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS"),
                           c("Background", "BACKGROUND"),
                           c("Experimental", "EXPERIMENTAL", "Experiment"), #Experiment :Yu, Z et al 2013.pdf
                           c("Supplementary", "SUPPLEMENTARY"),
                           c("Methodology"), #"Meng, H et al 2007.pdf"
                           c("Appendix"),
                           c("Section", "SECTION")
  )
  
  
  df_poppler<-clean_font_txt(df_poppler)
  section_title_df<-create_section_title_df(font_section, list_of_sections, df_poppler)
  section_title_df<-clean_title_journal(pdf_name, section_title_df)
  section_title_df<-ad_hoc_reorder(section_title_df)
  
  x<-remove_bibliography(x, section_title_df)
  section_title_df<-remove_reference_section(section_title_df)
  
  positions_sections_df<-locate_sections_position(x, section_title_df)
  check_sections_df(positions_sections_df)
  
  result_section<-extract_result_section(x, positions_sections_df)
  
  saveRDS(result_section, file = paste0(path , paste0(pdf_name, ".rds")))
  
  
}

run_tests_with_error_count_bib_removed <- function(pdf_list, pdf_to_ignore) {
  error_counter<-0
  articles_with_error<-c()
  for (pdf_name in pdf_list){
    print(pdf_name)
    if (pdf_name %in% pdf_to_ignore){next}
    res<- try(extract_material_methods(pdf_name))
    
    if (class(res) == "try-error"){
      #print(pdf_name)
      error_counter<-error_counter+1
      articles_with_error<-c(articles_with_error, pdf_name)
    }

  }
  print("Error on all articles :")
  print(error_counter)
  return(list("errors"=error_counter, "articles"=articles_with_error))
}

run_extraction_results <- function(pdf_list, pdf_to_ignore, path) {
  error_counter<-0
  articles_with_error<-c()
  for (pdf_name in pdf_list){
    print(pdf_name)
    if (pdf_name %in% pdf_to_ignore){next}
    res <- try(extract_results(pdf_name, path))
    
    if (class(res) == "try-error"){
      #print(pdf_name)
      error_counter<-error_counter+1
      articles_with_error<-c(articles_with_error, pdf_name)
    }
    
  }
  print("Error on all articles :")
  print(error_counter)
  return(list("errors"=error_counter, "articles"=articles_with_error))
}


# Dev :
# pdf_to_ignore<-c("Huang X et al 2013.pdf", #Supporting information
#                  "Durantie, E et al 2017.pdf", #SupplementaryInformation
#                  "Heringa, M B et al 2016.pdf", #no material and method
#                  "Katsnelson, B A et al 2011.pdf", #problem with poppler section
#                  "Jensen, A I et al 2017.pdf", #problem with poppler section
#                  "Kim, Y R et al 2014.pdf", #review + material in table
#                  "Mangalampalli, B et al 2018.pdf", #problem with poppler section
#                  "Wang, Y et al 2008.pdf", #special caracters in output of df_popplers
#                  "Weissig, V et al 1998.pdf", #pdf is "empty", cannot be read, look more like a scan
#                  "Tam, Y T et al 2016.pdf", #not an article
#                  "Terentyuk, G 2009.pdf", #not an article
#                  "Stolnik, S et al 2001.pdf", #pdf is "empty", cannot be read, look more like a scan
#                  "Li, Z et al 2005.pdf" #a book inside lung
# )


pdf_to_ignore<-c("Durantie, E et al 2017.pdf", #SupplementaryInformation
                 "Heringa, M B et al 2016.pdf", #no material and method #Introduction with 4 subparts
                 "Huang X et al 2013.pdf", #Supporting information
                 "Stolnik, S et al 2001.pdf", #pdf is "empty", cannot be read, look more like a scan
                 "Tam, Y T et al 2016.pdf", #not an article, #communication
                 "Terentyuk, G 2009.pdf", #not an article, #news room
                 "Weissig, V et al 1998.pdf" #pdf is "empty", cannot be read, look more like a scan
                 
)

#Biodistribution

setwd("~/Dev_pdf_poppler_output/Biodistribution/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
res_Biodistribution<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Biodistribution/"
res_Biodistribution_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Biodistribution$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Biodistribution$articles), "/home/NET1/rollaet/Articles_bug/")
gc()


#Cardiotoxicity


pdf_to_ignore<-c("Eckardt, Kai-Uwe 2013.pdf", #not an article, communication
                 "Holland N 2014.pdf" #PhD thesis
)

setwd("~/Dev_pdf_poppler_output/Cardiotoxicity/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
res_Cardiotoxicity<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Cardiotoxicity/"
res_Cardiotoxicity_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Cardiotoxicity$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Cardiotoxicity$articles), "/home/NET1/rollaet/Articles_bug/")


gc()

# Genotoxicity

pdf_to_ignore<-c("Kwon, J Y et al 2014.pdf", #not an article, erratum
                 "Lv, H et al 2006.pdf", #Review
                 "Ma Y et al 2016.pdf", #not an article ? scientific report #bug because no introduction and different
                  #font between References and Aknowledgement and other section
                 "Stone, V et al 2009.pdf", #review
                 "Valdiglesias, V et al 2015.pdf", #review
                 "Yoshida, R et al 2009.pdf" #letter
)

setwd("~/Dev_pdf_poppler_output/Genotoxicity/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
res_Genotoxicity<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Genotoxicity/"
res_Genotoxicity_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Genotoxicity$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Genotoxicity$articles), "/home/NET1/rollaet/Articles_bug/")

gc()

#Hemato

pdf_to_ignore<-c("Girard, D 2014.pdf", #letter
                 "Helen Vallhov et al 2006.pdf", #letter
                 "Jones C, Brooks AE et al 2012.pdf", #supplementary information
                 "Juliano, RL 1983.pdf", #book, scanned
                 "Semberova, J et al 2009.pdf", #letter
                 "Thomson, H et al 2008.pdf", #correspondance
                 "Zbinden, G 1989.pdf" ##book, scanned
                 
)

setwd("~/Dev_pdf_poppler_output/Heamcompatibility/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
res_Heamcompatibility<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Heamcompatibility/"
res_Heamcompatibility_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Heamcompatibility$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Heamcompatibility$articles), "/home/NET1/rollaet/Articles_bug/")

gc()

pdf_to_ignore<-c("Cheung, K L et al 2012.pdf", #Communication
                 "Deng et al 2011.pdf", #Letter
                 "Halets, I et al 2013.pdf", #Note
                 "Inoue, K I et al 2011.pdf",#Communication or something like this
                 "Journeay, W S et al 2014.pdf", #Weird Stuff
                 "Junnila, S K 2015.pdf", #Med hypothesis
                 "Lanone, S et al 2011.pdf", #Editorial
                 "Moghimi, S M et al 2010.pdf", #Is this an article ? Crazy sections titles
                 "Preedia Babu, E et al 2017.pdf", #Scientific report
                 "Rolland, A et al 1995.pdf", #scan of an article
                 "Shvedova, A A et al 2013.pdf", #communication
                 "Szebeni, J 2005.pdf", #Is this an article ? Strange sections titles
                 "Toyama, T et al 2008.pdf" #Case report
                 
)

setwd("~/Dev_pdf_poppler_output/Immune Effects/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
#res_Immune_Effects<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Immune_effects/"
res_Immune_Effects_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Immune_Effects$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Immune_Effects$articles), "/home/NET1/rollaet/Articles_bug/")

gc()

pdf_to_ignore<-c("Yang, B et al 2010.pdf" #Abstracts
                 
)


setwd("~/Dev_pdf_poppler_output/Liver Toxicity/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
#res_Liver_Toxicity<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Liver_toxicity/"
res_Liver_Toxicity_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Liver_Toxicity$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Liver_Toxicity$articles), "/home/NET1/rollaet/Articles_bug/")


gc()



pdf_to_ignore<-c("Li, Z et al 2005.pdf", #a book inside lung
                 "Braakhuis H M, Park M et al 2014.pdf", #Review
                 "Chen, H et al 2017.pdf", #Supplemental table
                 "Erdely A et al 2009.pdf", #Nano letter
                 "Fytianos, K et al 2016.pdf", #Letter
                 "Gilbert, N 2009.pdf", #News
                 "Inoue K 2011.pdf", #Review
                 "Inoue K, Takano H 2011.pdf", #Review
                 "Li, Junyi et al 2016.pdf", #Scientific Report
                 "Liu, H-L L et al 2011.pdf", #Supplemental data
                 "Wu, Tianshu 2014.pdf" #Review
                 
)

setwd("~/Dev_pdf_poppler_output/Lung Toxicity/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
#res_Lung_Toxicity<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Lung_toxicity/"
res_Lung_Toxicity_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Lung_Toxicity$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Lung_Toxicity$articles), "/home/NET1/rollaet/Articles_bug/")

gc()

pdf_to_ignore<-c("")


setwd("~/Dev_pdf_poppler_output/Nephrotoxicity/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
res_Nephrotoxicity<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Nephrotoxicity/"
res_Nephrotoxicity_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Nephrotoxicity$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Nephrotoxicity$articles), "/home/NET1/rollaet/Articles_bug/")

gc()


pdf_to_ignore<-c("Boyes WK et al 2012.pdf", # Summary of a symposium
                 "Dobson A et al 2012.pdf", #can't be read
                 "Wang, J et al 2007.pdf", #Article in chinese
                 "Zhang, Q L et al 2011.pdf", #Scan of article
                 "Zhang, Y et al 2013.pdf" #Communication
                 
                 
)

setwd("~/Dev_pdf_poppler_output/Neurotoxicity/")
pdf_list<-list.files(pattern = "\\.pdf$", recursive = TRUE)
res_Neurotoxicity<-run_tests_with_error_count_bib_removed(pdf_list, pdf_to_ignore)
path="/home/NET1/rollaet/Results/Neurotoxicity/"
res_Neurotoxicity_Results<-run_extraction_results(pdf_list, pdf_to_ignore, path)
file.copy(res_Neurotoxicity$articles, "/home/NET1/rollaet/Articles_bug/")
file.copy(gsub("pdf", "pdf.output_poppler.txt", res_Neurotoxicity$articles), "/home/NET1/rollaet/Articles_bug/")

gc()







