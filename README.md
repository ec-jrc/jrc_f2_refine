# jrc_f2_refine 

This repository hosts the code for a research work conducted at the Joint Research Centre, Directorate F - Health, Consumers and Reference Materials, Ispra (VA), Italy, in relation to the Horizon 2020 project REFINE, aiming to develop a regulatory science framework for the risk-benefit assessment of medical products based on nanotechnology.

Please refer to the following publication : *Toxicity effects of nanomaterials for health applications: how automation can support systematic review of the literature ? Blanka Halamoda-Kenzaoui, Etienne Rolland, Jacopo Piovesan, Antonio Puertas Gallardo, Susanne Bremer-Hoffmann* [doi.org/10.1002/jat.4204 ](https://doi.org/10.1002/jat.4204).

# Organisation of the repository

* **Analyses_of_corpus :** this folder contains all the .Rmd files that were used to produce the analysis mentionned in the article, as well as the .md documents that were produced by this .Rmd files. This folder contain also all the original charts present in the articles.
* **DOIs_articles_corpus :** this folder contain the DOIs of the articles of the corpus, as well as their titles, when this information was extractible via pdftool. The DOI and the titles are avalaible in CSV and RDS format. Some example of missing DOI are due to : a lack of DOI in the original article (in case the article was not a research article), an article of the corpus that was not an article in pdf format but a scan of article in pdf format, etc.
* **segmenteR :** this repository contain the tool used to segment the articles of the corpus during the research work, as well as the documentation and the test. This subdirectory is a **R package** that can be install using devtools. Please see below for more details on its installation and utilisation.

## Important : About the corpus and this repository

The corpus used for the research work was composed of articles in pdf formats. For copyright reason, the pdfs and rds files created by the tools for segmentation, containing sections from the articles, could not have been added to this repository. If you want to reproduce part or the totality of the analysis, please use the DOIs to access the articles and clone this repository or install the segmentation tool.

## segmenteR

segmenteR is a tool to extract a section, for example, "material and methods", from the pdf of an article, using the fonts information from the pdf and natural language processing.

### Introduction

segmenteR is a tool to segment articles that has been elaborated in the context of a research work conducted at the Joint Research Centre, Directorate F - Health, Consumers and Reference Materials, Ispra (VA), Italy, in relation to  the Horizon 2020 project REFINE,  aiming to develop a regulatory science framework for the risk-benefit assessment of medical products based on nanotechnology.

The project aimed to analyse a corpus of 801 articles, obtained from the PubMed MeSH database and related to several toxicity topics (cardiotoxicity, genotoxicity, etc), to evaluate the quality of the reporting of methods of characterization and to parse the articles for specifics toxicity effects and specifics nanoparticules.

In order to evaluate the quality of the reporting inside each articles and parse the texts for specific toxicity effects, we needed to extract both the material and methods section and the results section of each articles, respectively. The tool segmenteR was developed to carry this specific subtask of segmentating the articles into the differents sections.

If you use this tool, please cite to the following publication : *Toxicity effects of nanomaterials for health applications: how automation can support systematic review of the literature ?, Blanka Halamoda-Kenzaoui, Etienne Rolland, Jacopo Piovesan, Antonio Puertas Gallardo, Susanne Bremer-Hoffmann* .

### Requirement

To extract the information on the fonts inside the pdf we use poppler, a [PDF rendering library](https://poppler.freedesktop.org/) and the cpp API of poppler. 
The package require a **version of poppler >= 0.89** as well as a recent version of pdftools. The dev version of pdftools integrate the required change, but you need to install it from github. Currently you need to install :

``` r
devtools::install_github("ropensci/pdftools") 
devtools::install_github("ec-jrc/jrc_f2_refine", subdir="segmenteR") 
```

### Getting started

#### The short way

Download an open access article that was part of the corpus : 

``` r
url <- ('https://www.cell.com/action/showPdf?pii=S1525-0016%2816%2931594-5')
download.file(url, 'Abrams, M T et al 2010.pdf')
```

``` r
library(segmenteR)
## basic example code

section_aliases <- c("material", "method", "experimental", "experiment", "methodology")

#model definition can be skipped, the function can download it automatically
model <- "english-gum-ud-2.4-190531.udpipe"
material_and_methods <- segmenteR::extract_section_from_pdf(pdf_name="Abrams, M T et al 2010.pdf",
                                                             udpipe_model=model, 
                                                             section_aliases=section_aliases)

```
You have your material and methods section in ConLL-U format inside the dataframe material_and_methods, a format suitable for parsing, etc.
You can stop reading the example here.

#### A more in-depth example

This example show the inner working of the function extract_section_from_pdf() :

```{r}
pdf_name <- "Abrams, M T et al 2010.pdf"
remove_bibliography <- TRUE

txt_pdf <- tabulapdf::extract_text(pdf_name) # read the text from the pdf
txt_pdf <- segmenteR::preprocess_article_txt(txt_pdf)
```

The role of the function annotate_txt_pdf() is to load the required model and use the library udpipe to tokenize and annotate the text. Please refer to the vignette to get more detail on the Conll-U format.
The reason behind to do this annotation is that we will need this informations to estimate where the most likely is the section title. For example, if it is the first word of a sentence, if the the word of the sentence is also a section title, etc.

```{r}
conllu_df <- segmenteR::annotate_txt_pdf(txt_pdf, udpipe_model=model ) # create the dataframe for NLP using udpipe
```
The other information we use, and the reason why we work directly on a pdf instead of a text, is the font informations from the pdf, the font and fontsize of the words inside the pdf. To do this we use poppler, a [PDF rendering library](https://poppler.freedesktop.org/) and the cpp API of poppler. We extract this information using a specific version of pdftools, reason why the package need a version of poppler > 0.89 as well as a recent version of pdftools. 

```{r}
poppler_output <- segmenteR::prepare_poppler_output(pdf_name)
```

This information is used to identify the probable font of the section, by first looking at the font used for the words Reference and Acknowledgment, that usually appear in only one occurrence in scientific articles :

```{r}
font_section <- segmenteR::identify_font(poppler_output)
print(font_section)
```
Knowing this, we can know which sections are inside the articles and in which order they appear.
The list under is the sections title that the function will try to identify in the poppler output :

```{r}
list_of_sections <- list(
    c("Introduction", "INTRODUCTION"),
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
    c("Experimental", "EXPERIMENTAL", "Experiment"),
    c("Supplementary", "SUPPLEMENTARY"),
    c("Methodology"),
    c("Appendix"),
    c("Section", "SECTION")
  )
```
Clean_font_txt() remove the most common font inside the articles, which improve the correct localization of the sections inside the pdf by create_section_title_df().

```{r}
poppler_output <- segmenteR::clean_font_txt(poppler_output)
```
section_title_df is a dataframe with that contain the section titles in the articles and their relative order, based on the fonts information retrieved from the pdf. This informations (order and existence) will be used to localize the section in the ConLL-U format. This step is needed as the order and the composition of the sections title can change from one article to the other.

```{r}
section_title_df <- segmenteR::create_section_title_df(font_section, list_of_sections, poppler_output)
section_title_df <- segmenteR::clean_title_journal(pdf_name, section_title_df)
section_title_df <- segmenteR::ad_hoc_reorder(section_title_df)
```
Removing the bibliography prevent some error in the localization in some sections, especially if a reference start with the word "material". This option can be set to false.

```{r}
if (remove_bibliography == TRUE) {
  conllu_df <- segmenteR::remove_bibliography_from_conllu(conllu_df, section_title_df)
  section_title_df <- segmenteR::remove_reference_section_from_titles(section_title_df)
}
```

Knowing the relative order of the sections from one side, and their name and having the information from the Conll-U dataframe, such as the position inside the sentence, or the other words in the sentence, we can estimate the position of the different sections inside the Conll-U dataframe.
Please note that the positions_sections_df is not the section_title_df, and that the position number (occurrences) of the position is different, since section_title_df refer to the position inside the output from poppler, while section_title_df indicate the position inside the Conll-U dataframe.

```{r}
positions_sections_df <- segmenteR::locate_sections_position_in_conllu(conllu_df, section_title_df)
segmenteR::check_sections_df(positions_sections_df)
```

```{r}
section <- segmenteR::extract_section_from_conllu(conllu_df, positions_sections_df, section_aliases)
```
Finally extract_section_from_conllu() provide the section in ConLL-U format inside the dataframe section.
