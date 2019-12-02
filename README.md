# Refine H2020


# Quick start


# Dependencies

* udpipe -> NLP structure.
* readR 
* Rpoppler -> not anymore. Pdf are now read using **tabulizer**, a R binding for the Tabula java library. This allow to read more consistently pdf wrotte in a two columns format.
* ...

# Notes

At the beginning of the project the content of the pdfs in txt format was already provided. Most of the Markdown report are based on this dataset.
Current implementation use **Tabulizer** to extract directly the text from Pdf. The reasons are :

* with the use of pdf metadata (font, font size), the preferred input is now the pdf of articles.
* it ease the simultaneous parsing of pdfs texts and of the poppler output that contain the fonts of words of the pdf. The data-structure of udpipe does not allow to retrieve easely the title of the document once processed.
* make the library easely reusable for new articles later and remove the need for pre-processing outside R.


# Technical debt 

* The output of poppler that contain the font for each word in each pdf's has already been prepared in advance. Producing this output require the use of an experimental feature from a old version of poppler (0.63, current poppler : 0.83). That being said this feature can certainly be reimplementel properly inside directly, using Rcpp. The details to do checkout on the good branch and compile the desired version are inside the labbook.
* The R function to read the poppler_output use a global variable behind the scene. The variable has the same name that the expected output of the function read_poppler_output().
* In order to call all the functions into one funtion Extract_material_and_method(), some variable has been passed as global, such has x, and an other dataframe. Some refactory of functions input are required.
