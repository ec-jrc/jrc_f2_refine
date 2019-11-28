# Refine H2020


# Quick start


# Dependencies

* udpipe
* readR
* Rpoppler
* ...

# Notes

At the beginning of the project the pdf content in txt format was already provide. Most of the Markdown report are based on this dataset.
Current implementation use Rpoppler to extract text from Pdf. The reasons are :

* with the use of pdf metadate (font, font size), the preferred input is now the pdf of article.
* it ease the simultaneous parsing of pdfs texts and of the poppler output that contain the fonts of words of the pdf. 
The data-structure of udpipe does not allow to retrieve easely the title of the document once processed.
* make the library easely reusable for new articles later and remove the need for pre-processing outside R.

This open the possibility for a shiny app or a R library later.
Using Rpoppler instead of an other application to read pdf seems to introduce subtiles differences in the content. 
NB : the output of Rpoppler contain 242 **more** lemma for Abrams, M T et al 2010. 


## Technical debt 

* The output of poppler that contain the font for each word in each pdf's has already been prepared in advance. 
* Producing this output require the use of an experimental feature from a old version of poppler (0.63, current poppler : 0.83).
That being said this feature can certainly be reimplementel properly inside directly, using Rcpp.
The details do checkout on the good branch and compile the desired version are inside the labbook.

