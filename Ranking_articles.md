Ranking of articles
================
Etienne Rolland
20/01/2020

  - [Ranking of the material and method
    section](#ranking-of-the-material-and-method-section)
  - [Visualization of the Ranking :](#visualization-of-the-ranking)

## Ranking of the material and method section

Note that the R code used to create the automatic ranking his hidded to
make it more easily readable. A version with the code visible is
available on the Github repository.

Below are the ontologies of terms used to rank the articles for the
differents categories.

``` r
#ontology of terms
ontology_material_characterisation <- list( Size = c("diameter", "size", "dimension", "radius", "nm"), 
                                   Surface_area = c("surface area"), 
                                   Surface_charge = c("zeta potential", "surface charge", "mv"),
                                   Chemical_composition = c("chemical composition", "coating", "core",
                                                            "shell", "content", "configuration", 
                                                            "molecular ratio"),
                                   # Concentration = c("final concentration", "dilution", "diluted", "mM"),
                                   Aggregation = c("aggregation", "aggregate", "polydisperse", "monodisperse",
                                                   "stability", "agglomeration", "stable", "agglomerate") #,
                                   # Wavelenght = c("wavelenght", "excitation wavelenght", "excitation", "emission",
                                   #                "emission wavelenght")
                                   )




ontology_in_vitro_in_vivo <- list( In_vitro = c("incubation temperature", "incubator", "incubated", "viability",
                                                "cells/ml", "cells/mL",  "cells/well", "cells per well", 
                                                "cultured", "culture", "medium", "media", "penicillin", "% of CO",
                                                "streptomycin", "well plates", "well plate", "cell line", "cells"), 
                                   
                                  In_vivo = c("age", "year old", "month old", "year-old", "month-old", 
                                              "cage", "holding rooms", "housed", "facility", "diet", "ad libitum",
                                              "dark cycle", "light cycle", "euthanized", "acclimate",
                                              "acclimatized", "body weight", "administration route", 
                                              "dose in mg/kg"))
```

``` r
rds_list<-list.files(path="~/Dev_pdf_poppler_output/", pattern = "\\.rds$", recursive=TRUE)

quality_evaluation_df<-create_quality_df(ontology_material_characterisation)

quality_evaluation_df<-run_assesment(rds_list, ontology_material_characterisation, ontology_in_vitro_in_vivo)
```

## Visualization of the Ranking :

This section shows the results of the automated ranking of the articles.
The following plot show the number of articles for each ranking number :

![](Ranking_articles_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The following plot shows the ranking for the articles that has been
automatically classified has containing in vivo experiments :

![](Ranking_articles_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The following plot shows the ranking for the articles that has been
automatically classified has containing in vitro experiments :

![](Ranking_articles_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Finally, the following plot provide an estimation of the overall sizes
of the material and methods section extracted. The unit is the number of
sentences.

![](Ranking_articles_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Finally, here is a sample of best ranked articles :

    ##                             Article_name Size Surface_area Surface_charge
    ## 24           Chinde S, Chinde S 2017.pdf    1            1              1
    ## 69               Kim, Y R et al 2014.pdf    1            1              1
    ## 99       Mangalampalli, B et al 2017.pdf    1            1              1
    ## 114         Oliveira, L T et al 2017.pdf    1            1              1
    ## 149           Singh S P, et al 2013b.pdf    1            1              1
    ## 242              Cowie, H et al 2015.pdf    1            1              1
    ## 273                 Lu, X et al 2016.pdf    1            1              1
    ## 296            Singh, S P et al 2013.pdf    1            1              1
    ## 298           Stoccoro, A et al 2016.pdf    1            1              1
    ## 306            Wills, J W et al 2016.pdf    1            1              1
    ## 534              Cho, W S et al 2011.pdf    1            1              1
    ## 537                Gosens et al 2016.pdf    1            1              1
    ## 539 Götz AA, Vidal-Puig A et al 2011.pdf    1            1              1
    ## 540             Götz, A A et al 2011.pdf    1            1              1
    ## 567                Schreiber, N 2016.pdf    1            1              1
    ## 638                Liu, Y et al 2012.pdf    1            1              1
    ## 654               Wang, Y et al 2011.pdf    1            1              1
    ## 13               Bellusci et al 2014.pdf    1            1              0
    ## 18                Chen, L et al 2017.pdf    1            0              1
    ## 25              Chinde, S et al 2017.pdf    1            0              1
    ## 27             Chung, E J et al 2015.pdf    1            0              1
    ## 30                 Dam DH et al 2015.pdf    1            0              1
    ## 33             Dekkers, S et al 2017.pdf    1            0              1
    ## 52                  Hak S et al 2015.pdf    1            0              1
    ## 53   Harivardhan Reddy, L et al 2005.pdf    1            0              1
    ## 63             Hureaux, J et al 2017.pdf    1            1              1
    ## 66            Jensen, A I et al 2017.pdf    1            0              1
    ## 67                Kaur, A et al 2017.pdf    1            0              1
    ## 72              Kumari, M et al 2014.pdf    1            0              1
    ## 76               Lee, I C et al 2016.pdf    1            1              1
    ##     Chemical_composition Aggregation In_vitro In_vivo Size_mm_section
    ## 24                     1           1      Yes     Yes             167
    ## 69                     1           1      Yes     Yes             142
    ## 99                     1           1      Yes     Yes             163
    ## 114                    1           1      Yes     Yes             190
    ## 149                    1           1      Yes     Yes             142
    ## 242                    1           1      Yes     Yes             108
    ## 273                    1           1      Yes      No              45
    ## 296                    1           1      Yes     Yes              93
    ## 298                    1           1      Yes     Yes             145
    ## 306                    1           1      Yes     Yes             684
    ## 534                    1           1      Yes     Yes             153
    ## 537                    1           1      Yes     Yes             164
    ## 539                    1           1      Yes     Yes              45
    ## 540                    1           1      Yes     Yes              70
    ## 567                    1           1      Yes      No             274
    ## 638                    1           1       No     Yes             126
    ## 654                    1           1      Yes     Yes             129
    ## 13                     1           1      Yes     Yes             111
    ## 18                     1           1      Yes     Yes              38
    ## 25                     1           1      Yes     Yes             170
    ## 27                     1           1      Yes     Yes              69
    ## 30                     1           1      Yes     Yes              62
    ## 33                     1           1      Yes     Yes             169
    ## 52                     1           1      Yes     Yes             105
    ## 53                     1           1      Yes      No             100
    ## 63                     1           0      Yes     Yes             129
    ## 66                     1           1      Yes     Yes             313
    ## 67                     1           1      Yes     Yes              88
    ## 72                     1           1      Yes     Yes             169
    ## 76                     0           1      Yes     Yes              92
    ##     Ranking condition
    ## 24        5     Mixed
    ## 69        5     Mixed
    ## 99        5     Mixed
    ## 114       5     Mixed
    ## 149       5     Mixed
    ## 242       5     Mixed
    ## 273       5 Not_mixed
    ## 296       5     Mixed
    ## 298       5     Mixed
    ## 306       5     Mixed
    ## 534       5     Mixed
    ## 537       5     Mixed
    ## 539       5     Mixed
    ## 540       5     Mixed
    ## 567       5 Not_mixed
    ## 638       5 Not_mixed
    ## 654       5     Mixed
    ## 13        4     Mixed
    ## 18        4     Mixed
    ## 25        4     Mixed
    ## 27        4     Mixed
    ## 30        4     Mixed
    ## 33        4     Mixed
    ## 52        4     Mixed
    ## 53        4 Not_mixed
    ## 63        4     Mixed
    ## 66        4     Mixed
    ## 67        4     Mixed
    ## 72        4     Mixed
    ## 76        4     Mixed
