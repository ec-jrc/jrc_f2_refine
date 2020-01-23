Radar plot, Circular Plot
================
Etienne Rolland
21/01/2020

  - [Content of this Rmarkdown
    document](#content-of-this-rmarkdown-document)
  - [Radar plots](#radar-plots)
      - [Radar plot, for all the articles
        :](#radar-plot-for-all-the-articles)
      - [Radar plot, for the articles with a ranking of 1
        :](#radar-plot-for-the-articles-with-a-ranking-of-1)
      - [Radar plot, for the articles with a ranking of 2
        :](#radar-plot-for-the-articles-with-a-ranking-of-2)
      - [Radar plot, for the articles with a minimum ranking of 2
        :](#radar-plot-for-the-articles-with-a-minimum-ranking-of-2)
      - [Radar plot, all the articles, in vitro
        :](#radar-plot-all-the-articles-in-vitro)
      - [Radar plot, all the articles, in vivo
        :](#radar-plot-all-the-articles-in-vivo)
      - [Radar Plot for each subtype of articles, with a minumum score
        of 2
        :](#radar-plot-for-each-subtype-of-articles-with-a-minumum-score-of-2)
  - [Circular barplots](#circular-barplots)
      - [Circularbar plot, for the article with a Ranking of 1
        only](#circularbar-plot-for-the-article-with-a-ranking-of-1-only)
      - [Circular barplot, for the article with a Ranking of 2
        only](#circular-barplot-for-the-article-with-a-ranking-of-2-only)
      - [Circular barplot, for all the articles, stratified by
        folder](#circular-barplot-for-all-the-articles-stratified-by-folder)
      - [Circular barplot, for all the articles, organized by
        characteristic](#circular-barplot-for-all-the-articles-organized-by-characteristic)
      - [Circular barplot, for all the articles, stratified by folder,
        of articles, with a minumum score of 2
        :](#circular-barplot-for-all-the-articles-stratified-by-folder-of-articles-with-a-minumum-score-of-2)
      - [Circular barplot, for all the articles, stratified by material
        characteristic, of articles, with a minumum score of 2
        :](#circular-barplot-for-all-the-articles-stratified-by-material-characteristic-of-articles-with-a-minumum-score-of-2)

# Content of this Rmarkdown document

This document present different graphs used to visualized how the
different aspects of the material characterisation are present in the
“material and method” sections. Two visualization technics are used :

  - the radar plot : a two-dimensional chart type designed to plot one
    or more series of values over multiple quantitative variables. The
    variable here are the percentage of article with the
    characterisation.

  - the circular barploy : a barplot, with each bar displayed along a
    circle instead of a line.

# Radar plots

## Radar plot, for all the articles :

The following radar plot display the percentage of articles that have
the aspects of the material characterisation shown in axis, for all the
articles for which the extraction was successufull.

``` r
data<-quality_evaluation_df

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Folder<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-as.data.frame(t(colMeans(data)))
data <- rbind(rep(1) , rep(0) , data)

radarchart( data, axistype=1, 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8,
            
            #title
            title="Radar plot for the article\n with a ranking of 1 only."
            

) 
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Radar plot, for the articles with a ranking of 1 :

The following radar plot display the percentage of articles that have
the aspects of the material characterisation shown in axis, uniquely for
the article with a ranking of 1.

``` r
data<-quality_evaluation_df

data<-quality_evaluation_df[which(quality_evaluation_df$Ranking==1),]

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Folder<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-as.data.frame(t(colMeans(data)))
data <- rbind(rep(1) , rep(0) , data)

radarchart( data, axistype=1, 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8,
            
            #title
            title="Radar plot for the article\n with a ranking of 1 only."
            

) 
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Radar plot, for the articles with a ranking of 2 :

The following radar plot display the percentage of articles that have
the aspects of the material characterisation shown in axis, uniquely for
the article with a ranking of 2.

``` r
data<-quality_evaluation_df

data<-quality_evaluation_df[which(quality_evaluation_df$Ranking==2),]

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Folder<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-as.data.frame(t(colMeans(data)))
data <- rbind(rep(1) , rep(0) , data)

radarchart( data, axistype=1, 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8,
            
            #title
            title="Radar plot for the article\n with a ranking of 2 only."
            

) 
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Radar plot, for the articles with a minimum ranking of 2 :

The following radar plot display the percentage of articles that have
the aspects of the material characterisation shown in axis, uniquely for
the article with a minimum ranking of 2.

``` r
data<-quality_evaluation_df

data<-quality_evaluation_df[which(quality_evaluation_df$Ranking>1),]

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Folder<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-as.data.frame(t(colMeans(data)))
data <- rbind(rep(1) , rep(0) , data)

radarchart( data, axistype=1, 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8,
            
            #title
            title="Radar plot for the article\n with a minimum ranking of 2 only."
            

) 
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Radar plot, all the articles, in vitro :

The following radar plot display the percentage of articles that have
the aspects of the material characterisation shown in axis, uniquely for
the article that are categorized has “In vitro”.

``` r
data<-quality_evaluation_df

data<-data[which(data$In_vitro=="Yes"),]


data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Folder<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-as.data.frame(t(colMeans(data)))
data <- rbind(rep(1) , rep(0) , data)

radarchart( data, axistype=1, 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8,
            
            #title
            title="Radar plot for the article\n that are categorized as In Vitro."
)
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Radar plot, all the articles, in vivo :

The following radar plot display the percentage of articles that have
the aspects of the material characterisation shown in axis, uniquely for
the article that are categorized has “In vivo”.

``` r
data<-quality_evaluation_df

data<-data[which(data$In_vivo=="Yes"),]

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Folder<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-as.data.frame(t(colMeans(data)))
data <- rbind(rep(1) , rep(0) , data)

radarchart( data, axistype=1, 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8,
            
            #title
            title="Radar plot for the article\n categorised as in vivo")
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Radar Plot for each subtype of articles, with a minumum score of 2 :

The following radar plots display the percentage of articles that have
the aspects of the material characterisation shown in axis, uniquely for
the article that have a minimum ranking of 2.

``` r
data<-quality_evaluation_df
data<-data[which(data$Ranking>1),] 

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-aggregate(data, by = list(data$Folder), FUN = mean)
data$Folder<-data$Group.1
data$Group.1<-NULL



mytitle <- data$Folder
data <- rbind(rep(1) , rep(0) , data)
data$Folder<-NULL

par(mar=rep(0.8,4))
par(mfrow=c(5,2))
# Prepare color
colors_border=colormap(colormap=colormaps$viridis, nshades=9, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=9, alpha=0.3)


# Loop for each plot
for(i in 1:9){
  
  # Custom the radarChart !
  radarchart( data[c(1,2,i+2),], axistype=1, 
              
              #custom polygon
              pcol=colors_border[i] , pfcol=colors_in[i] , plwd=2, plty=1 , 
              
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8,
              
              #custom labels
              vlcex=0.9,
              
              #title
              title=mytitle[i]
  )
}
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Circular barplots

## Circularbar plot, for the article with a Ranking of 1 only

Similar to the radar plot in a previous section,this circular plot
display the percentage of articles that have the specified aspects of
the material characterisation, uniquely for the article with a quality
of 1.

``` r
data<-quality_evaluation_df
data<-quality_evaluation_df[which(quality_evaluation_df$Ranking==1),]

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-aggregate(data, by = list(data$Folder), FUN = mean)
data$Folder<-data$Group.1
data$Group.1<-NULL

data<-cbind(data$Folder, gather(data[1:5]))
colnames(data)<-c("folder", "feature", "value")

data$feature<-as.factor(data$feature)
empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$feature), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$feature=rep(levels(data$feature), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(feature)


data$id=seq(1, nrow(data))



# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)


# prepare a data frame for base lines
base_data=data %>% 
  group_by(feature) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

data$value<-data$value*100

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=folder)) +    
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=folder, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -30, label=feature), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE) #+

  # ggtitle("\n \n \nCircular Barplot, for the article with a ranking of 1.") + theme(plot.title = element_text(hjust = 0.5, vjust = 0.8))

p
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Circular barplot, for the article with a Ranking of 2 only

Similar to the radar plot above, this circular plot display the
percentage of articles that have the specified aspects of the material
characterisation, uniquely for the article with a quality of 2.

``` r
data<-quality_evaluation_df
data<-quality_evaluation_df[which(quality_evaluation_df$Ranking==2),]

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL


data<-aggregate(data, by = list(data$Folder), FUN = mean)
data$Folder<-data$Group.1
data$Group.1<-NULL

data<-cbind(data$Folder, gather(data[1:5]))
colnames(data)<-c("folder", "feature", "value")

data$feature<-as.factor(data$feature)
empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$feature), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$feature=rep(levels(data$feature), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(feature)


data$id=seq(1, nrow(data))



# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)


# prepare a data frame for base lines
base_data=data %>% 
  group_by(feature) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

data$value<-data$value*100

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=folder)) +    
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=folder, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -30, label=feature), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE) #+
# 
#   ggtitle("\n \n \nCircular Barplot, for the article with a ranking of 2.") + theme(plot.title = element_text(hjust = 0.5, vjust = 0.8))

p
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Circular barplot, for all the articles, stratified by folder

This circular plot display the percentage of articles that have the
specified aspects of the material characterisation, for all the
articles:

``` r
data<-quality_evaluation_df

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL

data<-aggregate(data, by = list(data$Folder), FUN = mean)
data$Folder<-data$Group.1
data$Group.1<-NULL

data<-cbind(data$Folder, gather(data[1:5]))
colnames(data)<-c("folder", "feature", "value")


empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$folder), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$folder=rep(levels(data$folder), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(folder)

data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)


# prepare a data frame for base lines
base_data=data %>% 
  group_by(folder) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

data$value<-data$value*100

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=folder)) +    

  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -20, label=folder), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE) 

p
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Circular barplot, for all the articles, organized by characteristic

This circular plot display the percentage of articles that have the
specified aspects of the material characterisation, for all the
articles:

``` r
data<-quality_evaluation_df

 data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL

data<-aggregate(data, by = list(data$Folder), FUN = mean)
data$Folder<-data$Group.1
data$Group.1<-NULL

data<-cbind(data$Folder, gather(data[1:5]))
colnames(data)<-c("folder", "feature", "value")

data$feature<-as.factor(data$feature)
empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$feature), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$feature=rep(levels(data$feature), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(feature)


data$id=seq(1, nrow(data))



# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)


# prepare a data frame for base lines
base_data=data %>% 
  group_by(feature) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

data$value<-data$value*100

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=folder)) +    
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=folder, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -30, label=feature), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)

p
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Circular barplot, for all the articles, stratified by folder, of articles, with a minumum score of 2 :

This circular plot display the percentage of articles that have the
specified aspects of the material characterisation, for all the articles
with a minimum score of two :

``` r
data<-quality_evaluation_df
data<-quality_evaluation_df[which(quality_evaluation_df$Ranking>1),]

data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL

data<-aggregate(data, by = list(data$Folder), FUN = mean)
data$Folder<-data$Group.1
data$Group.1<-NULL

data<-cbind(data$Folder, gather(data[1:5]))
colnames(data)<-c("folder", "feature", "value")


empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$folder), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$folder=rep(levels(data$folder), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(folder)

data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)


# prepare a data frame for base lines
base_data=data %>% 
  group_by(folder) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

data$value<-data$value*100

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=folder)) +    

  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -20, label=folder), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE) 

p
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Circular barplot, for all the articles, stratified by material characteristic, of articles, with a minumum score of 2 :

This circular plot display the percentage of articles that have the
specified aspects of the material characterisation, with a minimum score
of two :

``` r
data<-quality_evaluation_df
data<-quality_evaluation_df[which(quality_evaluation_df$Ranking>1),]


data$Article_name<-NULL
data$Size_mm_section<-NULL
data$Ranking<-NULL
data$In_vitro<-NULL
data$In_vivo<-NULL
data$Wavelenght<-NULL
data$Concentration<-NULL

data<-aggregate(data, by = list(data$Folder), FUN = mean)
data$Folder<-data$Group.1
data$Group.1<-NULL

data<-cbind(data$Folder, gather(data[1:5]))
colnames(data)<-c("folder", "feature", "value")

data$feature<-as.factor(data$feature)
empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$feature), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$feature=rep(levels(data$feature), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(feature)


data$id=seq(1, nrow(data))



# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)


# prepare a data frame for base lines
base_data=data %>% 
  group_by(feature) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

data$value<-data$value*100

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=folder)) +    
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=folder), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=folder, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -30, label=feature), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)

p
```

![](Radar_circular_barplot_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
