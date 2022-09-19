# import libraries ####
# data wrangling

library(tidyverse)
library(reshape)
# library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(viridis)
library(rscopus)
library(rlist)
library(rgeos)
library(future)
library(parallel)
library(doParallel)
library(feather)
library(FSA) # summarize the percentage
library(R.utils) # R utilities packages
library(Kendall) # Mann-Kendall trend test
library(tidytext)
library(psych) # summary statistics
library(patchwork) # Combine different graphs


# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(corrplot)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(bibliometrix)
library(ggwordcloud)
library(colorspace)
library(rworldmap)
library(countrycode)
library(usethis)
library(pastecs)
library(officer) # make editable map
library(rvg) # make editable map
library(ggvis)
library(scales) # Make color gradient scales
library(dendextend) # make dendrogram plots
library(ggdendro) # make dendrogram plots

# Import data ####

river_ml_short <- read_csv("river_ml.csv")
# Figure 3 ####

river_ML <- river_ml_short %>% group_by(ML, Period) %>% 
    #filter(!str_detect(ML, "Big Data|Reinforcement Learning|Human interpretable information extraction")) %>%  
    summarise(n = n())  %>% 
    group_by(Period) %>% 
    mutate(Percentage = round(n*100/sum(n), digits = 2)) %>%
    arrange(desc(n))
river_ML <- river_ML[complete.cases(river_ML),]
river_ML$ML <- factor(river_ML$ML, levels = c("Supervised Learning", "Neural Networks & Deep Learning", "Reinforcement Learning",
                                              "Self-supervised Learning", "Semi-supervised Learning","Unsupervised Learning"))

cols <- c("Increasing" = "red", "Decreasing" = "blue", "Stable" = "violet")
cc_model <- seq_gradient_pal("dodgerblue3", "firebrick1", "Lab")(seq(0,1,length.out=35))
plot_river_ML_cluster2 <- ggplot(river_ML, aes(fill=ML, y=Percentage, x=Period)) + 
    geom_bar(position="fill", stat="identity", color = "white") + 
    theme_bw() +
    ylab("Percentage of publications (%)") +
    xlab("Periods") +
    scale_fill_manual(values=cc_model[rev(c(30,23,18,13,8,1))],  name="Machine Learning\nCategories",) +
    scale_y_continuous(labels = seq(from = 0, to = 100, length.out = 5))+
    theme(text=element_text(size=16),
          strip.text.x = element_text(size=10),
          axis.text = element_text(size=11),
          axis.title = element_text(size=14),
          legend.position = "right",
          legend.title =element_text(size = 14),
          legend.text = element_text(size = 11))
plot_river_ML_cluster2


river_pub_cluster <- river_ml_short %>%  select( Year, ML) %>% 
    dplyr::group_by(Year, ML) %>% 
    dplyr::summarise(n=n()) %>% 
    dplyr::arrange(Year) %>% 
    dplyr::filter(Year < 2021 & Year > 1979)

river_pub_cluster$ML <- factor(river_pub_cluster$ML, levels = c("Supervised Learning", "Neural Networks & Deep Learning", "Reinforcement Learning",
                                                                "Semi-supervised Learning","Self-supervised Learning", "Unsupervised Learning"))


plot_river_pub_cluster2 <- ggplot(river_pub_cluster, aes(x=Year, y=n)) +
    geom_point(size = 2) +
    geom_line()+
    labs(x = "Years", y = "Number of publications (-)", fill = NULL, title = NULL) +
    facet_wrap(.~ ML, scales = "free_y", ncol=3) +
    theme_bw()+
    theme(text=element_text(size=16),
          strip.text.x = element_text(size=10),
          axis.text = element_text(size=11),
          axis.title = element_text(size=14))
design <- "
111
223
" 
plot_combined <- plot_river_pub_cluster2/plot_river_ML_cluster2 +
    guide_area() + plot_layout(design=design, guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = "bold"))
plot_combined

# Figure 4 ####


river_pub_year <- river_ml_short %>% 
    filter(str_detect(ML, "Unsupervised Learning|Supervised Learning")) %>% 
    select( Year, id) %>% 
    dplyr::group_by(Year, id) %>% 
    dplyr::summarise(n=n()) %>% 
    dplyr::arrange(Year) %>% 
    dplyr::filter(Year < 2021 & Year > 1979)

river_pub_year$id <- factor(river_pub_year$id, labels = c("Associate rule", "Clustering", "Decision trees", 
                                                          "Discriminant Analysis", "Ensemble methods", "Gaussian processes",
                                                          "Nearest neighbors", "Linear models",
                                                          "Manifold learning", "Matrix factorisation","Multiclass and multilabel", "Naive Bayes",
                                                          "Stochastic Gradient Descent", "Support Vector Machines"))

river_pub_year$id <- factor(river_pub_year$id, levels = c("Matrix factorisation", "Clustering","Manifold learning", "Associate rule",
                                                          "Linear models", "Ensemble methods","Decision trees", "Support Vector Machines",
                                                          "Discriminant Analysis", "Stochastic Gradient Descent","Nearest neighbors","Gaussian processes", 
                                                          "Naive Bayes", "Multiclass and multilabel"))
river_pub_year2 <- ggplot(river_pub_year, aes(x=Year, y=n)) +
    geom_point(size = 2) +
    geom_line()+
    labs(x = "Years", y = "Number of publications", fill = NULL, title = NULL) +
    facet_wrap(.~ id, scales = "free_y", ncol=4) +
    theme_bw()+
    theme(text=element_text(size=16),
          strip.text.x = element_text(size=10),
          axis.text = element_text(size=11),
          axis.title = element_text(size=14))
river_pub_year2

# Figure 5 ####

ml_topics_2 <- river_ml_short %>%  filter(str_detect(ML, "Unsupervised Learning|Supervised Learning")) %>% group_by(id, Period, ML) %>% summarise(n=n())
ml_topics_2 <- ml_topics_2[complete.cases(ml_topics_2),]
ml_rank_2 <- ml_topics_2 %>% 
    arrange(Period, -n) %>% 
    group_by(Period)  %>% 
    mutate(Rank = rank(desc(n), ties.method = "min"))

ml_rank_2$id <- factor(ml_rank_2$id, labels = c("Associate rule", "Clustering", "Decision trees", 
                                                "Discriminant Analysis", "Ensemble methods", "Gaussian processes",
                                                "Nearest neighbors", "Linear models",
                                                "Manifold learning", "Matrix factorisation","Multiclass and multilabel", "Naive Bayes",
                                                "Stochastic Gradient Descent", "Support Vector Machines"))

ml_rank_2$id <- factor(ml_rank_2$id, levels = c("Matrix factorisation", "Clustering","Manifold learning", "Associate rule",
                                                "Linear models", "Ensemble methods","Decision trees", "Support Vector Machines",
                                                "Discriminant Analysis", "Nearest neighbors","Stochastic Gradient Descent", "Gaussian processes",
                                                "Naive Bayes", "Multiclass and multilabel"))

ml_rank_2$id <- as.character(ml_rank_2$id)
ml_rank_2_err <- list(aggregate(data = ml_rank_2, Rank ~ id, FUN = mean),
                      aggregate(data = ml_rank_2, Rank ~ id, FUN = min),
                      aggregate(data = ml_rank_2, Rank ~ id, FUN = max)) %>% reduce(full_join, by = "id")


colnames(ml_rank_2_err)[2:4] <- c("Mean rank", "Min rank", "Max rank")
cc_2 <- seq_gradient_pal("dodgerblue3", "firebrick1", "Lab")(seq(0,1,length.out=14))

ml_rank_2_err$id <- reorder(ml_rank_2_err$id, -ml_rank_2_err$`Mean rank`)
ml_rank_2_err <- ml_rank_2_err %>% arrange(`Mean rank`)

plot_ml_rank_2_decade <- ggplot(ml_rank_2_err, aes(y = id, x =`Mean rank`)) +
    geom_point(aes(size = 0.5, color = id)) + 
    geom_errorbar(aes(xmin=`Min rank`, xmax=`Max rank`, color = id), width=0,
                  position=position_dodge(0.05)) + 
    scale_x_continuous(position = "top", limits = c(0,14)
                       ,expand = c(0, 0)
    ) + 
    scale_colour_manual(values=rev(cc_2)) + 
    theme_classic() + 
    geom_text(data = ml_rank_2_err[1:2,], 
              aes(label = id, x = `Max rank` +2),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[3,],
              aes(label = id, x = `Max rank` +2),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[4,],
              aes(label = id, x = `Max rank` +2),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[5:7,],
              aes(label = id, x = `Max rank` +2),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[8,],
              aes(label = id, x = `Min rank` -3),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[9,],
              aes(label = id, x = `Min rank` -2),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[10,],
              aes(label = id, x = `Min rank` -3),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[11,],
              aes(label = id, x = `Min rank` - 2),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[12,],
              aes(label = id, x = `Min rank` - 2),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[13,],
              aes(label = id, x = `Min rank` -2),
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[14,],
              aes(label = id, x = `Min rank` -3),
              vjust = 0.25, size = 5) +
    xlab("Rank of Machine Learning")+
    theme(text=element_text(size=16),
          axis.ticks.x = element_line(size = 1),
          axis.ticks.length.x = unit(5, "pt"),
          axis.text = element_text(size=14),
          axis.title = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none", plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm")) # margin ==> troubles

plot_ml_rank_2_decade

# Figure 6 ####

river_research <- river_ml_short[,c(1,2,str_which(colnames(river_ml_short), "Period"):str_which(colnames(river_ml_short), "Others"))]
river_research[is.na(river_research)] <- 0
river_research$ML <- as.factor(river_research$ML)
river_research$ML <- factor(river_research$ML, 
                            levels = c("Supervised Learning", "Unsupervised Learning",
                                       "Deep Learning"))

river_research$id <- as.factor(river_research$id)
river_research_period <- river_research %>% select(-id, - ML) 
river_research_period <- aggregate(data = river_research_period, .~Period, sum)
river_research_period <- river_research_period %>% pivot_longer(cols = -c(Period), names_to = "Research Topics", values_to = "Number of publications")
river_research_rank <- river_research_period %>% 
    arrange(Period, -`Number of publications`) %>% 
    group_by(Period) %>% 
    mutate(Rank = rank(desc(`Number of publications`), ties.method = "min"))

river_research_rank$Trends <- NA
for (i in seq_len(nlevels(as.factor(river_research_rank$`Research Topics`)))){
    if (max(river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]])-
        min(river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]]) < 3){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Stable"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][1]-
               river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][5] > 2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Increasing"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][1]-
               river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][5] < -2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Decreasing"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][3]-
               river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][6] > 2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Increasing"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][3]-
               river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][6] < -2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Decreasing"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][4]-
               river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][6] > 2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Increasing"
    } else {
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Stable"
    }
}

river_research_rank$Trends[river_research_rank$`Research Topics` == "Biodiversity"] <- "Stable"
river_research_rank$Trends[river_research_rank$`Research Topics` == "Groundwater"] <- "Stable"
river_research_rank$Trends <- as.factor(river_research_rank$Trends)
river_research_rank$Trends <- relevel(river_research_rank$Trends, "Increasing")
river_research_rank$`Research Topics` <- as.factor(river_research_rank$`Research Topics`)
rank_research <- tibble(`Research Topics` = levels(river_research_rank$`Research Topics`), nlevels = seq_len(nlevels(river_research_rank$`Research Topics`)))
river_research_rank <- left_join(river_research_rank, rank_research, by = "Research Topics")
river_research_rank$`Research Topics`[river_research_rank$`Research Topics` == "Others"] <- "Misc"


plot_rank_group_new <- ggplot(river_research_rank, aes(x = Period, y= Rank, group = `Research Topics`)) + # Good graph 
    geom_point(aes(color = Trends, size = 1.01)) +
    geom_line(aes(color = Trends, size = 1.005, alpha = 0.8)) +
    theme_bw() +
    ylab("Rank of Research Topics") +
    xlab("Periods") +
    facet_wrap(.~Trends, ncol = 2) +
    scale_y_reverse() +
    scale_colour_manual(values = cc_model[c(1,30,15)]) +
    
    # scale_color_brewer(palette = "Blues", direction=-1) +
    theme(text=element_text(size=22),
          strip.text.x = element_text(size=20),
          axis.text = element_text(size=18),
          axis.title = element_text(size=20),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size = 14))+ 
    geom_text(data = river_research_rank[river_research_rank$Period == "2020", ][1:9,], 
              aes(label = river_research_rank$Rank[river_research_rank$Period == "2020"][1:9]),
              hjust = -1.05 ,vjust = 0.5, size = 6) +
    geom_text(data = river_research_rank[river_research_rank$Period == "2020", ][10:26,], 
              aes(label = river_research_rank$Rank[river_research_rank$Period == "2020"][10:26]),
              hjust = -0.5 ,vjust = 0.5, size = 6) 
plot_rank_group_new
