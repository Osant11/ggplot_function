library(haven)
library(ggplot2)
library(plyr)
library(dplyr)
library(shiny)
library(tidyr)
library(knitr)
library(stringr)
library(DT)
library(tidyverse)
library(plyr)
library(shinyWidgets)
library(jsonlite)
library(gmodels)
library(gridExtra)
library(grid)
library(egg)
library(ggpubr)
library(foreign)
library(writexl)
library("openxlsx")


### Load dataset ### 

trt_n <- data1_sum %>% 
  group_by(id_num) %>% 
  filter(!duplicated(id_num)) %>% 
  group_by(TRT) %>% 
  dplyr::summarise(n_trt = n())




### FUNCTION FOR SUMMARY STATISTICS ###
quanti_stat_visit_plot <- function(dataset, group, var){
  
  dataset %>% 
    group_by_(group, as.symbol("VISIT"), as.symbol("VISIT_NUM"), as.symbol("xaxis")) %>% 
    dplyr::summarise(n = n(), 
              mean = round(mean(!!sym(var), na.rm = T), 3),
              sum = round(sum(!!sym(var), na.rm = T), 1), 
              st = round(sd(!!sym(var), na.rm = T), 3),
              Median = round(median(!!sym(var), na.rm = T), 2),
              Q1 = round(quantile(!!sym(var), na.rm = T, 0.25, type = 2), 2),
              Q3 = round(quantile(!!sym(var), na.rm = T, 0.75, type = 2), 2),
              min = round(min(!!sym(var), na.rm = T), 2),
              max = round(max(!!sym(var), na.rm = T), 2)) %>%  
    ungroup() %>% 
    mutate(mean = ifelse(is.nan(mean), 0, mean), 
           se = round(st / sqrt(n), 2),
           lower.ci = ifelse(is.na(se), 0,round(mean - qt(1 - (0.05 / 2), n - 1) * se, 2)),
           upper.ci = ifelse(is.na(se), 0,round(mean + qt(1 - (0.05 / 2), n - 1) * se, 2)), 
           lowsd = mean - st, 
           uppsd = mean + st, 
           lowse = ifelse(is.na(se), 0, mean - se), 
           uppse = ifelse(is.na(se), 0, mean + se)
    )
}



### GGPLOT FUNCTION ###
macro_plot <- function(dataset, xaxis, yaxis, group, col, nsub, grporder, 
                       ymin, ymax, xlabels, position, yline, yline_alpha, 
                       ylimits, ybreaks, color_grp, xlab, ylab, 
                       titles, footnotes){
  
  p <- ggplot(dataset, aes_string(x = xaxis, y = yaxis, group = group, col = col, shape = group, linetype = group)) +  
    geom_errorbar(aes_string(ymin = ymin, ymax = ymax), position = position, linetype = "solid", show.legend = F) +
    geom_line(data = dataset %>% filter(xaxis <= 254), position = position) + 
    geom_line(data = dataset %>% filter(xaxis > 254), position = position) +
    geom_point(position = position) +
    geom_hline(yintercept = yline, alpha = yline_alpha) +
    scale_x_discrete(labels = xlabels) +
    scale_y_continuous(limits = ylimits, 
                       breaks = ybreaks) +
    scale_color_manual(values = color_grp) +
    scale_linetype_manual(values=c("dashed", "solid", "dotdash")) +
    scale_shape_manual(values=c(0, 1, 2)) +
    xlab(xlab) + 
    ylab(ylab) +
    theme_linedraw() +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 10),
          legend.title = element_blank(),
          legend.text=element_text(size=9),
          legend.justification = c(0.5, 0.05), 
          legend.position = c(0.5, 0.85),
          legend.direction="horizontal", 
          legend.key = element_blank(), 
          legend.background = element_rect(fill="transparent"),
          axis.text.x = element_text(angle = -45, hjust = 0),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          plot.margin =  unit(c(0.5,0,-0.5,0), "lines")
    )
  
  df.table <- ggplot(dataset, aes_string(x = xaxis, y = group,
                                         label = nsub, colour = group)) +
    geom_text(size = 3) + 
    scale_y_discrete(limits= grporder) +
    scale_color_manual(values = color_grp) +
    theme_minimal() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          panel.border = element_blank(), 
          axis.text.x =  element_blank(),
          axis.ticks =  element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(), 
          plot.margin =  unit(c(-0.3,0,0,0), "lines")
    )


  df1 <- dataset %>%
    arrange_(group, "xaxis") %>%
    filter(!duplicated(xaxis)) %>%
    mutate(Subjects1 = ifelse(!duplicated(!!sym(group)), "Number of subjects", ""))
  
  df.table1 <- ggplot(df1, aes(x = xaxis, y = group, label = Subjects1)) +
    geom_text(size = 3.5) + 
    scale_color_manual(values = c("Black")) +
    scale_x_discrete(expand = c(0.09,0)) +
    theme_minimal() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          panel.border = element_blank(), 
          axis.text.x =  element_blank(),
          axis.ticks =  element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(), 
          plot.margin = unit(c(0,0.2,0,0.2), "cm")
    )
  
  c <- ggpubr::ggarrange(p, df.table1, df.table, ncol = 1, nrow = 3, heights = c(17, 1.2, 2), align = "v")
  
  grid.arrange(c, bottom = textGrob(footnotes,
                                    x = 0.05, hjust = 0, gp = gpar(fontface = 3L, fontsize = 9)),
               top = textGrob(titles,
                              x = 0.05, hjust = 0, gp = gpar(fontface = 3L, fontsize = 9))
  )
  
}


###################################### PLOT VALUE ######################################
###################################### PLOT VALUE ######################################
###################################### PLOT VALUE ######################################
###################################### PLOT VALUE ######################################


data1_sum_1 <- quanti_stat_visit_plot(data1_sum, "TRT", "VALUE") %>% 
  mutate(yaxis = case_when(
    TRT == "Treatment A" ~ 1, 
    TRT == "Treatment B" ~ 2,
    TRT == "Treatment C" ~ 3
  ), 
  xaxis_char = case_when(
    xaxis == 5000 ~ "LOT", 
    xaxis == 10005 ~ "FU7",
    xaxis == 10017 ~ "FU30",
    xaxis == 10046 ~ "FU90",
    xaxis == 15000 ~ "LFU",
    TRUE ~ as.character(xaxis)
  )
  ) %>% 
  arrange(TRT, xaxis) %>% 
  left_join(trt_n, by = "TRT") %>% 
  mutate(TRT = paste(TRT, " (N = ", n_trt, ")", sep = ""))
  


p<- macro_plot(dataset = data1_sum_1, xaxis = "as.factor(xaxis)", yaxis = "mean", 
               group = "TRT", col = "TRT", 
               nsub = "n", 
               grporder = c("Treatment C (N = 98)", "Treatment B (N = 103)", "Treatment A (N = 99)"),
               xlabels = data1_sum_1$xaxis_char,
               ymin = "lowse", ymax = "uppse", position = position_dodge(0.50), 
               ylimits = c(92.5, 101), 
               ybreaks = c(seq(92.5, 100, 2.5)), 
               yline = NULL, yline_alpha = NULL, 
               color_grp = c("Blue2", "Red2", "Green4"), 
               xlab = "Analysis visit (Week)", 
               ylab = "Mean (variable of interest)", 
               titles = paste("Mean (plus/minus standard error) of (variable of interest), by analysis visit",
                              "Analysis Set: (Analysis Set)", sep = "\n"),
               footnotes = paste("Footnote 1", 
                                 "Footnote 2",
                                 paste("Produced on:", format(Sys.time(), format = "%F %R %Z", usetz=F)),
                                 sep = "\n")
)



