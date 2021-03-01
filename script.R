library(readr)
data_raw <- read_csv("F:/Dropbox/HCID/Exp. Design/Log Files/all.csv")
data_raw[6] <- lapply(data_raw, as.character)
data_raw[data_raw == 'Direction_Parallelism'] <- "Combination"

# data cleaning
data <- subset(data_raw,select = -c(DesignName,Trial))

# assign var
time <- data$visualSearchTime
id <- data$TrialID
vv <- data$VV
oc <- data$OC
participant <- data$ParticipantID
error <- data$ErrorCount

# assign font
library(showtext)
font_add(family = "Libertine", regular = "G:\\linux_libertine\\LinLibertine_R.ttf", bold = "G:\\linux_libertine\\LinLibertine_RB.ttf", italic = "G:\\linux_libertine\\LinLibertine_RI.ttf" , bolditalic = "G:\\linux_libertine\\LinLibertine_RBI.ttf")
showtext_auto() 






# identity outlier
library(rstatix)
data %>%
  group_by(data$visualSearchTime) %>%
  identify_outliers(visualSearchTime) # no extreme outliers

# 
library(ez)
ezStats(data=data,dv=ErrorCount,wid=.(ParticipantID),within=.(VV,OC),type=1)
summary(data)
colSums(data != 0)





### plot
library(ggplot2)

# scatter plot
#ggplot(
#  data_filtered, 
#  aes(
#    x=OC, 
#    y=visualSearchTime,
#    color = VV,
#    )
#  ) + 
#  geom_point()

# box plot for OC
library(forcats)
data %>%
  mutate(OC = fct_relevel(OC, "Small","Medium","Large")) %>%
  ggplot(
    ., 
    aes(
      x=OC, 
      y=visualSearchTime,
      add = "point",
      #color = OC,
      )
    ) + 
    geom_boxplot(varwidth=T, aes(fill=OC))+
    theme_stata()+
    theme(
      text = element_text(size=14 , family = "Libertine"),
      legend.title =  element_blank(),
      legend.position = c(0.8, 0.8),
      plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
      plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
      axis.title.x = element_text(vjust = -0.5),
      axis.title.y = element_text(vjust = 2),
      plot.margin = unit(c(10, 20, 10, 20), "pt"),
    ) + 
    labs(
      #title="Visual Search Time", 
      #subtitle="subtitle",
         #caption="Source: mpg",
         x="Object Count (OC)",
         y="Visual Search Time (ms)")

# box plot for VV
library(forcats)
data %>%
  mutate(VV = fct_relevel(VV, "Direction","Parallelism","Combination")) %>%
  ggplot(
    ., 
    aes(
      x=VV, 
      y=visualSearchTime,
      add = "point",
      #color = OC,
    )
  ) + 
  geom_boxplot(varwidth=T, aes(fill=VV))+
  theme_stata()+
  theme(
    text = element_text(size=14 , family = "Libertine"),
    legend.title =  element_blank(),
    legend.position = c(0.8, 0.8),
    plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
    plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2),
    plot.margin = unit(c(10, 20, 10, 20), "pt"),
  ) + 
  labs(
    #title="Visual Search Time", 
    #subtitle="subtitle",
    #caption="Source: mpg",
    x="Visual Variables (VV)",
    y="Visual Search Time (ms)")






### Test H3

## two-way repeated ANOVA
library(ez)
summary_two <- ezStats(data=data,dv=.(visualSearchTime),wid=.(ParticipantID),within=.(VV,OC),type=1)
anova_two <- ezANOVA(data=data,dv=.(visualSearchTime),wid=.(ParticipantID),within=.(VV,OC),detailed=TRUE,type=1)


ezStats(data=data,dv=.(visualSearchTime),wid=.(ParticipantID),within=.(VV),type=1)
summarySEwithin(data, measurevar="visualSearchTime",withinvars=c("OC", "VV"), idvar="ParticipantID")

## By Two-way repeated AnOVA, VV is significant

# Post-hoc test for VV
postHocVV <- pairwise.t.test(data$visualSearchTime,data$VV,paired=TRUE)

library(xtable)
xtable(ezANOVA(data=data,dv=.(visualSearchTime),wid=.(ParticipantID),within=.(VV,OC),detailed=TRUE,type=1))
print(xtable(anova_two))
print.xtable(anova_two)



## Given a level of OC, VV has a simple effect
# create data frame by OC condition
data_oc_small <- subset(data, oc == 'Small', select = -c(OC))
data_oc_medium <- subset(data, oc == 'Medium', select = -c(OC))
data_oc_large <- subset(data, oc == 'Large', select = -c(OC))
# One-way ANOVA to see the result # all significant
anova_vv_small <- ezANOVA(data=data_oc_small,dv=.(visualSearchTime), wid=.(ParticipantID), within =.(VV),detailed=TRUE)
anova_vv_medium <- ezANOVA(data=data_oc_medium,dv=.(visualSearchTime), wid=.(ParticipantID), within =.(VV),detailed=TRUE)
anova_vv_large <- ezANOVA(data=data_oc_large,dv=.(visualSearchTime), wid=.(ParticipantID), within =.(VV),detailed=TRUE)
# post-hoc for each OC condition
posthoc_vv_small <- pairwise.t.test(data_oc_small$visualSearchTime,data$VV,paired=TRUE) # no effect
posthoc_vv_medium <- pairwise.t.test(data_oc_medium$visualSearchTime,data$VV,paired=TRUE) # no effect
posthoc_vv_large <- pairwise.t.test(data_oc_large$visualSearchTime,data$VV,paired=TRUE) # no effect
## ???



### Test H1,H2


## one-way repeated ANOVA
anova_vv <- ezANOVA(data=data,dv=.(visualSearchTime), wid=.(ParticipantID), within =.(VV),detailed=TRUE) # significant
anova_oc <- ezANOVA(data=data,dv=.(visualSearchTime), wid=.(ParticipantID), within =.(OC),detailed=TRUE) # for hypothesis, not significant



## one-way repeated ANOVA for different group of VV

# create data frame fro each VV
data_vv_direction <- subset(data, vv == 'Direction', select = -c(VV))
data_vv_parallelism <- subset(data, vv == 'Parallelism', select = -c(VV))
data_vv_combination <- subset(data, vv == 'Parallelism + Direction', select = -c(VV))
data_vv_combinationRv <- subset(data_vv_combination, ! vvCombination$TrialID == 192) 
# ANOVA
anova_oc_combination <- ezANOVA(data=data_vv_Combination,dv=.(visualSearchTime), wid=.(ParticipantID), within =.(OC),detailed=TRUE) # not significant
anova_oc_direction <- ezANOVA(data=data_vv_direction,dv=.(visualSearchTime), wid=.(ParticipantID), within =.(OC),detailed=TRUE) # not significant
anova_oc_parallelism <- ezANOVA(data=data_vv_parallelism,dv=.(visualSearchTime), wid=.(ParticipantID), within =.(OC),detailed=TRUE) # significant


## futher explore df of parallelism

# post-hoc
posthoc_oc_parallelism <- pairwise.t.test(data_vv_parallelism$visualSearchTime,data$OC,paired=TRUE) # no effect
# summary
library(Rmisc)
summary_oc_parallelism <- summarySEwithin(data_vv_parallelism, measurevar="visualSearchTime",withinvars=c("OC"), idvar="ParticipantID")
# sort by visualSearchTime
library(dplyr)
summary_oc_parallelism <- summary_oc_parallelism %>% arrange(visualSearchTime)
# create bar chart
library(ggplot2)
library(dplyr)
summary_oc_parallelism %>%
  mutate(OC=factor(OC, levels=OC)) %>%
  ggplot(., aes(x=OC, y=visualSearchTime, fill=OC)) + 
  geom_bar(stat="identity" , width = .7) +
  geom_errorbar(aes(ymin=visualSearchTime-ci, ymax=visualSearchTime+ci), width=.1 ) +
  geom_text(
    label = round(summary_oc_parallelism$visualSearchTime,digits = 0),
    nudge_x = 0.25, nudge_y =200, 
    aes(color=OC,family = "Libertine"),
  ) +
  theme(
    text = element_text(size=14 , family = "Libertine"), 
    legend.position = "bottom",
    plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
    plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2),
    plot.margin = unit(c(10, 20, 10, 20), "pt"),
  ) + 
  labs( 
    title = "Mean Visual Search Time ", 
    subtitle = "Visual Variable = Parallelism", 
    y = "Mean Time (ms)" , 
    x = "Object Count (OC)"
  )








# summary within VV
library(Rmisc)
summary_withinVV <- summarySEwithin(data, measurevar="visualSearchTime",withinvars=c("VV"), idvar="ParticipantID")
# sort by visualSearchTime
library(dplyr)
summary_withinVV <- summary_withinVV %>% arrange(visualSearchTime)


summary_vv <- summarySEwithin(data, measurevar="visualSearchTime",withinvars=c("VV"), idvar="ParticipantID")
summary_vv <- summary_vv %>% arrange(visualSearchTime)


library(ggplot2)
library(dplyr)
summary_vv %>%
  #arrange(visualSearchTime) %>%
  mutate(VV=factor(VV, levels=VV)) %>%
  ggplot(., aes(x=VV, y=visualSearchTime, fill=VV)) + 
  geom_bar(stat="identity" , width = .7) +
  geom_errorbar(aes(ymin=visualSearchTime-ci, ymax=visualSearchTime+ci), width=.1 ) +
  geom_text(
    label = round(summary_withinVV$visualSearchTime,digits = 2),
    nudge_x = 0.22, nudge_y =500, 
    aes(color=VV,family = "Libertine"),
  ) +
  theme(
    text = element_text(size=14 , family = "Libertine"), 
    legend.position = c(0.16, 0.8),
    #legend.title =  element_text( size = 12),
    legend.text = element_text(size = 10, face="italic"),
    legend.key.size = unit(.5,"line"),
    plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
    plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2),
    plot.margin = unit(c(10, 20, 10, 20), "pt"),
  ) + 
  labs( 
    #title = "Mean Visual Search Time", 
    #subtitle = "by Visual Variable Condition", 
    y = "Mean Time (ms)" , 
    x = "Visual Variable (VV)",
  ) 







# create the plot based on summary_withinVV
#library(forcats)
#summary_withinVV %>%
#mutate(VV = fct_reorder(VV,visualSearchTime)) %>%
#ggplot(aes(x=VV, y=visualSearchTime, fill=VV)) + 
#  geom_bar(stat="identity") +
#  geom_errorbar(aes(ymin=visualSearchTime-ci, ymax=visualSearchTime+ci), width=.2 ) +
#  theme(
#    text = element_text(size=18), 
#    legend.position = "bottom") + 
#  labs( 
#    title = "Visual Search Time", 
#    subtitle = "per Visual Variable Condition", 
#    y = "Visual Search Time" , 
#    x = "Visual Variable"
#    )

library(ggthemes)
library(ggplot2)
library(dplyr)

# bar char per VV
summary_withinVV %>%
  #arrange(visualSearchTime) %>%
  mutate(VV=factor(VV, levels=VV)) %>%
  ggplot(., aes(x=VV, y=visualSearchTime, fill=VV)) + 
  geom_bar(stat="identity" , width = .7) +
  geom_errorbar(aes(ymin=visualSearchTime-ci, ymax=visualSearchTime+ci), width=.1 ) +
  geom_text(
    label = round(summary_withinVV$visualSearchTime,digits = 2),
    nudge_x = 0.2, nudge_y =500, 
    aes(color=VV,family = "Libertine"),
    size= 3,
  ) +
  theme(
    text = element_text(size=14 , family = "Libertine"), 
    legend.position = "bottom",
    legend.title =  element_blank(),
    legend.text = element_text(size = 10, face="italic"),
    plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
    plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2),
    plot.margin = unit(c(30, 20, 10, 20), "pt"),
    ) + 
  labs( 
    #title = "Mean Visual Search Time", 
    #subtitle = "by Visual Variable Condition", 
    y = "Mean Time (ms)" , 
    x = "Visual Variable (VV)"
  )

# multiple bar chart per two factors
summary_two <- summarySEwithin(data, measurevar="visualSearchTime",withinvars=c("VV","OC"), idvar="ParticipantID")
summary_two <- summary_two %>% arrange(visualSearchTime)
summary_two %>%
  #mutate(VV=factor(VV, levels=VV)) %>%
  ggplot(., aes(x=VV, y=visualSearchTime, fill=VV)) + 
  geom_bar(stat="identity" , width = .7, position=position_dodge()) +
  geom_errorbar(aes(ymin=visualSearchTime-ci, ymax=visualSearchTime+ci), width=.2, position=position_dodge(.9) ) +
  geom_text(
    label = round(summary_withinVV$visualSearchTime,digits = 2),
    nudge_x = 0.2, nudge_y =500, 
    aes(color=VV,family = "Libertine"),
    size= 3,
  ) +
  theme(
    text = element_text(size=14 , family = "Libertine"), 
    legend.position = "bottom",
    legend.title =  element_blank(),
    legend.text = element_text(size = 10, face="italic"),
    plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
    plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2),
    plot.margin = unit(c(30, 20, 10, 20), "pt"),
  ) + 
  labs( 
    #title = "Mean Visual Search Time", 
    #subtitle = "by Visual Variable Condition", 
    y = "Mean Time (ms)" , 
    x = "Visual Variable (VV)"
  )


# multiple bar chart per two factors
summary_two %>%
  mutate(VV=factor(VV, levels=VV)) %>%
  ggplot(., aes(x=VV, y=visualSearchTime, fill=VV)) + 
  geom_bar(stat="identity" , width = .7, position=position_dodge()) +
  geom_errorbar(aes(ymin=visualSearchTime-ci, ymax=visualSearchTime+ci), width=.2, position=position_dodge(.9) ) +
  geom_text(
    label = round(summary_withinVV$visualSearchTime,digits = 2),
    nudge_x = 0.2, nudge_y =500, 
    aes(color=VV,family = "Libertine"),
    size= 3,
  ) +
  theme(
    text = element_text(size=14 , family = "Libertine"), 
    legend.position = "bottom",
    legend.title =  element_blank(),
    legend.text = element_text(size = 10, face="italic"),
    plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
    plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2),
    plot.margin = unit(c(30, 20, 10, 20), "pt"),
  ) + 
  labs( 
    #title = "Mean Visual Search Time", 
    #subtitle = "by Visual Variable Condition", 
    y = "Mean Time (ms)" , 
    x = "Visual Variable (VV)"
  )

library(dplyr)
summary_vv %>%
  mutate(VV=factor(VV, levels=VV)) %>%
  ggplot(., aes(x=VV, y=visualSearchTime, fill=VV)) + 
  #geom_bar(stat="identity" , width = .7, position=position_dodge()) +
  geom_boxplot(aes(fill = dose))
  #geom_errorbar(aes(ymin=visualSearchTime-ci, ymax=visualSearchTime+ci), width=.2, position=position_dodge(.9) ) +
  geom_text(
    label = round(summary_withinVV$visualSearchTime,digits = 2),
    nudge_x = 0.2, nudge_y =500, 
    aes(color=VV,family = "Libertine"),
    size= 3,
  ) +
  theme(
    text = element_text(size=14 , family = "Libertine"), 
    legend.position = "bottom",
    legend.title =  element_blank(),
    legend.text = element_text(size = 10, face="italic"),
    plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
    plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2),
    plot.margin = unit(c(30, 20, 10, 20), "pt"),
  ) + 
  labs( 
    #title = "Mean Visual Search Time", 
    #subtitle = "by Visual Variable Condition", 
    y = "Mean Time (ms)" , 
    x = "Visual Variable (VV)"
  )
  
# bar char per VV
  summary_withinVV %>%
    #arrange(visualSearchTime) %>%
    mutate(VV=factor(VV, levels=VV)) %>%
    ggplot(., aes(x=VV, y=visualSearchTime)) + 
    geom_boxplot(aes(fill=VV)) +
    geom_text(
      label = round(summary_withinVV$visualSearchTime,digits = 2),
      nudge_x = 0.2, nudge_y =500, 
      aes(color=VV,family = "Libertine"),
      size= 3,
    ) +
    theme(
      text = element_text(size=14 , family = "Libertine"), 
      legend.position = "bottom",
      legend.title =  element_blank(),
      legend.text = element_text(size = 10, face="italic"),
      plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
      plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
      axis.title.x = element_text(vjust = -0.5),
      axis.title.y = element_text(vjust = 2),
      plot.margin = unit(c(30, 20, 10, 20), "pt"),
    ) + 
    labs( 
      #title = "Mean Visual Search Time", 
      #subtitle = "by Visual Variable Condition", 
      y = "Mean Time (ms)" , 
      x = "Visual Variable (VV)"
    )

  
  
  # Final Version of multiplot
  
  data_summary %>% 
    
    mutate(OC = factor(OC, levels = c("Small","Medium","Large")),VV = factor(VV,levels = c("Parallelism","Direction","Combination"))) %>%
    
    
    
    ggplot(., aes(x=VV, y=visualSearchTime, fill=OC)) +
    # plot data as is using a bar plot layer (use position_dodge to display conditions side-by-side)
    geom_bar(stat="identity", position=position_dodge()) +
    # plot error bar layer using the confidence intervals
    geom_errorbar(aes(ymin=visualSearchTime-ci, ymax=visualSearchTime+ci), width=.2, position=position_dodge(.9)) +
    # make text smaller
    geom_text(
      label = round(data_summary$visualSearchTime,digits = 2),
      aes(color=OC,
          family = "Libertine"),
      size= 3,
      check_overlap = TRUE,
      vjust= -3,
      position=position_dodge(width = 1),
    ) +
    theme_stata()+
    theme(
      text = element_text(size=14 , family = "Libertine"), 
      legend.position = c(0.15,0.8),
      legend.title =  element_blank(),
      legend.text = element_text(size = 10, face="italic"),
      plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
      plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
      axis.title.x = element_text(vjust = -0.5),
      axis.title.y = element_text(vjust = 2),
      plot.margin = unit(c(30, 20, 10, 20), "pt"),
    ) + 
    labs( 
      #title = "Mean Visual Search Time", 
      #subtitle = "by Visual Variable Condition", 
      y = "Visual Search Time (ms)" , 
      x = "Visual Variable (VV)"
    )

  
#ditribution plot
  
  library(ggplot2)
  ggplot(data, aes(x=visualSearchTime)) +
    geom_density() +
    theme(
      text = element_text(size=14 , family = "Libertine"), 
      legend.position = "bottom",
      legend.title =  element_blank(),
      legend.text = element_text(size = 10, face="italic"),
      plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
      plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
      axis.title.x = element_text(vjust = -0.5),
      axis.title.y = element_text(vjust = 2),
      plot.margin = unit(c(30, 20, 10, 20), "pt"),
    )  + 
    labs( 
      #title = "Mean Visual Search Time", 
      #subtitle = "by Visual Variable Condition", 
      y = "Density" , 
      x = "Visual Search Time (ms)"
    )
  
  
  ggplot(data, aes(x=visualSearchTime, color=OC)) +
    geom_density()+
    theme(
      text = element_text(size=14 , family = "Libertine"), 
      legend.position = "bottom",
      legend.title =  element_blank(),
      legend.text = element_text(size = 10, face="italic"),
      plot.title = element_text(size = 20, face="bold", margin = margin(10,0,5,0), hjust = 0.5),
      plot.subtitle = element_text(size = 14,margin = margin(0,0,10,0), hjust = 0.5),
      axis.title.x = element_text(vjust = -0.5),
      axis.title.y = element_text(vjust = 2),
      plot.margin = unit(c(30, 20, 10, 20), "pt"),
    )  + 
    labs( 
      #title = "Mean Visual Search Time", 
      #subtitle = "by Visual Variable Condition", 
      y = "Density" , 
      x = "Visual Search Time (ms)"
    )
  
  
