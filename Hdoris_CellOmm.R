#Photoreceptor and ommatidial classes by sex, JB
#Last update: 170804
#Data: H. doris counts 
#-------------------------------------------------------------------------------------

rm(list=ls()) # clears workspace
setwd("~/Dropbox/School/UCI/Briscoe/Projects/Hdoris_UV/Experiments_CellCounts/HdorisCounts_JB/R_JB") #file path 

#load in H doris count data (already subsetted by cell/ommatidia)
  HdorCell.df <-read.csv("Hdoris_CellCounts.csv", header=TRUE) 
  HdorOmm.df <-read.csv("Hdoris_OmmCounts.csv", header=TRUE) 


#Boxplots-------------------------------------------------------------------------------------------------------

  #Note: "Error in grid.Call" means expand the plot box.

  library(plyr)
  library(ggplot2)                      
  library(scales) #Not available (for R version 3.2.3) 
    #color picks = "pink", "skyblue"  


#Boxplot 1: photoreceptor cells
  
dev.new(width=unit(c(40), "cm"), height=unit(c(10), "cm"))

HdorCell.plot <- ggplot(HdorCell.df, aes(x = CellType, y = CellPercent, fill = Sex)) + geom_boxplot() + ylim(0,100) + 
  labs(x="\nPhotoreceptor class", y="Percent (%)\n") + 
  ggtitle(expression(atop("H. doris photoreceptor percentages", atop(italic("\npooled cell counts by sex"), "")))) +
  scale_x_discrete(labels=c(" ", "UV1-UV2", "UV1", "UV2", "B-LW", "B")) +
  
  theme_bw() + theme(panel.grid.major = element_blank()) +

  theme(plot.title = element_text(color="#666666", face="bold", size=32)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=22)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=13),
        axis.text.y = element_text(face="bold", color="#993333", size=16, angle=45) ) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) 

 
HdorCell.plot 
  


#Boxplot 2: ommatidia

dev.new(width=unit(c(200), "cm"), height=unit(c(100), "cm"))

HdorOmm.plot <- ggplot(HdorOmm.df, aes(x = OmmType, y = OmmPercent, fill = Sex)) + geom_boxplot() + ylim(0,100) + 
  labs(x="\nOmmatidia type", y="Percent (%)\n") + 
  ggtitle(expression(atop("H. doris ommatidia percentages", atop(italic("\npooled ommatidia counts by sex"), "")))) +
  scale_x_discrete(labels=c(" ", "UV1-UV2 / UV1-UV2", "UV2 / UV1", "UV2 / UV1- UV2", "B-LW / UV1-UV2", "UV2 / B-LW", "B-LW / B-LW", "B / B-LW", "B/B", "B / UV1", "UV1 / B-LW", "UV1 / UV1", "UV2 / UV2", "UV2 / B", "UV1-UV2 / B", "UV1-UV2 / UV1")) +
  
  theme_bw() + theme(panel.grid.major = element_blank()) +
  
  theme(plot.title = element_text(color="#666666", face="bold", size=32)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=22)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=9),
        axis.text.y = element_text(face="bold", color="#993333", size=16, angle=45) ) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) 


HdorOmm.plot


#Save plots to disk (will overwrite previous)
    
    ggsave("HdorCell.plot", plot = HdorCell.plot, device = png, path = NULL,
           scale = 11, width = 175, height = 150, units = c("cm"),
           dpi = 500, limitsize = FALSE)
    
    ggsave("HdorOmm.plot", plot = HdorOmm.plot, device = png, path = NULL,
           scale = 11, width = 175, height = 150, units = c("cm"),
           dpi = 500, limitsize = FALSE)


#-----------------------------------------------------------------------------
#Analysis
#----------------------------------------------------------------------------- 

#Take a look

  head(HdorCell.df) #cells
  summary(HdorCell.df)
  
  head(HdorOmm.df) #ommatidia
  summary(HdorOmm.df)
  

  # A Z-test was performed to compare pooled male and female proportions for each photoreceptor and ommatidial class. 
  # For the Z-test, the number of photoreceptors sampled was 3280 for males and 1374 for females, 
  # and the number of ommatidia sampled was 2356 for males and 2428 for females.
  # ***P<0.001 between the two groups

    unique(HdorCell.df$Specimen)
    
    #Number of photoreceptors sampled
    with(HdorCell.df, sum(HdorCell.df[Sex=="male", "CellCount"])) #4 males, 3280 cells
    with(HdorCell.df, sum(HdorCell.df[Sex=="female", "CellCount"])) #3 females, 1374 cells
    
    #Number of ommatidia sampled
    with(HdorOmm.df, sum(HdorOmm.df[Sex=="male", "OmmCount"])) #4 males, 1758 omms
    with(HdorOmm.df, sum(HdorOmm.df[Sex=="female", "OmmCount"])) #3 females, 687 omms
      
  
##  Two-Sample t-Test for Equal Means - used to determine if two population means are equal.

##------------------------------------------------

    
  ##Cell counts - check parametric assumptions
  ##-------------------------------------------
    
        #Subsets
          #females
          HdorCellF_UV2.df <- HdorCell.df[with (HdorCell.df, Sex == "female" & CellType == "UV2"), ]
          HdorCellF_UV1.df <- HdorCell.df[with (HdorCell.df, Sex == "female" & CellType == "UV1"), ]
          HdorCellF_B.df <- HdorCell.df[with (HdorCell.df, Sex == "female" & CellType == "B"), ]
          #males
          HdorCellM_UV2.df <- HdorCell.df[with (HdorCell.df, Sex == "male" & CellType == "UV2"), ]
          HdorCellM_UV1.df <- HdorCell.df[with (HdorCell.df, Sex == "male" & CellType == "UV1"), ]
          HdorCellM_B.df <- HdorCell.df[with (HdorCell.df, Sex == "male" & CellType == "B"), ]

  #F test to compare two variances. Verify the homoskedasticity (homogeneity of variances).
    #If we obtain p-value greater than 0.05, then we can assume that the two variances are homogeneous. 
      var.test(HdorCellF_UV2.df$CellPercent, HdorCellM_UV2.df$CellPercent) #UV2 p-value = 0.6174
      var.test(HdorCellF_UV1.df$CellPercent, HdorCellM_UV1.df$CellPercent) #UV1 p-value = 0.3271
      var.test(HdorCellF_B.df$CellPercent, HdorCellM_B.df$CellPercent) #B p-value = 0.07184
    
      
  #Shapiro-Wilk test of normality ---- 
    #tests the Null hypothesis that "the samples come from a Normal distribution" 
    #against the alternative hypothesis "the samples do not come from a Normal distribution".
      
    shapiro.test(HdorCellF_UV2.df$CellPercent); shapiro.test(HdorCellM_UV2.df$CellPercent) #p-value = 0.5074, p-value = 0.5255
    shapiro.test(HdorCellF_UV1.df$CellPercent); shapiro.test(HdorCellM_UV1.df$CellPercent) #p-value = 0.2175, p-value = 0.5982
    shapiro.test(HdorCellF_B.df$CellPercent); shapiro.test(HdorCellM_B.df$CellPercent) #p-value = 0.2351, p-value = 0.778
    
      
    
   ##Omm counts - check parametric assumptions
   ##----------------------------------------- 
      
          #Subsets
            #females
            HdorOmmF_UV2UV2.df <- HdorOmm.df[with (HdorOmm.df, Sex == "female" & OmmType == "UV2/UV2"), ]
            HdorOmmF_UV2B.df <- HdorOmm.df[with (HdorOmm.df, Sex == "female" & OmmType == "UV2/B"), ]
            HdorOmmF_BB.df <- HdorOmm.df[with (HdorOmm.df, Sex == "female" & OmmType == "B/B"), ]
            HdorOmmF_UV1UV1.df <- HdorOmm.df[with (HdorOmm.df, Sex == "female" & OmmType == "UV1/UV1"), ]
            HdorOmmF_UV1UV2.df <- HdorOmm.df[with (HdorOmm.df, Sex == "female" & OmmType == "UV1/UV2"), ]
            HdorOmmF_UV1B.df <- HdorOmm.df[with (HdorOmm.df, Sex == "female" & OmmType == "UV1/B"), ]
            #males
            HdorOmmM_UV2UV2.df <- HdorOmm.df[with (HdorOmm.df, Sex == "male" & OmmType == "UV2/UV2"), ]
            HdorOmmM_UV2B.df <- HdorOmm.df[with (HdorOmm.df, Sex == "male" & OmmType == "UV2/B"), ]
            HdorOmmM_BB.df <- HdorOmm.df[with (HdorOmm.df, Sex == "male" & OmmType == "B/B"), ]
            HdorOmmM_UV1UV1.df <- HdorOmm.df[with (HdorOmm.df, Sex == "male" & OmmType == "UV1/UV1"), ]
            HdorOmmM_UV1UV2.df <- HdorOmm.df[with (HdorOmm.df, Sex == "male" & OmmType == "UV1/UV2"), ]
            HdorOmmM_UV1B.df <- HdorOmm.df[with (HdorOmm.df, Sex == "male" & OmmType == "UV1/B"), ]
            
    #F test to compare two variances. Verify the homoskedasticity (homogeneity of variances).
      #If we obtain p-value greater than 0.05, then we can assume that the two variances are homogeneous. 
        var.test(HdorOmmF_UV2UV2.df$OmmPercent, HdorOmmM_UV2UV2.df$OmmPercent) #UV2/UV2 p-value < 2.2e-16
        var.test(HdorOmmF_UV2B.df$OmmPercent, HdorOmmM_UV2B.df$OmmPercent) #UV2/B p-value = 0.1562
        var.test(HdorOmmF_BB.df$OmmPercent, HdorOmmM_BB.df$OmmPercent) #B/B p-value = 0.192
        var.test(HdorOmmF_UV1UV1.df$OmmPercent, HdorOmmM_UV1UV1.df$OmmPercent) #UV1/UV1 p-value = 0.1494
        var.test(HdorOmmF_UV1UV2.df$OmmPercent, HdorOmmM_UV1UV2.df$OmmPercent) #UV1/UV2 p-value < 2.2e-16
        var.test(HdorOmmF_UV1B.df$OmmPercent, HdorOmmM_UV1B.df$OmmPercent) #UV1/B p-value = 0.1629
        
 
      #Shapiro-Wilk test of normality ---- 
        #tests the Null hypothesis that "the samples come from a Normal distribution" 
        #against the alternative hypothesis "the samples do not come from a Normal distribution".
        
        shapiro.test(HdorOmmF_UV2UV2.df$OmmPercent); shapiro.test(HdorOmmM_UV2UV2.df$OmmPercent) #p-value = 0.4084 BLANK
        shapiro.test(HdorOmmF_UV2B.df$OmmPercent); shapiro.test(HdorOmmM_UV2B.df$OmmPercent) #p-value = 0.4066, p-value = 0.9749
        shapiro.test(HdorOmmF_BB.df$OmmPercent); shapiro.test(HdorOmmM_BB.df$OmmPercent) #p-value = 0.12, p-value = 0.288
        shapiro.test(HdorOmmF_UV1UV1.df$OmmPercent); shapiro.test(HdorOmmM_UV1UV1.df$OmmPercent) #p-value = 0.85, p-value = 0.9891
        shapiro.test(HdorOmmF_UV1UV2.df$OmmPercent); shapiro.test(HdorOmmM_UV1UV2.df$OmmPercent) #p-value = 0.678 BLANK
        shapiro.test(HdorOmmF_UV1B.df$OmmPercent); shapiro.test(HdorOmmM_UV1B.df$OmmPercent) #p-value = 0.6986, p-value = 0.2692       

        
        
#-----------------------------------                   
#Mann-Whitney test (nonparametric) 
 #Using the Mann-Whitney-Wilcoxon Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution.
  
  
#Cell counts - test for sex differences
#--------------------------------------

        # independent 2-group Mann-Whitney U Test 
        wilcox.test(y~A) 
        # where y is numeric and A is A binary factor
