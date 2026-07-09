# An approach for visualizing rank factors
# 2026-07-09
# Tim Howard, New York Natural Heritage Program
# tghoward@esf.edu
# accompanies manuscript
## 
##

library(ggplot2)
library(stringr)
library(patchwork)

# get ranking spreadsheet
fn <- "pollinator_ranks_nynhp.csv"
rank_dat <- read.csv(fn)
# explore data
head(rank_dat)
table(rank_dat$SRank, useNA = "ifany")
names(rank_dat)

# just get relevant columns to clean up a bit
keepCols <- c("SRank",  
             "ScientificName", 
             "MaxRecords", 
             "RangeExtent", "AreaOfOccupancy", "NumberOccurrences", 
             "ThreatImpact", "IntrinsicVulnerability", "LongTermTrend")
allSRanks <- rank_dat[,keepCols]
names(allSRanks) <- c("srank", "sciname","MaxRecords","RangeExtent",
                "AOO","NumOccur",
                "ThreatImpact","IntrinVulner","LongTTrend")
head(allSRanks)

# remove the SNA (==Not Applicable subnational ranks)
table(allSRanks$srank, useNA = "ifany")
allSRanks <- allSRanks[!allSRanks$srank %in% "SNA",]
table(allSRanks$srank, useNA = "ifany")

## Range Extent figure ----
table(allSRanks$RangeExtent, useNA = "ifany")

# do in reverse order to rarer score is kept.
allSRanks$srankgrp <- NA
allSRanks[grepl("S5",allSRanks$srank),"srankgrp"] <- "S5"
allSRanks[grepl("S4",allSRanks$srank),"srankgrp"] <- "S4"
allSRanks[grepl("S3",allSRanks$srank),"srankgrp"] <- "S3"
allSRanks[grepl("S2",allSRanks$srank),"srankgrp"] <- "S2"
allSRanks[grepl("S1",allSRanks$srank),"srankgrp"] <- "S1"
allSRanks[grepl("SU",allSRanks$srank),"srankgrp"] <- "SU"
allSRanks[grepl("SH",allSRanks$srank),"srankgrp"] <- "SH"

table(allSRanks$srankgrp, useNA = "ifany")
allSRanks$srankgrp <- factor(allSRanks$srankgrp, levels = c("SH","S1","S2","S3","S4","S5","SU"))

# extract out the min rank from the RangeExtent col
allSRanks$RE_min <- str_sub(allSRanks$RangeExtent,1,1)
# extract out the max rank from the RangeExtent col
allSRanks$RE_max <- str_sub(allSRanks$RangeExtent,-1,-1)

table(allSRanks$RE_max, useNA = "ifany")
table(allSRanks$RE_min, useNA = "ifany")

# define them as factors
factorScaleForRE <- c("Z","A","B","C","D","E","F","G","U")
allSRanks$RE_min <- as.numeric(factor(allSRanks$RE_min, 
                                levels = factorScaleForRE))
allSRanks$RE_max <- as.numeric(factor(allSRanks$RE_max, 
                                levels = factorScaleForRE))
table(allSRanks$srank, useNA = "ifany")
allSRanks$srank <- factor(allSRanks$srank, 
            levels = c("SH","S1","S1S2","S1S3","S2","S2?","S2S3","S2S4","S3",
                       "S3?","S3S4", "S3S5", "S4", "S4S5", "S5",  "SU"))

# order by S Rank, then RangeExtent for the first figure
allSRanks <- allSRanks[order(allSRanks$srank, allSRanks$RangeExtent),]

# define a row number for plotting Y axis
allSRanks$rowNum <- nrow(allSRanks):1
# widen mins and maxes so a bar will show up on graph
allSRanks$RE_min <- allSRanks$RE_min - 0.35
allSRanks$RE_max <- allSRanks$RE_max + 0.35
allSRanks$RE_mid <- (allSRanks$RE_min + allSRanks$RE_max)/2

# Customize the colors
# each row is an S-rank group (row1 = S1, S1S2, S1S3)
# these sets from colorbrewer
custColors <- c("#00008B", #SH
                "#ce1256","#df65b0","#d7b5d8", #S1s
                "#08519c","#3182bd","#6baed6","#bdd7e7", #S2s
                "#006d2c", "#31a354","#74c476","#bae4b3", #S3s
                "#e6550d", "#fdae6b", #S4s
                "grey2", #S5
                "yellow4") #SU
xlabels <- c("0 km2","<100 km2",
"100-250 km2","250-1000 km2",
"1000-5000 km2","5000-20,000 km2",
  "20,000-200,000 km2","200,000-2.5M km2","Unknown")
# change to en-dash
x_labels_endash <- sub("-","\\\u2013",xlabels)

# get counts for the legend
allSRankCounts <- aggregate(allSRanks[,c("srank")], by = list(allSRanks$srank), FUN = "length" )
names(allSRankCounts) <- c("srank","count")
allSRankCounts$col <- "H"
allSRankCounts$merge <- paste0(allSRankCounts$srank," (",allSRankCounts$count,")")
head(allSRankCounts)
 
p <- ggplot(allSRanks, aes(x=RE_mid, y=rowNum, xmin = RE_min, 
                           xmax = RE_max, col = srank)) +
  geom_linerange(linewidth = 0.7) + 
  scale_x_continuous(breaks = 1:length(factorScaleForRE),labels = x_labels_endash) +
  scale_color_manual(values = custColors,
    labels = allSRankCounts$merge) +  
  labs(x = "Range extent", y = "Species",
       colour = "S-Rank") + 
  theme_minimal() + 
  theme(text = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line(colour = "black")) + 
  theme(plot.margin = margin(1, 0, 1, 1)
) + 
  guides(color = guide_legend(
    title = "S-Rank (n)", 
    override.aes = list(linewidth=6))
  )
p

# make an output folder
bp <- "outputFigs"
if (!dir.exists(bp)) {dir.create(bp)}
fn <- paste0("RangeExtent_", strftime(Sys.Date(),"%Y%m%d"),".jpg")
ggsave(file.path(bp,fn), width = 8, height = 6)

######
## AOO (Area of Occupancy) ----
######
table(allSRanks$AOO, useNA = "ifany")

# get min and max S ranks
allSRanks$AOO_min <- str_sub(allSRanks$AOO,1,1)
allSRanks$AOO_max <- str_sub(allSRanks$AOO,-1,-1)

factorScaleForAOO <- c("Z","A","B","C","D","E","F","G","H","I","U")
allSRanks$AOO_min <- as.numeric(factor(allSRanks$AOO_min, 
                                      levels = factorScaleForAOO))
allSRanks$AOO_max <- as.numeric(factor(allSRanks$AOO_max, 
                                      levels = factorScaleForAOO))

# add some spread for the bar on the figure
allSRanks$AOO_min <- allSRanks$AOO_min - 0.35
allSRanks$AOO_max <- allSRanks$AOO_max + 0.35
allSRanks$AOO_mid <- (allSRanks$AOO_min + allSRanks$AOO_max)/2

xlabels <- c("0","1","2","3-5","6-25","26-125","126-500","501-2,500",
  "2,501-12,500", ">12,500","unknown")
# change to en-dash
x_labels_endash <- sub("-","\\\u2013",xlabels)

allSRanks_aoo <- allSRanks[order(allSRanks$srank, allSRanks$AOO),]
allSRanks_aoo$rowNum <- nrow(allSRanks_aoo):1

p <- ggplot(allSRanks_aoo, aes(x=AOO_mid, y=rowNum, xmin = AOO_min, 
                           xmax = AOO_max, col = srank)) +
  geom_linerange(linewidth = 0.7) + 
  scale_x_continuous(breaks = 1:length(factorScaleForAOO),labels = x_labels_endash) +
  scale_color_manual(values = custColors, 
        labels = allSRankCounts$merge) +  
  labs(x = "Area of occupancy", y = "Species",
       colour = "S-Rank") + 
  theme_minimal() + 
  theme(text = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line(colour = "black")) + 
  theme(plot.margin = margin(1, 0, 1, 1)) + 
  guides(color = guide_legend(
    title = "S-Rank (n)", 
    override.aes = list(linewidth=6))
  )
p

bp <- "outputFigs"
if (!dir.exists(bp)) {dir.create(bp)}
fn <- paste0("AreaOfOccupancy_", strftime(Sys.Date(),"%Y%m%d"),".jpg")
ggsave(file.path(bp,fn), width = 8, height = 6)

######
## Long Term Trend ----
######

table(allSRanks$LongTTrend, useNA = "ifany")
allSRanks$LTT_min <- str_sub(allSRanks$LongTTrend,1,1)
allSRanks$LTT_max <- str_sub(allSRanks$LongTTrend,-1,-1)

factorScaleForLTT <- c("A","B","C","D","E","F","G","H","I","U")
allSRanks$LTT_min <- as.numeric(factor(allSRanks$LTT_min, 
                                       levels = factorScaleForLTT))
allSRanks$LTT_max <- as.numeric(factor(allSRanks$LTT_max, 
                                       levels = factorScaleForLTT))

allSRanks$LTT_min <- allSRanks$LTT_min - 0.35
allSRanks$LTT_max <- allSRanks$LTT_max + 0.35
allSRanks$LTT_mid <- (allSRanks$LTT_min + allSRanks$LTT_max)/2

xlabels <- c("decline of >90%","decline of 80-90%","decline of 70-80%",
  "decline of 50-70%","decline of 30-50%","decline of 10-30%","relatively stable (<10% change)",
  "increase of 10-25%", "increase of >25%", "unknown")
# change to en-dash
x_labels_endash <- sub("-","\\\u2013",xlabels)

allSRanks_ltt <- allSRanks[order(allSRanks$srank, allSRanks$LongTTrend),]
allSRanks_ltt$rowNum <- nrow(allSRanks_ltt):1


p <- ggplot(allSRanks_ltt, aes(x=LTT_mid, y=rowNum, xmin = LTT_min, 
                           xmax = LTT_max, col = srank)) +
  geom_linerange(linewidth = 0.7) + 
  scale_x_continuous(breaks = 1:length(factorScaleForLTT),labels = x_labels_endash) +
  scale_color_manual(values = custColors, 
    labels = allSRankCounts$merge) + 
  labs(x = "Long-term trend", y = "Species",
       colour = "S-Rank") + 
  theme_minimal() + 
  theme(text = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line(colour = "black")) + 
  theme(plot.margin = margin(1, 0, 1, 1)) + 
  guides(color = guide_legend(
    title = "S-Rank (n)", 
    override.aes = list(linewidth=6))
  )
p

bp <- "outputFigs"
if (!dir.exists(bp)) {dir.create(bp)}
fn <- paste0("LongTermTrends_", strftime(Sys.Date(),"%Y%m%d"),".jpg")
ggsave(file.path(bp,fn), width = 8, height = 6)
