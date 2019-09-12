# libraries --------------

pacman::p_load(readr, caret, ggplot2, reshape2, DataExplorer, MASS,
               tidyr, scales, mlbench, arules, arulesViz, tabulizer, dplyr, gridExtra)

# Reading data -------------
basket_data <- read.transactions("/Drive/Ubiqum/Course_2_Data_Analytics_II/Task_4/ElectronidexTransactions2017.csv",
                                 format = "basket", header = FALSE, sep = ",")

clean_data <- read.transactions("/Drive/Ubiqum/Course_2_Data_Analytics_II/Task_4/cleandata1.csv",
                                 format = "basket", header = FALSE, sep = ",")

bw <- read.csv("/Drive/Ubiqum/Course_2_Data_Analytics_II/Task_4/blackwelldata.csv")

# data exploration item level ---------
summary(basket_data)

inspect (head(basket_data)
length (basket_data)
size (basket_data)
LIST(head(basket_data))
itemLabels(basket_data)

itemFrequency(basket_data[,1:5])
itemFrequencyPlot(basket_data, support=0.1)
itemFrequencyPlot(basket_data, topN = 15,
                  xlab = "Product name",  cex.lab= 2, ylab = "Relative frequency",
                  col = "lightcyan2", main="Relative product Frequency Plot")

image(sample(basket_data, 100))

products_1item <- basket_data[which(size(basket_data) == 1), ]

my_crosstable <- crossTable(products_1item)
itemFrequencyPlot(products_1item, topN = 10, type = "absolute")
my_crosstable["Apple MacBook Air","Apple MacBook Air"]

# data exploration category level ---------
summary(clean_data)

inspect (head(clean_data))
length (clean_data)
size (clean_data)
LIST(head(clean_data))
itemLabels(clean_data)

itemFrequency(clean_data[,1:5])
itemFrequencyPlot(clean_data, support=0.1)
itemFrequencyPlot(clean_data, type = "relative", topN=17, 
                  xlab = "Product Type",  cex.lab= 2, ylab = "Relative frequency",
                  col = "coral", main="Relative product type Frequency Plot")

pt_1item <- clean_data[which(size(clean_data) == 1), ]

pt_my_crosstable <- crossTable(pt_1item)
itemFrequencyPlot(pt_1item, topN = 10, type = "absolute")
pt_my_crosstable["Laptop","Laptop"]

# Plotting the support and confidence item level----------------
# Number of rules found with a support level of 10%
# Support and confidence values
supportLevels_I <- c(0.05, 0.01, 0.005, 0.0025)
confidenceLevels_I <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup5_I <- integer(length=9)
rules_sup1_I <- integer(length=9)
rules_sup0.5_I <- integer(length=9)
rules_sup0.25_I <- integer(length=9)

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels_I)) {
  
  rules_sup5_I[i] <- length(apriori(basket_data, parameter=list(sup=supportLevels_I[1], 
                                                               conf=confidenceLevels_I[i], target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels_I)) {
  
  rules_sup1_I[i] <- length(apriori(basket_data, parameter=list(sup=supportLevels_I[2], 
                                                              conf=confidenceLevels_I[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels_I)) {
  
  rules_sup0.5_I[i] <- length(apriori(basket_data, parameter=list(sup=supportLevels_I[3], 
                                                              conf=confidenceLevels_I[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.25%
for (i in 1:length(confidenceLevels_I)) {
  
  rules_sup0.25_I[i] <- length(apriori(basket_data, parameter=list(sup=supportLevels_I[4], 
                                                                conf=confidenceLevels_I[i], target="rules")))
  
}

plot1_I <- qplot(confidenceLevels_I, rules_sup5_I, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") +
  theme_bw()

# Number of rules found with a support level of 1%
plot2_I <- qplot(confidenceLevels_I, rules_sup1_I, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 
  theme_bw()

# Number of rules found with a support level of 0.5%
plot3_I <- qplot(confidenceLevels_I, rules_sup0.5_I, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.5%") + 
  theme_bw()

# Number of rules found with a support level of 0.25%
plot4_I <- qplot(confidenceLevels_I, rules_sup0.25_I, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.25%") + 
  theme_bw()

# Subplot
grid.arrange(plot1_I, plot2_I, plot3_I, plot4_I, ncol=2)


# Apriori analysis item level ---------

RulesName<- apriori (basket_data, parameter = list(supp = 0.005, conf = 0.5, minlen = 2))
inspect(RulesName)
summary(RulesName)
inspect(sort(RulesName, by = "lift"))
is.redundant(RulesName)
sum(is.redundant(RulesName))


ItemRules <- subset(RulesName, items %in% "iMac")
inspect(ItemRules)

plot(RulesName)
plot(RulesName, method="graph", control=list(type="items")) 

# Plotting the support and confidence category level----------------

# Support and confidence values
supportLevels <- c(0.15, 0.1, 0.05, 0.01)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup15 <- integer(length=9)
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1<- integer(length=9)

# Apriori algorithm with a support level of 15%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup15[i] <- length(apriori(clean_data, parameter=list(sup=supportLevels[1], 
                                                             conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(clean_data, parameter=list(sup=supportLevels[2], 
                                                             conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup5[i] <- length(apriori(clean_data, parameter=list(sup=supportLevels[3], 
                                                               conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup1[i] <- length(apriori(clean_data, parameter=list(sup=supportLevels[4], 
                                                                conf=confidenceLevels[i], target="rules")))
  
}

plot1 <- qplot(confidenceLevels, rules_sup15, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 15%") +
  theme_bw()

# Number of rules found with a support level of 10%
plot2 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") + 
  theme_bw()

# Number of rules found with a support level of 5%
plot3 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  theme_bw()

# Number of rules found with a support level of 1%
plot4 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)


# Apriori analysis category level ---------

Rules_clean <- apriori (clean_data, parameter = list(supp = 0.10, conf = 0.5, minlen = 2))

inspect(Rules_clean)
summary(Rules_clean)
inspect(sort(Rules_clean, by = "lift"))
is.redundant(Rules_clean)
sum(is.redundant(Rules_clean))

top20Rules_clean <- head(Rules_clean, n = 20, by = "confidence")
plot(top10Rules_clean, method="graph", control=list(type="items"))

ItemRules1 <- subset(Rules_clean, items %in% "Desktop")
inspect(ItemRules1)

plot(Rules_clean)
plot(Rules_clean, method="scatterplot", control=list(type="items"))
plot(Rules_clean, method="two-key plot", control=list(type="items"))
plot(Rules_clean, method="graph", control=list(type="items"))
plot(Rules_clean, method="paracoord", control=list(type="items"))
plot(Rules_clean, method="grouped", control=list(type="items"))


# apriori analysis with filtering the rhs

categories <- itemLabels(clean_data)
without_D_L <- categories[-c(5, 8)]


Rules_test <- apriori (clean_data, parameter = list(supp = 0.05, conf = 0.2, minlen = 2), 
                       appearance = list(rhs = without_D_L))

summary(Rules_test)
inspect(sort(Rules_test, by = "lift"))
is.redundant(Rules_test)
sum(is.redundant(Rules_test))
plot(Rules_test, method="graph", control=list(type="items"))

#analysis of blackwell data -----------

bw %>% 
  count(ProductType) %>% 
  mutate(Volume = n / nrow(bw)) -> bw2


bw_plot <- ggplot(bw2, aes(x = reorder(ProductType, -Volume), y = Volume, fill = ProductType)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Product type") +
  ylab("relative frequencies")

bw_plot1 <- bw_plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
bw_plot2 <- bw_plot1 + plot.settings

# General plot settings ----------------
plot.settings <- theme(
  axis.line.x =       element_line(colour = "black", size = 1),                                                       # Settings x-axis line
  axis.line.y =       element_line(colour = "black", size = 1),                                                       # Settings y-axis line 
  axis.text.x =       element_text(colour = "black", size = 16, lineheight = 0.9, vjust = 1, face = "bold"),        # Font x-axis 
  axis.text.y =       element_text(colour = "black", size = 16, lineheight = 0.9, hjust = 1),                         # Font y-axis
  axis.ticks =        element_line(colour = "black", size = 0.3),                                                     # Color/thickness axis ticks
  axis.title.x =      element_text(size = 20, vjust = 1, face = "bold", margin = margin(10,1,1,1)),                   # Font x-axis title
  axis.title.y =      element_text(size = 20, angle = 90, vjust = 1, face = "bold", margin = margin(1,10,1,1)),       # Font y-axis title
  
  legend.background = element_rect(colour=NA),                                                                        # Background color legend
  legend.key =        element_blank(),                                                                                # Background color legend key
  legend.key.size =   unit(1.2, "lines"),                                                                             # Size legend key
  legend.text =       element_text(size = 18),                                                                        # Font legend text
  legend.title =      element_text(size = 20, face = "bold", hjust = 0),                                              # Font legend title  
  legend.position =   "right",                                                                                        # Legend position
  
  panel.background =  element_blank(),                                                                                # Background color graph
  panel.border =      element_blank(),                                                                                # Border around graph (use element_rect())
  panel.grid.major =  element_blank(),                                                                                # Major gridlines (use element_line())
  panel.grid.minor =  element_blank(),                                                                                # Minor gridlines (use element_line())
  panel.margin =      unit(1, "lines"),                                                                               # Panel margins
  
  strip.background =  element_rect(fill = "grey80", colour = "grey50"),                                               # Background colour strip 
  strip.text.x =      element_text(size = 20),                                                                        # Font strip text x-axis
  strip.text.y =      element_text(size = 20, angle = -90),                                                           # Font strip text y-axis
  
  plot.background =   element_rect(colour = NA),                                                                      # Background color of entire plot
  plot.title =        element_text(size = 20, face = "bold", hjust = 0.5),                                                                        # Font plot title 
  plot.margin =       unit(c(1, 1, 1, 1), "lines")                                                                    # Plot margins
)
