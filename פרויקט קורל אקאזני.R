library(tidyverse)   ##include ggplot2,dplyr, helps at "group by", manipulation, etc.
library(broom)       ## for the table of all the tests
library(plotrix)     ##for 3D pie chart
library(reshape2) #merging data sets
library(vcd) #Visualization of Chi - test, Cramer
library(stats) #R statistical function
library(plotly) #interactive graphs
library(lsr) #Cramer's calculation
library(gplots) #extra graph - heatmap
library(grid) #control the layout 
library(RColorBrewer) #creating build R palette
library(plot3D)
library(rgl)
library(ggthemes)
library(car)#VIF
library(lmtest)
library(jpeg)
library(gt) #Advanced design for tables
library(kableExtra)
library(htmltools)

## Koral Akazani 207012865

#Question 2 - Is there a statistically significant difference in the type of accidents committed
#by drivers of different genders on different types of roads?
#If so, what is the most common form of road with the highest number of accidents among women?


#Loading Data
data1 <- read.csv("C:\\Users\\Dell\\OneDrive\\women road accidents group 5\\H20221161AccData.csv" , header = TRUE)
data2 <- read.csv("C:\\Users\\Dell\\OneDrive\\women road accidents group 5\\H20221161InvData.csv" , header = TRUE)

#Merging tables by common column
mergeData <- merge(data1 , data2 , by = "pk_teuna_fikt")

#Create merge data with the columns we need for this project

#selecting the wanted columns
all_mergedata <- mergeData %>% select(pk_teuna_fikt , MIN, YOM_BASHAVUA, EZOR_TIVI_MEGURIM, SUG_TEUNA, ZURAT_DEREH, KVUZA_GIL, ZURAT_ISHUV_MEGURIM,
                                      YOM_LAYLA, HUMRAT_PGIA, SUG_DEREH)

# Remove duplicates based on the 'pk_teuna_fikt' column
all_mergedata <- all_mergedata %>% distinct(pk_teuna_fikt, .keep_all = TRUE)

#Delete NA values
Final_mergeData <- na.omit(all_mergedata)

#Final data - from here we will use this data
Final_mergeData <- as.data.frame(Final_mergeData)

# Print the resulting data frame
print(Final_mergeData)

#Column select
accColumns <- Final_mergeData %>% select(SUG_TEUNA, ZURAT_DEREH, MIN )

#Removing rows with no relevant values
accColumns<-accColumns[!(accColumns$MIN %in% c(0)) , ]

print(accColumns)


#1- Dictionary for MIN 
accColumns$MIN[accColumns$MIN == 1] <- "Man"
accColumns$MIN[accColumns$MIN == 2] <- "Woman"

# 2- Create groups for similar accidents types
accColumns <- accColumns %>%
  mutate(SUG_TEUNA = case_when(
    SUG_TEUNA %in% c(1) ~ "Injury to a pedestrian",  
    SUG_TEUNA %in% c(2,5) ~ "Head-on collision",
    SUG_TEUNA %in% c(3,7,17,18) ~ "Rear-end collision",  
    SUG_TEUNA %in% c(4,6) ~ "Side by side collision",
    SUG_TEUNA %in% c(8,14,15,19,20) ~ "Other",
    SUG_TEUNA %in% c(10,13) ~ "Overturning",
    SUG_TEUNA %in% c(9,11,12) ~ "Slip",
    TRUE ~ as.character(SUG_TEUNA)      
  ))

# 3- Create groups for similar road types
accColumns <- accColumns %>%
  mutate(ZURAT_DEREH = case_when(
    ZURAT_DEREH %in% c(1,2) ~ "Exit/Entrance to Interchange",
    ZURAT_DEREH %in% c(4,5) ~ "Sharp Slope/Curve",
    ZURAT_DEREH %in% c(6,8,10) ~ "Straight road/junction",
    ZURAT_DEREH %in% c(9) ~ "Other",
    TRUE ~ as.character(ZURAT_DEREH)      
  ))

#Check for NULL/NA data 
any(is.na(accColumns)) # Checking for "na" values - > FALSE = There are no "na" values
any(is.null(accColumns)) # Checking for "NULL" values - > FALSE = There are no "NULL" values

##Summary tables of the types of roads and accidents which exist at this question
# Extract unique road types
unique_road_types <- accColumns %>%
  select(ZURAT_DEREH) %>%
  distinct()

# Extract unique accident types
unique_accident_types <- accColumns %>%
  select(SUG_TEUNA) %>%
  distinct()

# Create the tables
table_road <- unique_road_types %>%
  gt() %>%
  tab_header(title = "Road Types") %>%
  cols_label(ZURAT_DEREH = "Road Types")

table_accident <- unique_accident_types %>%
  gt() %>%
  tab_header(title = "Accident Types") %>%
  cols_label(SUG_TEUNA = "Accident Types")

# Convert gt tables to data frames
df_road <- as.data.frame(table_road)
df_accident <- as.data.frame(table_accident)

# Create tables using kable
table_road_kable <- kable(df_road, format = "html", escape = FALSE, col.names = "Road Types") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

table_accident_kable <- kable(df_accident, format = "html", escape = FALSE, col.names = "Accident Types") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Combine the tables side by side and center on the page
htmltools::browsable(
  tagList(
    tags$div(
      style = "display: flex; justify-content: center; align-items: flex-start; gap: 50px;",  # Align tables at the top
      tags$div(
        HTML(table_road_kable)
      ),
      tags$div(
        HTML(table_accident_kable)
      )
    )
  )
)

# Define Mode function
Mode <- function(x) {
  ux <- unique(x)
  mode_val <- ux[which.max(tabulate(match(x, ux)))]
  freq <- max(tabulate(match(x, ux)))
  attr(mode_val, "freq") <- freq
  return(mode_val)
}

#############STATISTICS##########################

####### statistic for SUG_TEUNA by genders#######

#Calculate number of accidents for every type of accidents of any gender
acc_summary <- accColumns %>%
  group_by(SUG_TEUNA, MIN) %>%
  summarise(total_accidents = n(), .groups = 'drop')
print(acc_summary)
# Create summary statistics for accident types by genders 
summary_stats <- acc_summary %>%
  group_by(MIN) %>%
  summarise(
    Min = min(total_accidents),
    `1st Qu.` = quantile(total_accidents, 0.25),
    Median = median(total_accidents),
    Mean = mean(total_accidents),
    `3rd Qu.` = quantile(total_accidents, 0.75),
    Max = max(total_accidents),
    SD = sd(total_accidents)
  )

# Convert the data frame to a matrix and remove the column names
summary_stats <- as.data.frame(lapply(summary_stats, function(x) format(x, digits = 1)))
colnames(summary_stats)[-1] <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "SD")

print(summary_stats)#, row.names= FALSE)


#######After statistics########
###Distribution Graph - Stacked bar plot

# Creating table by road shape and count of accidents
table_G <- table(accColumns$ZURAT_DEREH, accColumns$SUG_TEUNA)
table_G <- as.data.frame.table(table_G)
colnames(table_G) <- c("ZURAT_DEREH", "SUG_TEUNA", "Freq")
print(table_G)

# Create sum of frequencies of all the combinations of type of road and type of accident
Frame_group <- aggregate(Freq ~ ZURAT_DEREH + SUG_TEUNA, data = table_G, FUN = sum)

# Creating dictionary with the sum of accidents for shape of roads
Total_Accidents_Dictionary <- setNames(sapply(unique(Frame_group$ZURAT_DEREH), function(group_val) {
  sum(Frame_group$Freq[Frame_group$ZURAT_DEREH == group_val])
}), unique(Frame_group$ZURAT_DEREH))
print(Total_Accidents_Dictionary)

# Creating and adding new column of total accidents
Frame_group$TOTAL_ACCIDENT <- sapply(Frame_group$ZURAT_DEREH, function(group_val) {
  Total_Accidents_Dictionary[[group_val]]
})
print(Frame_group)

# Creating percentage by freq part total
Frame_group$Percentages <- round(((Frame_group$Freq / Frame_group$TOTAL_ACCIDENT) * 100), 2)

# Filtering values different from zero percent
Frame_group <- Frame_group %>%
  filter(Percentages != 0)

print(Frame_group)

# Ensure the `SUG_TEUNA` is a factor with levels in the same order across all rows
unique_SUG_TEUNA <- unique(Frame_group$SUG_TEUNA)
Frame_group$SUG_TEUNA <- factor(Frame_group$SUG_TEUNA, levels = unique_SUG_TEUNA)

# Reorder `ZURAT_DEREH` factor levels based on total number of accidents
Frame_group$ZURAT_DEREH <- factor(Frame_group$ZURAT_DEREH, levels = names(Total_Accidents_Dictionary[order(Total_Accidents_Dictionary)]))

# GGplot graph
Gplot <- ggplot(Frame_group, aes(x = ZURAT_DEREH, y = Percentages, fill = SUG_TEUNA)) +
  geom_bar(stat = "identity") +  # height of bars by percentages of type of road
  labs(title = "Distribution of accidents by type of road and type of accident",
       x = "Type of road",
       y = "Percentage of accidents",
       fill = "Type of accident") +
  geom_text(aes(label = paste0(Percentages, "%")), # puts percentages on top of all bars
            position = position_stack(vjust = 0.5), size = 6) + # size and position of the % on the bars
  theme(plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "cm"), # the total plot margin - top, bottom, left, right
        plot.title = element_text(hjust = 0.6, size = 28, face = "bold"), # "face" - to make the title bold
        axis.title.x = element_text(size = 20, margin = margin(t = 10), face = "bold"), # Negative margin so the title of X axis will get closer to the plot
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 15, hjust = 0.5, margin = margin(r = 0), size = 18), # Tilt the text of the axis by 15 degrees
        axis.text.y = element_text(size = 18), # Increasing the font size at Y axis
        legend.title = element_text(size = 22, hjust = 0.3, face = "bold"), # Increasing the size of the legend's title
        legend.text = element_text(size = 20), # Increasing the font size at the legend
        panel.background = element_blank())

print(Gplot)
ggsave("accidents_by_road_type.png", plot = Gplot, width = 14, height = 12, dpi = 600)


#Now after statistic, I will find the most common accident type, road shape and gender 

# Find most common accident type
most_commonType <- Mode(accColumns$SUG_TEUNA)
num_most_commonType <- attr(most_commonType, "freq")
print(most_commonType)

# Find most common road shape
most_common_roadShape <- Mode(accColumns$ZURAT_DEREH)
num_most_common_roadShape <- attr(most_common_roadShape, "freq")
print(most_common_roadShape)

# Find most common gender
most_commonGender <- Mode(accColumns$MIN)
num_most_commonGender <- attr(most_commonGender, "freq")
print(most_commonGender)


######Creating some graphs to show the frequencies visually I have found 

# Creating data frame
summary_table <- data.frame(
  Category = c("Type of Accident", "Road Shape", "Gender"),
  Most_Common = c(most_commonType, most_common_roadShape, most_commonGender),
  Frequency = c(num_most_commonType, num_most_common_roadShape, num_most_commonGender)
)
print(summary_table)

# Graph 1 - bar plot for showing the most common Gender, Road Shape and Type of accident

# Creating bar plot with labels
ggplot(summary_table, aes(x = Category, y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste(Most_Common, "\n(", Frequency, ")", sep = "")), 
            vjust = 2.5, size = 5) +
  scale_fill_brewer(palette = "Set3") +  # R palette
  labs(title = "Most Common Values",
       x = "Category",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.background = element_blank())
ggsave("Most_Common_Values.png", width = 10, height = 6, dpi = 480) # save as photo

### graph 2 - 3D pie - Mordi : Please check this graph in order to give me grade. 
# Creating data frame

# summary table
#summary_table from the data frame I created above

# Using custom colors
custom_colors <- c("#66c2a5", "#fc8d62", "#8da0cb")
par(mfrow = c(1, 1), mar = c(0, 0, 0, 5), cex.main = 1.5) # mfrow - 1 graph at a time, mar - margin settings

# Function to create 3D pie chart
#create_pie_chart <- function() {
labels_with_values <- paste(summary_table$Frequency) # Create labels with frequency values

# Creating 3D pie chart
pie3D(summary_table$Frequency,
      explode = 0.1,   # Gap between the slices of the pie
      theta = 0.8,   # Viewing angle in radians
      col = custom_colors,
      labelcex = 2,  # Size of the labels on the slices 
      main = "Most Common Attributes",  # Title of the pie chart
      radius = 1,# Radius of the pie
      cex.main = 1.5 ) #size of title

# Function to calculate the position of the labels on the pie
get_label_positions <- function(frequencies) {
  total <- sum(frequencies)
  angles <- cumsum(frequencies) / total * 2 * pi
  angles <- angles - frequencies / total * pi
  return(angles)
}

# Calculate positions for labels
label_angles <- get_label_positions(summary_table$Frequency)


# Adding the labels on the cake - each label as a position by X & Y coordinates 
for (i in seq_along(summary_table$Frequency)) {
  angle <- label_angles[i]
  x <- cos(angle) * 0.6 
  y <- sin(angle) * 0.3
  
  # Adding the text on the slices with different parameters (size, color, position)
  text(x, y, labels_with_values[i], cex = 1.5, col = "black", pos = 3)
}

# Creating legend
# Calculate the width of the text in the legend
text_widths <- strwidth(paste(summary_table$Category, summary_table$Most_Common), units = "inches")
max_width <- max(text_widths) # Maximum width of legend text

# Create custom legend using grid
#Mode of adding a legend by X,Y, width and height position
pushViewport(viewport(x = unit(0, "npc"), y = unit(0.6, "npc"), width = unit(0.28, "npc"), height = unit(0.5, "npc")))
for (i in seq_along(summary_table$Category)) {
  grid.rect(x = unit(0.85, "npc"),  # Position and size of legend rectangles
            y = unit(1 - (i - 1) * 0.15, "npc"), 
            width = unit(0.3, "inches"), 
            height = unit(0.3, "inches"), 
            gp = gpar(fill = custom_colors[i], col = "black")) # Color of the legend squares
  grid.text(label = paste(summary_table$Category[i], ":", summary_table$Most_Common[i]), # Text in the legend
            x = unit(0.94, "npc"), 
            y = unit(0.99 - (i - 1) * 0.15, "npc"), 
            hjust = 0, 
            vjust = 0, 
            gp = gpar(fontsize = 14))
}
popViewport()
#npc = normalized parent coordinates -The position is determined in relation to the calculated height and width.

# Save the pie chart as a PNG file with specific dimensions
png("3D_ pie chart.png", width = 1072, height = 794) # Dimensions in pixels

# Close the PNG device
dev.off()



############ TESTS ###########

######ANOVA two-way assumption 1 - Testing for independence between the variables

##Chi-test check

##Chi-test - Hypotheses
#cat("H0 : There is no connection between the variables", "\n")
#cat("H1 : There is a connection between the variables", "\n")

# Create a frequency table
chi_table <- table(accColumns$ZURAT_DEREH, accColumns$SUG_TEUNA)
print(chi_table)

# Perform Chi-squared test of independence
chi_square_test <- chisq.test(chi_table, correct = TRUE)

# Print test results
print(chi_square_test)

#Making final Decision
if (chi_square_test$p.value < 0.05) {
  cat("P-Value is : ",chi_square_test$p.value, "\n", "Desicion - Reject HO hypothesis (Accept H1)." ,"\n", 
      "There is a connection between the different road shapes and the types of accidents.\n")
} else {
  cat("P-Value is : ",chi_square_test$p.value, "\n", "Desicion - Accept the HO hypothesis.","\n", 
      "There is no connection between the different road shapes and the types of accidents.\n")
}

#Visual result

#Creating a frequency table
chi_table <- table(accColumns$ZURAT_DEREH, accColumns$SUG_TEUNA)

# Association graph with label resizing and spacing
# Adjusting the direction of the labels on the X and Y axis
labeling_args <- list(gp = gpar(cex = 0.5), rot_labels = c(45, 45, -60, -60))
assoc(chi_table, 
      shade = TRUE, 
      legend = TRUE, 
      main = "Association Plot of Road Type and Accident Type",
      spacing = spacing_conditional(sp = 2),
      gp_labels = gpar(cex = 1, col = "black", font = 2),
      gp_axis = gpar(cex = 1),
      labeling_args = labeling_args,
      title_margin = 4,
      mar = c(6, 0, 0, 4) # Adjust margins if supported
)


#ANOVA two-way assumption 2- Calculation of the strength of the connection between the variables

#After founding that there is a dependency between the variables, I will find what is the The strength of the relationship

# Calculate Cramer's V
contingency_table <- table(accColumns$SUG_TEUNA, accColumns$ZURAT_DEREH)
print(contingency_table)
# Print Cramer's V
result <- assocstats(contingency_table)
print(result)

#Making final Decision
if (result$cramer == 0) {
  cat("There is no connection between the variables.\n")
} else if (result$cramer > 0 && result$cramer <= 0.099) {
  cat("There is a very weak connection between the variables.\n",
      "The connection is :" , result$cramer, "\n")
}else if (result$cramer > 0.099 && result$cramer <= 0.3) {
  cat("There is a weak connection between the variables.\n",
      "The connection is :" , result$cramer, "\n")
}else if (result$cramer > 0.3 && result$cramer <= 0.5) {
  cat("There is a mid connection between the variables.\n",
      "The connection is :" , result$cramer, "\n")
}else if (result$cramer > 0.5 && result$cramer <= 0.7) {
  cat("There is a strong connection between the variables.\n",
      "The connection is :" , result$cramer, "\n")
}else{
  cat("There is a very strong connection between the variables.\n",
      "The connection is :" , result$cramer , "\n")
}

##Verbal final conclusion
cat("conclusion : The test indicates a statistically significant relationship between the two variables,\n",
    "But the strength of the relationship is very weak.\n"
)


# Correlation graph
geom_jitter <- ggplot(accColumns) +
  aes(x = ZURAT_DEREH, y = SUG_TEUNA ) +
  geom_jitter(size = 2, width = 0.5, height = 0.2) +  # adding space
  labs(
    x = "Type of road",
    y = "Type of accident",
    title = "Correlation between Type of road to Type of accident "
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18L, face = "bold"),
    axis.title.x = element_text(size = 18L, face = "bold", margin = margin(t = 20)),
    axis.text.x = element_text(size = 14L),  
    axis.text.y = element_text(size = 14L), 
    
  )

print(geom_jitter)



#ANOVA two-way assumption 3 - Checking if the variables are NORNALLY distributed

##Shapiro wilk test - Hypotheses
cat("H0 : The Frequency data is normally distributed", "\n")
cat("H1 : The Frequency data is not normally distributed", "\n")

#Check the Hypotheses with Shapiro wilk test
shapiro_test <- shapiro.test(Frame_group$Percentages)
print(shapiro_test)

##Print result 
cat("P-Value is:", shapiro_test$p.value, "\n")

#Making final Decision
if (shapiro_test$p.value < 0.05) {
  cat("Reject HO hypothesis (Accept H1), The frequency data is not normally distributed.\n")
} else {
  cat("Accept the HO hypothesis, The frequency data is not normally distributed.\n")
}

###NormalPlot - Showing if there is normal distribution by visual plot

# While Frame_group is my data frame and Percentages is the variable of interest

ggplot(Frame_group, aes(sample = Percentages)) + #variables and data
  stat_qq(size = 2.5) +
  stat_qq_line(color = "red", linewidth  = 1.1) +  # Change line color to red and set line size
  labs(title = "Q-Q Plot of Percentages (Residuals)",
       x = "Quantiles of Normal Distribution",
       y = "Quantiles of Sample Data") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Set breaks for x-axis
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))    # Set breaks for y-axis


#Verbal result according to the graph
cat("The frequency data is not normally distributed", "\n", 
    "The residuals are not arranged at 45 degrees according to the red qqline.", "\n")


### Because the variables are not normalliy distributed - we will use an un-parametric test

#Kroskal - wallis

##Kroskal - wallis test - Hypotheses
cat("H0 : There is no statistically significant difference in the type of accidents (committed by drivers), on different types of road shapes.", "\n")
cat("H1 : There is a statistically significant difference in the type of accidents (committed by drivers), on different types of road shapes.", "\n")

###Kroskal - wallis test
kruskal_testQ2 <- kruskal.test(SUG_TEUNA ~ ZURAT_DEREH, data = accColumns)
print(kruskal_testQ2)

#print result
cat("P-Value is:", kruskal_testQ2$p.value, "\n")

#Making final Decision
if (kruskal_testQ2$p.value < 0.05) {
  cat("Reject HO hypothesis (Accept H1).","\n","We will conclude at a significance level of 5%" ,"\n",
      "that there is a statistically significant difference in the type of accidents committed by drivers,","\n","on different types of road shapes." ,"\n")
} else {
  cat("Accept the HO hypothesis.","\n","We will conclude at a significance level of 5%" ,"\n",
      "that there is no statistically significant difference in the type of accidents committed by drivers, on different types of road shapes." ,"\n")
}


# Create the heatmap
hotMap <- ggplot(Frame_group ,aes(x = ZURAT_DEREH, y =SUG_TEUNA , fill = Percentages)) +
  geom_tile(color = "azure") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Heatmap of Accident Types by Road Shapes",
       x = "Road Shape",
       y = "Accident Type",
       fill = "Percentages %") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank())

print(hotMap)



####### Part 2 of the question - if there is a difference, i will find 
#the most common form of road with the highest number of accidents among women

# Filter data for woman
Woman_Accidents <- accColumns %>% filter(MIN == "Woman")

# Calculating the frequencies of type of road and type of accident for women only
woman_acc_summary <- Woman_Accidents %>%
  group_by(SUG_TEUNA, ZURAT_DEREH) %>%
  summarise(total_accidents = n(), .groups = 'drop')
print(woman_acc_summary)

# Find most common accident type for women
most_common_Type_Woman <- Mode(Woman_Accidents$SUG_TEUNA)
num_most_common_Type_Woman <- attr(most_common_Type_Woman, "freq")
print(most_common_Type_Woman)

# Find most common road shape for women
most_common_road_Shape_Woman <- Mode(Woman_Accidents$ZURAT_DEREH)
num_most_common_road_Shape_Woman <- attr(most_common_road_Shape_Woman, "freq")
print(most_common_road_Shape_Woman)

# Creating data frame
summary_table_Woman <- data.frame(
  Category = c("Type of Accident", "Road Shape"),
  Most_Common = c(most_common_Type_Woman, most_common_road_Shape_Woman),
  Frequency = c(num_most_common_Type_Woman, num_most_common_road_Shape_Woman)
)
print(summary_table_Woman)

######Creating some graphs to show the frequencies visually I have found

#Graph - Scatter 3D graph of the freq for woman only  ### Also Mordi - Please check for me this graph of Koral

# Creating 3D scatter plot
fig <- plot_ly(
  data = woman_acc_summary,
  x = ~SUG_TEUNA,
  y = ~ZURAT_DEREH,
  z = ~total_accidents,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 5.5, color = ~total_accidents, colorscale = 'Viridis', showscale = TRUE)
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = 'Type of Accident',
        titlefont = list(size = 14),  # Increase font size if needed
        ticktext = woman_acc_summary$SUG_TEUNA,
        tickangle = 60,
        title = list(
          standoff = 200,
          pad = list(b = 60),# Increase space between title and plot
          x = 0.5,
          xanchor = "center"
        )
      ),
      yaxis = list(
        title = 'Road Shape',
        titlefont = list(size = 14),  # Increase font size if needed
        ticktext = woman_acc_summary$ZURAT_DEREH,
        tickangle = -50,
        title = list(
          pad = list(l = 50)  # Increase space between title and plot
        )
      ),
      zaxis = list(
        title = 'Total Accidents',
        titlefont = list(size = 14),  # Increase font size if needed
        title = list(
          pad = list(t = 40)  # Increase space between title and plot
        )
      )
      
    ),
    title = list(
      text = 'Total Accidents for Women by Type of Accident and Road Shape',
      x = 0.5,   # Center the title
      xanchor = 'center',
      y = 0.9,  # Move the title closer to the plot
      yanchor = 'top'
    ),
    autosize = FALSE,  # Ensure the plot takes up most of the space
    margin = list(l = 0, r = 0, b = 0, t = 0)  # Adjust margins as needed
  )

# Show the plot
fig


##################