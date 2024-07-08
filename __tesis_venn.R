
require(ggven)
ggvenn(data_list, fill_color= c("blue", "red"), column= c("no","ya"))


# Create the data
set1 <- 1:85048
set2 <- (85048-22287 +1):(85048-22287 +74833)


#85048 son los originales de SENDA
#74833 v2 originalmente casos
# Create a list of the two datasets
data_list <- list(`SENDAs patients`=set1, `Patients in PO`=set2)

# Use ggvenn to create the Venn diagram
ggvenn(data_list, fill_color = c("blue", "red", "green"))



# Create data sets based on the numbers you provided
set1 <- 1:85048
set2 <- 1:70854
set3 <- 1:22287

# Create a named list of the three datasets
data_list <- list("1) SENDA patients" = set1, "2) W/ PO records" = set2, "3) Imputed with an offense" = set3)

# Use ggvenn to create the Venn diagram
ggvenn(data_list, fill_color = c("gray10", "gray40", "gray80"), digits = 0)
  #scale_y_continuous(labels = function(x) ifelse(x == 0, "?", x)) 
  
ggsave("venn.png", height=5, width=8, dpi=600)
