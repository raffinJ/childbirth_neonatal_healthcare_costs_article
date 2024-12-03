# This script is aimed at developing “Figure 1” of the article. This figure illustrates 
## the 20 most costly neonatal diagnoses to the SUS between 2011 and 2022 in terms of cost 
### per million Reais (R$) by the number of hospital records registered for each one.

install.packages("Polychrome")
library(Polychrome)

kelly <- kelly.colors(20)
kelly[1] <- "#e25822"  # Choosinf a darker color for Kelly's index 1 (previously white)

aih_cid <- merge(tot_cid, aih_cid, by = "DIAG_PRINC", all = TRUE)
aih_cid <- aih_cid[, -11]
aih_cid <- aih_cid[, -c(2:8)]

aih_cid$sum <- (aih_cid$sum / 1000000)


unique_codes <- unique(aih_cid$DIAG_PRINC)

# Aplicar a paleta modificada
color_mapping <- setNames(kelly[seq_along(unique_codes)], unique_codes)


#Plotting the Figure 1 from the article
plt <- ggplot(aih_cid, aes(x = sum, y = n, color = DIAG_PRINC, label = DIAG_PRINC)) +
  geom_point(size = 2) +
  scale_color_manual(values = color_mapping, guide = "none") + 
  labs(x = "Inpatient care costs in millions Reais (R$)", y = "Number of inpatient records") +
  theme_minimal() +

  scale_x_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","), trans = 'log2') +
  
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","), trans = 'log2', breaks = seq(14000, 605000, length.out = 5)) +
  
  theme(
    axis.title.x = element_text(size = 8, margin = margin(t = 20)),  
    axis.title.y = element_text(size = 8, margin = margin(r = 20)),
    axis.text.x = element_text(size = 7),  
    axis.text.y = element_text(size = 7),
    legend.position = "none"  
  ) +
  geom_text_repel(
    aes(label = DIAG_PRINC),
    size = 3,
    box.padding = 0.3,
    color = 'gray0',
    max.overlaps = 50
  )


# Visualizing the plot
print(plt)

# Saving the plot
ggsave("cid_costs.jpg", plot = plt, width = 9, height = 6, dpi = 800)
