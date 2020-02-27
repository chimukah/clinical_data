# Global Options

options(stringsAsFactors = FALSE)


# Required Packages 

library(sas7bdat)
library(ggplot2)
library(dplyr)

# Load Data 

clinical_data = data.frame(read.sas7bdat(file = "/chapter15_example.sas7bdat"))
save(clinical_data, file = "clinical_data.RData")


# Summary Checks on Data 

str(clinical_data)
summary(clinical_data)

# Adjust Data 

clinical_data = clinical_data %>% 
                mutate(GENDER = factor(GENDER, levels = c("F", "M")), 
                       THERAPY = factor(THERAPY, levels = c("DRUG", "PLACEBO")))


# Summaries

summary_1 = clinical_data %>%
            group_by(THERAPY, GENDER) %>%
            summarize(average_change = mean(change)) %>%
            ungroup()


# Create Plots

plot_1 = ggplot(clinical_data, aes(x = change/basval, fill = THERAPY)) +
         geom_density(alpha = 0.4) +
         scale_x_continuous(labels = scales::percent, 
                            breaks = scales::pretty_breaks(n = 10)) +
         scale_y_continuous(labels = scales::percent, 
                            breaks = scales::pretty_breaks(n = 10)) +
         ggtitle("Drug VS Placebo Change") +
         xlab("Change") 

plot_2 = ggplot(clinical_data, aes(x = basval, y = change/basval, color = GENDER, shape = THERAPY)) + 
         geom_point() +
         geom_hline(yintercept = 0, linetype = "dashed", color = "blue2") +
         scale_x_continuous(labels = scales::comma, 
                            breaks = scales::pretty_breaks(n = 10)) +
         scale_y_continuous(labels = scales::percent, 
                            breaks = scales::pretty_breaks(n = 10)) +
         ggtitle("Drug VS Placebo Change Scatter") +
         xlab("basval") + 
         ylab("percentage changed") +
         theme_minimal()

# Statistical Test - For Boys

boys_data = clinical_data %>% 
            filter(GENDER == "M")

N_drug = nrow(boys_data%>%filter(THERAPY == "DRUG"))
N_placebo = nrow(boys_data%>%filter(THERAPY == "PLACEBO"))

mu_drug = mean((boys_data%>%filter(THERAPY == "DRUG"))$change)
mu_placebo = mean((boys_data%>%filter(THERAPY == "PLACEBO"))$change)

sd_drug =  sd((boys_data%>%filter(THERAPY == "DRUG"))$change)
sd_placebo = sd((boys_data%>%filter(THERAPY == "PLACEBO"))$change)

mu_difference = 0
sd_difference = sqrt((sd_drug^2)/N_drug + (sd_placebo^2)/N_placebo)

actual_difference = mu_drug - mu_placebo

p_value = pnorm(actual_difference, mu_difference, sd_difference)

final_plot = ggplot(data.frame(simulation = rnorm(1e5 ,mu_difference, sd_difference)), aes(x = simulation)) + 
             geom_density() + 
             geom_vline(xintercept = qnorm(0.05), linetype = "dashed", color = "blue2", size = 1.5) + 
             geom_vline(xintercept = actual_difference, linetype = "dashed", color = "red2", size = 1.5)






