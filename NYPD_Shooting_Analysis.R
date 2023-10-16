library(tidyverse)
NYPD <- read.csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
summary(NYPD)

NYPD %>%
  group_by(VIC_RACE) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = VIC_RACE, y = Count, fill = VIC_RACE)) +
  geom_bar(stat = "identity") +
  labs(title = "Incidents by Victim Race", x = "Victim Race", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

NYPD %>%
  group_by(VIC_RACE, BORO) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = VIC_RACE, y = Count, fill = BORO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Incidents by Victim Race and Borough",
       x = "Victim Race", y = "Count",
       fill = "Borough") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")



unique(NYPD$STATISTICAL_MURDER_FLAG)
NYPD$STATISTICAL_MURDER_FLAG <- ifelse(NYPD$STATISTICAL_MURDER_FLAG == "Y", 1, 0)
unique(NYPD$STATISTICAL_MURDER_FLAG)

model <- glm(STATISTICAL_MURDER_FLAG ~ VIC_RACE, data = NYPD, family = "binomial")
summary(model)

# BIAS: In this analysis, I examined the relationship between demographic factors, 
# such as race, and the incidence of shootings in a specific urban area. 
# The dataset used in this study, titled "NYPD_Shooting_Analysis," 
# contains records of shooting incidents and associated demographic information.
#
# If there is variation in the reporting of shooting incidents based on factors like 
# neighborhood socioeconomic status or racial composition, this could lead to reporting bias. 
# Incidents may be overrepresented or underrepresented in the data due to differential reporting.


