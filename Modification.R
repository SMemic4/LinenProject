library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

simulation %>% group_by(type) %>% summarize(average_glove_pfu = mean(gloves_pfu), gloves_se = sd(gloves_pfu)) # This line of code ACCURATELY determines the average PFU concentration of gloves for both the control and treatment group
simulation %>% group_by(type) %>% summarize(average_gown_pfu = mean(gown_pfu), gown_se = sd(gown_pfu)/ sqrt(length(gown_pfu))) # Finds the average PFU 


ggplot() + geom_point(aes(x = Type))

averages <- simulation %>% group_by(type) %>% summarise(n = n(), average_glove = mean(gloves_pfu), average_gown = mean(gown_pfu), average_hand = mean(hands_pfu), average_neck = mean(neck_pfu),  )
ggplot(averages) + geom_col(aes(x = type, y = average_glove))
                            
con_Gloves <- simulation %>% filter(type == "Control") %>% tibble(typex = "control", Area = "Gloves", PFU = gloves_pfu, Dye = gloves_dye) %>% select(Control, Area, PFU, Dye)
con_Gown <-simulation %>% filter(type == "Control") %>% tibble(typex = "control", Area = "Gown", PFU = gown_pfu, Dye = gown_dye) %>% select(Control, Area, PFU, Dye)                            
con_Hands <-simulation %>% filter(type == "Control") %>% tibble(typex = "control", Area = "Hands", PFU = hands_pfu, Dye = hands_dye) %>% select(Control, Area, PFU, Dye)
con_Neck <-simulation %>% filter(type == "Control") %>% tibble(typex = "control", Area = "Neck", PFU = neck_pfu, Dye = neck_dye) %>% select(Control, Area, PFU, Dye)
Treat_Gloves <- simulation %>% filter(type == "Treatment") %>% tibble(typex = "treatment", Area = "Gloves", PFU = gloves_pfu, Dye = gloves_dye) %>% select(Control, Area, PFU, Dye)
Treat_Gown <- simulation %>% filter(type == "Treatment") %>% tibble(typex = "treatment", Area = "Gown", PFU = gown_pfu, Dye = gown_dye) %>% select(Control, Area, PFU, Dye)                            
Treat_Hands <- simulation %>% filter(type == "Treatment") %>% tibble(typex = "treatment", Area = "Hands", PFU = hands_pfu, Dye = hands_dye) %>% select(Control, Area, PFU, Dye)
Treat_Neck <- simulation %>% filter(type == "Treatment") %>% tibble(typex = "treatment", Area = "Neck", PFU = neck_pfu, Dye = neck_dye) %>% select(Control, Area, PFU, Dye)

new_sim <- bind_rows(con_Gloves, con_Gown, con_Hands, con_Neck, Treat_Gloves, Treat_Gown, Treat_Hands, Treat_Neck)
new_control <- bind_rows(con_Gloves, con_Gown, con_Hands, con_Neck)
new_treatment <- bind_rows(Treat_Gloves, Treat_Gown, Treat_Hands, Treat_Neck)

new_sim %>% group_by(Control, Area) %>% summarize(average_PFU = mean(PFU)) %>% ggplot() + geom_boxplot(aes(x = Area, y = average_PFU, ))
new_sim %>% group_by(Control, Area) %>% summarize(average_PFU = mean(PFU)) %>% ggplot(x = Area, y = average_PFU) + geom_boxplot(aes(color = Control))

### The code works
new_sim %>% ggplot() + geom_boxplot(aes(x= Area, y = PFU, fill = Control)) + scale_color_discrete(labels = c("Treatment", "Control")) + theme(legend.title = element_blank()) + scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) # THIS LINE WORKS 


new_control <- new_control %>% rename(Control == "Type")

ggplot(new_sim) + geom_boxplot(aes(x = Area~Control, y = PFU))

ggplot(new_sim, x = Area, y = average_PFU) + geom_boxplot(aes())

# Tracing map

diamonds %>% count(color, cut) %>% ggplot(aes(x = color, y = cut)) + geom_tile(aes(fill = n)) # Creates a heat map 


tracing3 <- tracing %>% gather(`A`,`B`,`C`,`D`,`E`,`F`,`G`,`H`,`I`,`J`, key = "A")

tracing3 %>% count(Location, Subject) %>% ggplot(aes(x = Subject, y = Location)) + geom_tile(aes(fill = prop))

tracing3 %>% group_by(Location, Subject) %>% mutate(prop = (A == TRUE)/10)


ggplot(tracing3, aes(x = Location, y = Subject)) + geom_tile(fill = prop) + coord_flip()

tracing3 %>% transmute(Location = Location, )

tracing %>% group_by(Location) %>% summarise(prop = n()/10))

tracing4 <- tracing3 %>% transmute(Location = Location, A = A) %>% group_by(Location) %>% count(A == TRUE)

tracing4 <- transmute(Location = Location, n = n)                                            