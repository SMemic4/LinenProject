library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(nycflights13)
library(Lahman)
library(ggstance)
library(lvplot)
library(ggbeeswarm)
library(heatmaply)
library(stringr)

dyedata %>% filter(Type == "Control") %>% ggplot(aes( x = Area)) + geom_col(aes(x = Area, y = `Fluorescence Rate`)) # Simple graph showing the how often different areas of the skin were contaminated with dye for control group
dyedata %>% filter(Type == "Treatment") %>% ggplot(aes( x = Area)) + geom_col(aes(x = Area, y = `Fluorescence Rate`)) # Same chart as above but just for the treatment group
dyedata %>% filter(Type == "Control") %>% ggplot(aes( x = Area)) + geom_col(aes(x = Area, y = `Contamination Rate`)) # Similar chart to the ones above but just with the y axis variable changed to pathogen contamination for control group
dyedata %>% filter(Type == "Treatment") %>% ggplot(aes( x = Area)) + geom_col(aes(x = Area, y = `Contamination Rate`)) # Similar to chart above but for treatment group

# All the charts are OK above but don't really give us a full picture of what is going on. One step to solving this issue is be condensing the data singular graphs to display what we want to see

dyedata %>% group_by(Type) %>% ggplot(aes(x = Area)) + geom_col(aes(x = Area, y = `Contamination Rate`, `Fluorescence Rate`, fill = Type)) + facet_wrap(.~ Type) + ylab("Percent Contamination Rate") # Makes a facet wrapped grid that displays the percent contamination rate of different areas. Nice graph but I want both of them on the one chart for easier comparison. We can start by taking a look at the difference between the control and treatment groups
dyedata %>% group_by(Type) %>% ggplot(aes(x = Area)) + geom_col(aes(x = Area, y = `Fluorescence Rate`, fill = Type)) + facet_wrap(.~ Type) + ylab("Percent Fluorescence Rate") # Same type of chart as above but for the % dye rate

ggplot(dyedata, aes(x = Area, y = `Contamination Rate`, fill = Type, color = Type)) + geom_col(stat = "identity", position = "dodge") + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) + ylab("% Contamination Rate")          





dye2 <- dyedata %>% gather(`Contamination Rate`, `Fluorescence Rate`, key = "Contamination", value = "Percent")

dye2 %>% filter(Type == "Control") %>% ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination)) + geom_col(stat = "identity", position = "dodge2") + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) + ylab("% Positive") + theme(axis.text = element_text(size = 20), text = element_text(size = 20)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) # Creates a bar graph of the control MS2 contamination rates and fluorescence rates
dye2 %>% filter(Type == "Treatment") %>% ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination)) + geom_col(stat = "identity", position = "dodge2") + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) + ylab("% Positive") + theme(axis.text = element_text(size = 20) , text = element_text(size = 20)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) # Creates a bar graph of the control MS2 contamination rates and fluorescence rates

# Facet wrapped grid
dye2 %>% ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination)) + geom_col(stat = "identity", position = "dodge2", width = 0.8) + facet_wrap(~Type) + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) + ylab("% Positive") + theme(axis.text = element_text(size = 15) , text = element_text(size = 15)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100))

dye2 %>% ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination)) + geom_col(stat = "identity", position = "dodge2") + facet_wrap(~Type) + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) + ylab("% Positive") + theme(axis.text = element_text(size = 15) , text = element_text(size = 15)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + xlab(" ")
### Change labels of contamination rate to fluorescence rate and just drop the rate
dye2 %>% ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination)) + geom_col(stat = "identity", position = "dodge2") + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) + ylab("% Positive") + theme(axis.text = element_text(size = 15) , text = element_text(size = 15)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + xlab(" ") + facet_wrap(~Type) + scale_color_continuous(breaks = c("Contamination Rate", "Fluorescence Rate"), labels = c("Contamination", "Fluorescence"))

# Attempting to change individual varaiables in plot

###### THIS CODE WORKS
dye3 <- dye2 %>% mutate(Contamination = str_replace(Contamination, "Contamination Rate", "Contamination"), Contamination = str_replace(Contamination, "Fluorescence Rate", "Fluorescence"))
dye3 %>% ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination)) + geom_col(stat = "identity", position = "dodge2") + facet_wrap(~Type) + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) + ylab("% Positive") + theme(axis.text = element_text(size = 20) , text = element_text(size = 20)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + xlab(" ")
##### THIS CODE WORKS

# Same charts as above but im editing them to look better

#### Attempting to make a stacked bar chart 

dye2 %>% group_by(Type, Contamination) %>%  ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination )) + geom_col(stat = "identity", position = "fill") + theme(legend.position = "none", legend.direction = "horizontal", (legend.title = element_blank()) + labels = c("Contamination", "Fluorescence")) + ylab("% Positive") + theme(axis.text = element_text(size = 20) , text = element_text(size = 20)) 
  

dye2 %>% group_by(Type, Contamination) %>%  ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination)) + geom_col(stat = "identity", position = "dodge2") + facet_grid(~Type) 
dye2 %>% group_by(Type, Contamination) %>%  ggplot(aes(x = Area, y = Percent, fill = Contamination, color = Contamination)) + geom_col(stat = "identity", position = "dodge2") + facet_wrap(~Type) 
dyetib <- tribble(~Area, ~Type, ~Positive,
                 "Gloves","Control Contamination Rate", 100,
                 "Gloves","Control Fluorescence Rate", 100,
                 "Gown", "Control Contamination Rate", 80,
                 "Gown", "Control Fluorescence Rate", 80,
                 "Hands/Wrists", "Control Contamination Rate", 20,
                 "Hands/Wrists", "Control Fluorescence Rate", 60,
                 "Neck", "Control Contamination Rate", 20,
                 "Neck", "Control Fluorescence Rate", 10,
                 "Gloves","Treatment Contamination Rate", 70,
                 "Gloves","Treatment Fluorescence Rate", 100,
                 "Gown", "Treatment Contamination Rate", 0,
                 "Gown", "Treatment Fluorescence Rate", 70,
                 "Hands/Wrists", "Treatment Contamination Rate", 0,
                 "Hands/Wrists", "Treatment Fluorescence Rate", 60,
                 "Neck", "Treatment Contamination Rate", 0,
                 "Neck", "Treatment Fluorescence Rate", 0,
                 )
dyetib %>% ggplot(aes(x = Area, y = Positive, fill = Type)) + geom_col(stat = "identity", position = "dodge2", width = 0.5)
dyetib %>% ggplot(aes(x = Area, y = Positive, fill = Type)) 


dyetib2 <- tribble(~Area, ~Type, ~Positive,
                  "Gloves","Control Contamination Rate", 10,
                  "Gloves","Control Fluorescence Rate", 10,
                  "Gown", "Control Contamination Rate", 8,
                  "Gown", "Control Fluorescence Rate", 8,
                  "Hands/Wrists", "Control Contamination Rate", 2,
                  "Hands/Wrists", "Control Fluorescence Rate", 6,
                  "Neck", "Control Contamination Rate", 2,
                  "Neck", "Control Fluorescence Rate", 1,
                  "Gloves","Treatment Contamination Rate", 7,
                  "Gloves","Treatment Fluorescence Rate", 10,
                  "Gown", "Treatment Contamination Rate", 0,
                  "Gown", "Treatment Fluorescence Rate", 7,
                  "Hands/Wrists", "Treatment Contamination Rate", 0,
                  "Hands/Wrists", "Treatment Fluorescence Rate", 6,
                  "Neck", "Treatment Contamination Rate", 0,
                  "Neck", "Treatment Fluorescence Rate", 0,
)

dyetib2 %>% ggplot(aes(x = Area, y = Positive, fill = Type)) +geom_col()

######


###################### Creating a charts with lab testing results

ggplot(Labtest1, aes(x = Area, y = Log_Reduction)) + geom_col(aes(fill = Area), width = 0.5) + geom_errorbar(aes(x = Area, ymin = Log_Reduction-SEM, ymax = Log_Reduction+SEM), width = 0.1) + theme(legend.position = "none") + scale_x_discrete(labels = c("Dry Bottom", "Dry Top", "Wet Bottom", "Wet Top")) + xlab(" ") + ylab(expression("Log"[10]*" PFU Reduction")) + scale_fill_manual(values=c("#2FB9EA", "#8FB9EA", "#56B4E9", "#76DAEA")) + ylim(0, 3) + scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) + theme(panel.background = element_blank(), axis.line = element_line(color = "black"))


#### Manipulating the data between the control and treatment groups of the actual simulation 

bedsim2 <- bedsim %>% group_by(type) %>% summarise(Gloves = mean(gloves_pfu), Gown = mean(gown_pfu), Hands = mean(hands_pfu), Neck = mean(neck_pfu), glovesSSEM = sd(gloves_pfu)/sqrt(length(gloves_pfu)), gownSEM = sd(gown_pfu)/sqrt(length(gown_pfu)), handsSEM = sd(hands_pfu)/sqrt(length(hands_pfu)), neckSEM = sd(neck_pfu)/sqrt(length(neck_pfu)))
bedsim3 <- bedsim2 %>% gather(Gloves, Gown, Hands, Neck, key = "Area", value = PFU )
bedsim4 <- bedsim3 %>% gather(glovesSSEM, gownSEM, handsSEM, neckSEM, value = SEM ) %>% transmute(type = type, Area = Area, PFU = PFU, SEM = SEM)

#### Boxplot data

### Uses simulation data the object is called simdata
# The code down below is used to create the box plot used in poster the code works well although it is a bit clunky

c_Gloves <- simdata %>% filter(type == "Control") %>% transmute(type = "Control", Area = "Gloves", PFU = gloves_pfu) 
c_Gloc_Gown <-simdata %>% filter(type == "Control") %>% transmute(type = "Control", Area = "Gown", PFU = gown_pfu)                            
c_Hands <-simdata %>% filter(type == "Control") %>% transmute(type = "Control", Area = "Hands", PFU = hands_pfu) 
c_Neck <-simdata %>% filter(type == "Control") %>% transmute(type = "Control", Area = "Neck", PFU = neck_pfu) 
t_Gloves <- simdata %>% filter(type == "Treatment") %>% transmute(type = "Treatment", Area = "Gloves", PFU = gloves_pfu) 
t_Gown <- simdata %>% filter(type == "Treatment") %>% transmute(type = "Treatment", Area = "Gown", PFU = gown_pfu)                             
t_Hands <- simdata %>% filter(type == "Treatment") %>% transmute(type = "Treatment", Area = "Hands", PFU = hands_pfu) 
t_Neck <- simdata %>% filter(type == "Treatment") %>% transmute(type = "Treatment", Area = "Neck", PFU = neck_pfu) 

simdata2 <- bind_rows(c_Gloves, c_Gown, c_Hands, c_Neck, t_Gloves, t_Gown, t_Hands, t_Neck) %>% select(type, Area, PFU)

simdata2 %>% group_by(type, Area) %>% ggplot(aes(x = Area, y = PFU, fill = type)) + geom_boxplot(width = 0.5) + scale_color_discrete(labels = c("Treatment", "Control")) + theme(legend.title = element_blank()) + scale_y_continuous(expand = c(0, 0), limits = c(0, 3.2)) + ylab(expression("Log"[10]*" PFU Reduction")) + theme(legend.position = "bottom", axis.text = element_text(size = 20) , text = element_text(size = 20) ) + xlab("Before doffing                                                 After doffing")

simdata %>% gather("gloves_pfu", "gown_pfu", "hands_pfu", "neck_pfu", key = "PFU", value = )

table4a %>% gather(`1999`, `2000`, key = "Year", value = "rate")

hrhr <- tribble(~Key1, ~rate1, ~rate2,
        "x1",   4,     110,
        "x2",   5,     120,
        "x3",   6,     130)
gather(hrhr, "rate1", "rate2", key = "Rate", value = "Speed")
