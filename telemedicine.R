library(tidyverse)
data <- read_csv("C:/Users/angie/OneDrive - McGill University/Pai Team/COVET/Nigeria program data/SHOPS Plus Progam Data 2018-2021.csv", guess_max = 80000)


#number of facilities using telemedicine
sum(data$telemedicine_service, na.rm=TRUE)

#number of facilities using telemedicine for TB services
sum(data$telemedicine_for_tb_service, na.rm=TRUE)

data_telem <- data %>% 
  filter(telemedicine_service == 1)

data_telem <- data_telem[,-c(10:593)]

data_telem$no_tb_presumptive_using_telem[is.na(data_telem$no_tb_presumptive_using_telem)] <- 0

data_telem$no_tb_presumptive_tested_telem[is.na(data_telem$no_tb_presumptive_tested_telem)] <- 0

data_telem$no_tb_diagnoscouns_using_telem[is.na(data_telem$no_tb_diagnoscouns_using_telem)] <- 0

data_telem$no_tb_trtmonitoring_using_telem[is.na(data_telem$no_tb_trtmonitoring_using_telem)] <- 0

data_telem <- data_telem %>% 
  mutate(telemedicine_for_tb_type = ifelse(telemedicine_for_tb_service == 1 & telemedicine_for_tb_screening == 1 & tb_diagnosedcounsel_using_telem != 1 & tb_trtmonitoring_using_telem != 1, "Screening Only",
      ifelse(telemedicine_for_tb_service == 1 & telemedicine_for_tb_screening != 1 & tb_diagnosedcounsel_using_telem == 1 & tb_trtmonitoring_using_telem != 1, "Diagnosis/Counseling Only", 
             ifelse(telemedicine_for_tb_service == 1 & telemedicine_for_tb_screening != 1 & tb_diagnosedcounsel_using_telem != 1 & tb_trtmonitoring_using_telem == 1, "Monitoring Only", 
                    ifelse(telemedicine_for_tb_service == 1 & telemedicine_for_tb_screening == 1 & tb_diagnosedcounsel_using_telem == 1 &       tb_trtmonitoring_using_telem != 1, "Screening & Diagnosis", 
                           ifelse(telemedicine_for_tb_service == 1 & telemedicine_for_tb_screening != 1 & tb_diagnosedcounsel_using_telem == 1 & tb_trtmonitoring_using_telem == 1, "Diagnosis & Monitoring", 
                                  ifelse(telemedicine_for_tb_service == 1 & telemedicine_for_tb_screening == 1 & tb_diagnosedcounsel_using_telem != 1 & tb_trtmonitoring_using_telem == 1, "Screening & Monitoring", 
                                         ifelse(telemedicine_for_tb_service == 1 & telemedicine_for_tb_screening == 1 & tb_diagnosedcounsel_using_telem == 1 & tb_trtmonitoring_using_telem == 1, "Screening, Diagnosis, & Monitoring", 
                                                ifelse(telemedicine_for_tb_service == 1 & telemedicine_for_tb_screening != 1 & tb_diagnosedcounsel_using_telem != 1 & tb_trtmonitoring_using_telem != 1, "Unspecified", NA)))))))))

data_telem$telemedicine_for_tb_type <- factor(data_telem$telemedicine_for_tb_type, levels = c("Screening Only", "Diagnosis/Counseling Only", "Monitoring Only", "Screening & Diagnosis", "Diagnosis & Monitoring", "Screening & Monitoring", "Screening, Diagnosis, & Monitoring", "Unspecified", NA))


data_telem$no_patients = data_telem$no_tb_presumptive_tested_telem + data_telem$no_tb_diagnoscouns_using_telem + data_telem$no_tb_trtmonitoring_using_telem

data_telem$no_patients2 = data_telem$no_tb_presumptive_using_telem + data_telem$no_tb_presumptive_tested_telem + data_telem$no_tb_diagnoscouns_using_telem + data_telem$no_tb_trtmonitoring_using_telem

telem_summary <- data_telem %>% 
  group_by(state) %>% 
  summarize("Facilities offering Telemedicine" = sum(telemedicine_service), 
            "Facilities offering Telemedicine for TB" = sum(telemedicine_for_tb_service), 
            "Facilities offering TB Screening via Telemedicine" = sum(telemedicine_for_tb_screening, na.rm=TRUE), 
            "Facilities offering TB Diagnosis/Counseling via Telemedicine" = sum(tb_diagnosedcounsel_using_telem, na.rm=TRUE), 
            "Facilities offering TB Treatment Monitoring via Telemedicine" = sum(tb_trtmonitoring_using_telem, na.rm=TRUE),
            "TB Patients using Telemedicine (any kind)" = sum(no_tb_presumptive_using_telem),
            "Patients Tested for TB using Telemedicine" = sum(no_tb_presumptive_tested_telem),
            "Patients Diagnosed/Counseled for TB using Telemedicine" = sum(no_tb_diagnoscouns_using_telem),
            "Patients Monitored for TB Treatment using Telemedicine" = sum(no_tb_trtmonitoring_using_telem))

telem_summary <- data.frame(t(telem_summary)[-1,])
names(telem_summary) = c("Kano", "Lagos")
telem_summary$Kano = as.numeric(telem_summary$Kano)
telem_summary$Lagos = as.numeric(telem_summary$Lagos)
write.csv(telem_summary, file = "telem_summary.csv")

ggplot(filter(data_telem, telemedicine_for_tb_service == 1), aes(x=telemedicine_for_tb_type, fill = telemedicine_for_tb_type))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 3)+
  ylim(c(0,20))+
  facet_wrap(~state)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "blank",plot.margin = unit(c(0.5,0.5,1,1), "cm"))+
  labs(title = "Facilities offering each type of Telemedicine Services for TB", y = "Count of Facilities", x = NULL, fill = "Telemedicine Services for TB")
ggsave("~/GitHub/nigeria-shops-analysis/outputs/img/telemedicine5.png", width = 8, height = 5, units = "in")
