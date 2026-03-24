library(readxl)

# Defining events
global_events <- data.frame(Date = as.Date(c("2008-09-15",
                                             "2013-05-02",
                                             "2020-01-01",
                                             "2022-03-01")),
                            events = c("Lehman Brothers Collapse",
                                       "Taper Tantrum",
                                       "Covid-19",
                                       "FOMC Tightening+Russia-Ukraine")) 

domestic_events <- data.frame(Date = as.Date(c("2014-01-21",
                                               "2015-01-01",
                                               "2016-01-01",
                                               "2016-11-08",
                                               "2017-10-25",
                                               "2018-01-29",
                                               "2018-09-27",
                                               "2020-03-02")),
                              events = c("Patel Committee Report",
                                         "TBS Crisis",
                                         "IBC + Regulations",
                                         "Demonetisation",
                                         "Bank Recapitalisation Plan",
                                         "PNB Scam",
                                         "IL&FS Default",
                                         "Yes Bank Crisis + Bank Mergers"))

# Systemic risk and resilience
Fig1 <- read_excel("New folder/Res_sys.xlsx")
Fig1$Date <- as.Date(Fig1$Date)
library(ggplot2)
coeff <- 10
Fig1_2 = ggplot(Fig1, aes(x = Date)) +
  geom_line(aes(y = Res_all*coeff), linewidth = 1, color =  'blue') +
  geom_line(aes(y = Sys_all*coeff), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-02-06","2025-06-27")),date_labels = "%m-%y", date_breaks = "6 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "Portfolio Resiliency", labels = function(x) x/coeff, 
                     sec.axis = sec_axis(~./coeff, name = "Portfolio Systemic Risk")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    axis.title.y.left = element_text(color = "blue", face = "bold", size = 14),
    axis.title.y.right = element_text(color = "orange", face = "bold", size = 14)
  ) +
  geom_vline(data = global_events, aes(xintercept = as.numeric(Date)), color = 'red', linetype = 'dashed') +
  geom_text(data = global_events, aes(x = Date, y = max(Fig1$Res_all*coeff), label = events), angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(data = domestic_events, aes(xintercept = as.numeric(Date)), color = 'blue', linetype = 'dashed') +
  geom_text(data = domestic_events, aes(x = Date, y = max(Fig1$Res_all*coeff), label = events), angle = 90, vjust = 1.2, hjust = 1) +
  ggtitle("Overall") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

#########################################
### EWS
Fig2 <- read_excel("New folder/EWS_sd.xlsx")
Fig2$Date <- as.Date(Fig2$Date)

# Defining events
global_events <- data.frame(Date = as.Date(c("2008-09-15",
                                             "2020-01-01")),
                            events = c("GFC",
                                       "Covid-19")) 

domestic_events <- data.frame(Date = as.Date(c("2015-01-01",
                                               "2016-01-01",
                                               "2016-11-08",
                                               "2017-10-25",
                                               "2018-09-27")),
                              events = c("TBS",
                                         "IBC",
                                         "Demonetisation",
                                         "Recapitalisation",
                                         "IL&FS"))



Fig2_1 = ggplot(Fig2, aes(x = Date)) +
  geom_line(aes(y = Score), linewidth = 1, color =  'blue') +
  geom_line(aes(y = ThSum-5), linewidth = 1, color = 'orange') +
  geom_hline(yintercept=-4, color = 'red') +
  geom_hline(yintercept=-3, color = 'red') +
  geom_hline(yintercept=-2, color = 'red') +
  scale_x_date(limits = as.Date(c("2008-04-23","2025-06-27")),date_labels = "%m-%y", date_breaks = "12 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "EWS Score", limits = c(-5,3)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(data = global_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = global_events, aes(x = Date, y = max(Fig2$Score), label = events), angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(data = domestic_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = domestic_events, aes(x = Date, y = max(Fig2$Score), label = events), angle = 90, vjust = 1.2, hjust = 1) +
  geom_text(x = as.Date("2025-01-01"), y = -1.8, label='High Risk', color='red') +
  geom_text(x = as.Date("2025-01-01"), y = -2.8, label='Moderate Risk', color='red') +
  geom_text(x = as.Date("2025-01-01"), y = -3.8, label='Low Risk', color='red')
Fig2_1

#######################################
######## EWS at frequencies
FigA3 <- read_excel("New folder/freq_ews.xlsx")
FigA3$Date <- as.Date(FigA3$Date)

FigA3_1 = ggplot(FigA3, aes(x = Date)) +
  geom_line(aes(y = short_score), linewidth = 1, color =  'blue') +
  geom_line(aes(y = short_breach-2), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-04-23","2025-06-27")),date_labels = "%m-%y", date_breaks = "12 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "EWS Score", limits = c(-2,3)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(data = global_events, aes(xintercept = as.numeric(Date)), color = 'red', linetype = 'dashed') +
  geom_text(data = global_events, aes(x = Date, y = 2.8, label = events), angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(data = domestic_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = domestic_events, aes(x = Date, y = 2.8, label = events), angle = 90, vjust = 1.2, hjust = 1) +
  ggtitle("Short Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )
FigA3_1

FigA3_2 = ggplot(FigA3, aes(x = Date)) +
  geom_line(aes(y = medium_score), linewidth = 1, color =  'blue') +
  geom_line(aes(y = medium_breach-2), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-04-23","2025-06-27")),date_labels = "%m-%y", date_breaks = "12 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "EWS Score", limits = c(-2,4)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(data = global_events, aes(xintercept = as.numeric(Date)), color = 'red', linetype = 'dashed') +
  geom_text(data = global_events, aes(x = Date, y = 3.8, label = events), angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(data = domestic_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = domestic_events, aes(x = Date, y = 3.8, label = events), angle = 90, vjust = 1.2, hjust = 1) +
  ggtitle("Medium Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )
FigA3_2

FigA3_3 = ggplot(FigA3, aes(x = Date)) +
  geom_line(aes(y = long_score), linewidth = 1, color =  'blue') +
  geom_line(aes(y = long_breach-2), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-04-23","2025-06-27")),date_labels = "%m-%y", date_breaks = "12 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "EWS Score", limits = c(-2,4)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(data = global_events, aes(xintercept = as.numeric(Date)), color = 'red', linetype = 'dashed') +
  geom_text(data = global_events, aes(x = Date, y = 3.8, label = events), angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(data = domestic_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = domestic_events, aes(x = Date, y = 3.8, label = events), angle = 90, vjust = 1.2, hjust = 1) +
  ggtitle("Long Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )
FigA3_3

FigA3_4 = ggplot(FigA3, aes(x = Date)) +
  geom_line(aes(y = all_score), linewidth = 1, color =  'blue') +
  geom_line(aes(y = all_breach-2), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-04-23","2025-06-27")),date_labels = "%m-%y", date_breaks = "12 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "EWS Score", limits = c(-2,4)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(data = global_events, aes(xintercept = as.numeric(Date)), color = 'red', linetype = 'dashed') +
  geom_text(data = global_events, aes(x = Date, y = 3.8, label = events), angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(data = domestic_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = domestic_events, aes(x = Date, y = 3.8, label = events), angle = 90, vjust = 1.2, hjust = 1) +
  ggtitle("Overall") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )
FigA3_4

combined_figureA3 <- FigA3_1 + FigA3_2 + FigA3_3 + plot_layout(nrow = 3)
combined_figureA3

#######################################
###### Robustness
Fig3 <- read_excel("New folder/Robustness.xlsx")
Fig3$Date <- as.Date(Fig3$Date)

Fig3_1 = ggplot(Fig3, aes(x = Date)) +
  geom_line(aes(y = score_cv), linewidth = 1, color =  'blue') +
  geom_line(aes(y = cv-3), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-04-23","2025-06-27")),date_labels = "%m-%y", date_breaks = "12 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "EWS Score", limits = c(-3,3)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(data = global_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = global_events, aes(x = Date, y = max(Fig3$score_cv), label = events), angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(data = domestic_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = domestic_events, aes(x = Date, y = max(Fig3$score_cv), label = events), angle = 90, vjust = 1.2, hjust = 1) +
  ggtitle("Coefficient of Variation") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )
Fig3_1
Fig3_2 = ggplot(Fig3, aes(x = Date)) +
  geom_line(aes(y = score_dr), linewidth = 1, color =  'blue') +
  geom_line(aes(y = dr-4), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-04-23","2025-06-27")),date_labels = "%m-%y", date_breaks = "12 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "EWS Score", limits = c(-4,5)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(data = global_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = global_events, aes(x = Date, y = max(Fig3$score_dr), label = events), angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(data = domestic_events, aes(xintercept = as.numeric(Date)), color = 'black', linetype = 'dashed') +
  geom_text(data = domestic_events, aes(x = Date, y = max(Fig3$score_dr), label = events), angle = 90, vjust = 1.2, hjust = 1) +
  ggtitle("Density Ratio") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )
Fig3_2

library(patchwork)
combined_figure <- Fig3_1 + Fig3_2 + plot_layout(nrow = 2)
combined_figure

#############################################
##### Network
dca_network <- ConnectednessApproach(df_zoo, 
                                    nlag=lag_value, 
                                    nfore=forecast_horizon,
                                    model="VAR",
                                    connectedness="Time",
                                    Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))
PlotNetwork(dca_network, threshold = 0.50, width = 12, height = 12)

#############################################
############ Frequency Connectedness
sample_tci <- dcaf_rolling$TCI
sample_df <- data.frame(sample_tci)
colnames(sample_df) <- c("Total", "Short", "Medium", "Long")

tci_dcaf <- ggplot(sample_df, aes(x = as.Date(rownames(sample_df)) )) +
  geom_area(aes(y = Total, fill = "Total")) + 
  geom_area(aes(y = Short, fill = "Short")) +
  geom_area(aes(y = Medium, fill = "Medium")) +
  geom_area(aes(y = Long, fill = "Long")) + 
  scale_x_date(date_labels = "%Y", date_breaks = "12 months", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0,0)) +
  scale_fill_manual(labels=c("Long","Medium","Short","Total"), values = c(4,3,2,1)) +
  xlab("") +
  ylab("Connectedness") +
  labs(fill = "Frequency") +
  theme_classic() +
  #ggtitle("Frequency Connectedness") + 
  theme(axis.text.x=element_text(hjust=0.5,vjust=0.5, size = 10),
        axis.text.y=element_text(size = 10),
        axis.title=element_text(size=12), #change font size of axis titles
        plot.title=element_text(hjust = 0.5, size=12),
        legend.position = "top")
tci_dcaf

########################################
######### Resilience at frequencies
Fig1 <- read_excel("New folder/Res_sys.xlsx")
Fig1$Date <- as.Date(Fig1$Date)
library(ggplot2)
coeff <- 10
FigA2_1 = ggplot(Fig1, aes(x = Date)) +
  geom_line(aes(y = Res_1to5*coeff), linewidth = 1, color =  'blue') +
  geom_line(aes(y = Sys_1to5*coeff), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-02-06","2025-06-27")),date_labels = "%m-%y", date_breaks = "6 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "Portfolio Resiliency", labels = function(x) x/coeff, 
                     sec.axis = sec_axis(~./coeff, name = "Portfolio Systemic Risk")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    axis.title.y.left = element_text(color = "blue", face = "bold", size = 14),
    axis.title.y.right = element_text(color = "orange", face = "bold", size = 14)
  ) +
  ggtitle("Short Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )

FigA2_2 = ggplot(Fig1, aes(x = Date)) +
  geom_line(aes(y = Res_5to20*coeff), linewidth = 1, color =  'blue') +
  geom_line(aes(y = Sys_5to20*coeff), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-02-06","2025-06-27")),date_labels = "%m-%y", date_breaks = "6 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "Portfolio Resiliency", labels = function(x) x/coeff, 
                     sec.axis = sec_axis(~./coeff, name = "Portfolio Systemic Risk")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    axis.title.y.left = element_text(color = "blue", face = "bold", size = 14),
    axis.title.y.right = element_text(color = "orange", face = "bold", size = 14)
  ) +
  ggtitle("Medium Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )

FigA2_3 = ggplot(Fig1, aes(x = Date)) +
  geom_line(aes(y = Res_20inf*coeff), linewidth = 1, color =  'blue') +
  geom_line(aes(y = Sys_20inf*coeff), linewidth = 1, color = 'orange') +
  scale_x_date(limits = as.Date(c("2008-02-06","2025-06-27")),date_labels = "%m-%y", date_breaks = "6 month", name = "Time") +
  #labs(x = 'Time') +
  scale_y_continuous(name = "Portfolio Resiliency", labels = function(x) x/coeff, 
                     sec.axis = sec_axis(~./coeff, name = "Portfolio Systemic Risk")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(
    axis.title.y.left = element_text(color = "blue", face = "bold", size = 14),
    axis.title.y.right = element_text(color = "orange", face = "bold", size = 14)
  ) +
  ggtitle("Long Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )

figureA2 <- FigA2_1 + FigA2_2 + FigA2_3 + plot_layout(nrow = 3)
figureA2

###########################################
##### EWS at frequencies
ews_stab <- read_excel("New folder/Stab_score.xlsx", sheet = "Sheet1")
colnames(ews_stab) <- c("Date", "All", "Short", "Medium", "Long")
ews_all <- uniEWS(data = cbind(ews_stab$Date,ews_stab$All), metrics = "SD", method = "expanding", burn_in = 50, threshold = 1.5)
plot(ews_all)
ews_1to5 <- uniEWS(data = cbind(ews_stab$Date,ews_stab$Short), metrics = "SD", method = "expanding", burn_in = 50, threshold = 1.5)
plot(ews_1to5)
ews_5to20 <- uniEWS(data = cbind(ews_stab$Date,ews_stab$Medium), metrics = "SD", method = "expanding", burn_in = 50, threshold = 1.5)
plot(ews_5to20)
ews_20inf <- uniEWS(data = cbind(ews_stab$Date,ews_stab$Long), metrics = "SD", method = "expanding", burn_in = 50, threshold = 1.5)
plot(ews_20inf)

freq_ews <- cbind(ews_all$EWS[,c(2,6)],ews_1to5$EWS[,c(2,6)],ews_5to20$EWS[,c(2,6)],ews_20inf$EWS[,c(2,6)])
write.xlsx(freq_ews, file = "freq_ews.xlsx")
############################################
##### PNB
PNB <- read_excel("New folder/PNB_analysis.xlsx")
PNB$Date <- as.Date(PNB$Date)

EWS_func <- function(score_df,burn_period,criterion,method_used,threshold_val){
  
  require(EWSmethods)
  time_stamp <- 1:nrow(score_df)
  avg_spect_score <- cbind(time_stamp,score_df)
  ews_eg <- uniEWS(data = avg_spect_score, metrics = criterion,method = method_used ,burn_in = burn_period,threshold = threshold_val)
  return(ews_eg)
  
}
### Parameters
burn_period <- 50
criterion <- "SD"
method_used <- "expanding"
threshold_val <- 2
pnb1 <- as.data.frame(PNB$idio)
ews_pnb1 <- EWS_func(pnb1,burn_period,criterion,method_used,threshold_val)
plot(ews_pnb1)
### merging with date
th1 <- ews_th1$EWS[,c(2,6)]
th1 <- cbind(date_roll[-(1:(burn_period-1))],th1)