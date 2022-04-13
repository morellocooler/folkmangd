# load packages
library(anytime)
library(data.table)
library(egg)
library(forecast)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(methods)
library(pxweb)
library(readxl)
library(reshape2)
library(rvest)
library(stringr)
library(tibble)
library(tidyverse)
library(writexl)
library(XML)
library(xml2)


getwd()


# Hämta data från SCB

url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkManad"

query <- pxweb_query("./queries/scbfolkmängd.json")
pxd <- pxweb_get(url, query)
pxd

pxdf <- as.data.frame(pxd,  column.name.type = "text", variable.value.type = "text")

 # not working!


# Försök 2 Hämta data från SCB via länk till en sparad sökning

url2 <- "https://www.statistikdatabasen.scb.se/sq/123683"

data_in <- data.frame(read.csv(url2, skip = 2, header = TRUE, sep = ","))

df <- data_in %>%
      filter(ålder!="uppgift saknas") %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~"Total"))) %>%
      rename_all(funs(str_replace_all(., "X", ""))) %>%
      melt() %>%
      filter(region == "Total") %>%
      mutate(rowid = 1:n()) %>%
      filter(rowid>84) %>%
      select(4:6) %>%
      rename(Tid = variable,
             Folkmängd = value) %>%
      mutate(Tid = str_replace(Tid, "M", "-"),
             Tid = trimws(Tid, "both"),
             Tid = anydate(Tid),
             Tid = ceiling_date(Tid, "month") - 1)


             

# Plot

plot_20072024 <- ggplot(df, aes(Tid, Folkmängd, label = Folkmängd))+
      geom_point(color="black", size = 0.5) +
      #geom_line(aes(group = 1)) +
      geom_hline(yintercept=400000, linetype="dashed", color = "red") +
      labs(title="Folkmängd i Uppsala län")+
      theme(plot.title = element_text(hjust=0.5),
            legend.position='none',
            plot.margin = unit(c(0,0,2,0), "cm"),
            axis.title.y=element_blank(),
            axis.ticks = element_blank())+
      scale_x_date(name = NULL, 
                   date_breaks = '1 year',
                   date_labels = '%Y',
                   limits = as.Date(c('2007-01-01','2024-01-01')),
                   expand = expansion(add = c(0, 100)))+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                         breaks = seq ( from = 320000, to = 405000, by = 10000),
                         minor_breaks=seq(from = 320000, to = 400000, by = 5000))+
      geom_point(aes(x = tail(Tid, 1), y = tail(Folkmängd, 1)), 
                 color = "red",
                 size = 1, show.legend = TRUE)+
      geom_label_repel(data=subset(df, Tid == "2022-02-28"),
                       label = "2022-02-28: \n 395.921 invånare",
                       cex=3.5,
                        nudge_x = 0,
                        nudge_y = -20000,
                        arrow = arrow(angle = 20, length = unit(4, "mm"), ends = "last", type = "open"))

plot_20072024

plot_20192024 <- {df %>% 
      filter(Tid >= "2019-01-01") %>%
      ggplot(aes(Tid, Folkmängd))+
      geom_point(aes(color="red")) + geom_line(aes(group = 1)) +
      geom_hline(yintercept=400000, linetype="dashed", color = "red") +
      labs(title="Folkmängd i Uppsala län")+
      theme(plot.title = element_text(hjust=0.5),
            legend.position='none',
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_x_date(name = NULL,
             date_breaks = "6 months",
             date_labels = '%b-%Y',
             date_minor_breaks = "1 month",
             limits = as.Date(c("2019-01-01","2023-12-01")),
             expand = c(0, 0))

plot_20192024
}



# Time series and forecast

myts <- ts(df$Folkmängd, frequency=12, start = c(2007, 01), end = c(2022, 02))
plot(myts)

myds_month <- decompose(myts)
plot(myds_month)


myholtts <-HoltWinters(myts)

myhw <- forecast(myholtts, h = 24, findfrequency = TRUE)
plot(myhw)


plot_prognos <- autoplot(myhw, showgap = FALSE) + ggtitle("Prognos") + 
            theme(plot.title = element_text(hjust = 0.5),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.ticks = element_blank()) +
      scale_x_continuous(breaks = seq(from = 2018, to =  2024, by = 1),
                         minor_breaks = seq(from = 2018, to =  2024, by = 1/12),
                         limits = c(2018,2024),
                         expand = expansion(add = c(0, 1/12))) +
      scale_y_continuous(limits = c(360000 , 420000),
                         breaks = seq(from = 360000, to = 420000, by = 10000),
                         labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))+
      autolayer(myhw, showgap = F)+
      geom_hline(yintercept=400000, linetype="dashed", color = "red")
 
plot_prognos                       
                         
                    
     
# Export plots

arrange <- grid.arrange(plot_20072024, plot_prognos, 
          ncol= 1,
          heights = unit(c(12,12), "cm"),
          widths = unit(17, "cm"))

annotated <- annotate_figure(arrange, 
                             fig.lab = "   \n 2022-04-08  ", fig.lab.size = 10,
                             fig.lab.pos = "top.right",
                             bottom = text_grob("stefanie.bastani@regionuppsala.se \n    ",
                                                #hjust = "left",
                                                #face = "italic", 
                                                size = 10))
                                                
ggsave(filename="./output/folkmängd_2204.pdf", 
       plot = annotated, 
       device = cairo_pdf, 
       width = 210, 
       height = 297, 
       units = "mm")
