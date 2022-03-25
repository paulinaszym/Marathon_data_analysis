#Wersja kodu w R
#Pakiety
library(tidyverse)
library(ggplot2)
library(graphics)
library(lattice)
library(plotly)
library(rbokeh)
library(gridExtra)
library(viridis)
library(crayon)
library(latticeExtra)

#Wczytywanie i zapisywanie danych
dane_surowe<-read.csv("marathon.csv", skip=1, header=FALSE, col.names=c("Order", "Age", "Gender", "Time"))
write.csv(dane_surowe, "Szymanek_dane_surowe.csv")
any(is.na(dane_surowe))
dane_surowe
str(dane_surowe)
summary(dane_surowe)
glimpse(dane_surowe)
class(dane_surowe)
typeof(dane_surowe)
names(dane_surowe)
levels(dane_surowe)
dim(dane_surowe)
head(dane_surowe)
tail(dane_surowe)
marathon<-dane_surowe%>% arrange(Order)%>% mutate(Gender=ifelse(as.character(Gender)==" M", "Male", "Female"))
write.csv(marathon, "Szymanek_dane_przeksztalcone.csv")

#Praca z danymi
salm<-make_style("indianred1")
cat(salm$underline$bold("All data summary:\n"))
summary(marathon)
m_data<-marathon%>% filter(Gender=="Male")
f_data<-marathon%>% filter(Gender=="Female")
cat(salm$underline$bold("Summary for males:\n"))
summary(m_data)
cat(salm$underline$bold("Summary for females:\n"))
summary(f_data)
marathon%>% group_by(Age)%>% summarize(medianTime=median(Time), meanTime=mean(Time))
marathon%>%group_by(Age)%>% summarize(NumOfRunners=n_distinct(Time))%>% arrange(desc(NumOfRunners))
marathon%>%group_by(Age)%>% summarize(maxTime=max(Time), minTime=min(Time))%>% filter(maxTime!=minTime)
co<-viridis_pal(option="magma", direction=-1)(10)
colo<-c(co[8], co[2], co[5])

#Wykres 1
pie(table(marathon$Gender), col=colo, border="white", main="Gender of runners", font=2)

#Wykres 2
ggplot(marathon, aes(x=Age), y=as.numeric(stat(count)))+ geom_bar(aes(fill=as.numeric(stat(count))))+ 
 scale_fill_viridis_c(option="magma", direction=-1, name="Number of runners")+
 geom_vline(xintercept=mean(marathon$Age), linetype="dashed")+
 labs(x="Age", y="Number of runners")+ ggtitle("Runners by age")+ 
 theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.major.y=element_line(colour="#875faf"), panel.grid.minor.y=element_line(colour="#875f87"), panel.background=element_rect(colour="#5f005f", fill="white"), plot.background=element_rect(fill="#e4e4e4")) 

#Wykres 3
figure(title="Time by gender")%>%

#Wykres 4
hist1<-histogram(~f_data$Age,group=Gender, data=marathon, xlab="Female", ylab="Percentage", col=colo[1], border="white", par.settings=ggplot2like(),lattice.options=ggplot2like.opts())
hist2<-histogram(~m_data$Age,group=Gender, data=marathon, xlab="Male", ylab="", col=colo[2], border="white", par.settings=ggplot2like(),lattice.options=ggplot2like.opts())
grid.arrange(hist1, hist2, ncol=2, top="Age of runners by gender") 

#Wykres 5
time_mean<-marathon%>% group_by(Age)%>% summarize(meanTime=mean(Time))
co<-viridis_pal(option="magma", direction=-1)(10)
time_meanf<-f_data%>% group_by(Age)%>% summarize(meanTimef=mean(Time))
time_meanm<-m_data%>% group_by(Age)%>% summarize(meanTimem=mean(Time))
time_meanf<-merge(time_mean,time_meanf, all=TRUE)%>%select(Age,meanTimef)
time_meanm<-merge(time_mean,time_meanm, all=TRUE)%>%select(Age,meanTimem)
marathon%>% plot_ly(x=~time_mean$Age)%>% layout(title="Time by age", xaxis=list(title="Age", gridcolor="#d7afaf"), yaxis=list(title="Mean time", gridcolor="#d7afaf"), legend = list(x=0.1, y=0.9), paper_bgcolor="#e4e4e4")%>%
  add_trace(y=~time_mean$meanTime,mode='lines', name="All", line=list(color=colo[3]))%>%
  add_trace(y=~time_meanf$meanTimef,mode='lines', name="Females", line=list(color=colo[1], dash = 'dash'))%>%
  add_trace(y=~time_meanm$meanTimem,mode='lines', name="Males", line=list(color=colo[2], dash = 'dash'))
