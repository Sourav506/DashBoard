---
title: "DripByRage"
output:
 flexdashboard::flex_dashboard:
 theme:
 version: 4
 bg: "#101010"
 fg: "#FDF7F7"
 primary: "#732C1A"
 navbar-bg: "#3ADAC6"
 base_font:
 google: Prompt
 heading_font:
 google: Sen
 code_font:
 google:
 family: JetBrains Mono
 local: false
 orientation: rows
 vertical_layout: fill
 social: ["twitter","instagram","menu"]
 source_code: embed
---
{r setup, include=FALSE}
library(flexdashboard)
library(knitr)
library(DT)
library(rpivotTable)
library(ggplot2)
library(dplyr)
library(plotly)
library(openintro)
library(highcharter)
{r}
mycolors<-c("#4D4DFF","#FFAD00","#44D62C","#D22730")
{r read}
data<- read.csv("Sourav.csv")
data1<- read.csv("table.csv")
Order<-read.csv("Order.csv")
Finance<-read.csv("Finance.csv")
Geo<-read.csv("Geo.csv")
Id<-read.csv("Id.csv")
Time<-read.csv("Time.csv")
Product1<-read.csv("Product.csv")
Sales And Profit
================================================
Row
------------------------------------------------
###
{r}
include_graphics("DripByRage.jpg",dpi=.5)
### Total Sales
{r}
valueBox(paste("$",format(round(sum(Finance$Sales) / 1e3, 1), trim = TRUE),"K"),
 icon = "fa-shopping-cart")
### Average Of Delivery Days
{r}
gauge(round(mean(Time$Deliverydays),
 digits =1),
 min= 0,
 max = 20,
 gaugeSectors(success = c(0,3),
 warning =c(3,10),
 danger = c(10,20),
 colors = c('red','yellow','green')))
### Total Profit by global store
{r}
valueBox("$",paste(format(round(sum(Finance$Sales) / 1e6, 1)),"M"),
 icon='fa-money')
Row
-----------------------------------------------
### No. of Sales Per Year
{r}
p1<- Time %>%
 group_by(year) %>%
 summarise(z = n()) %>%
 plot_ly(
 labels = ~year,
 values = ~z,
 marker = list(color = mycolors)) %>%
 add_pie(hole=0.2) %>%
 layout(xaxis=list(zeroline = F,
 showline = F,
 showticklabels = F,
 showgrid = F),
 yaxis = list(zeroline = F,
 showline = F,
 showticklabels = F,
 showgrid = F)
 )
p1
### Profit Per Year
{r}
da<-right_join(Finance,Time,by="Row.ID")
p2 <- da %>%
 group_by(year) %>%
 summarise(m = sum(Profit)) %>%
 plot_ly(
 x = ~year,
 y = ~m,
 color = rainbow(4),
 type = 'bar') %>%
 layout(xaxis = list(title = "YEAR"),
 yaxis = list(title = "PROFIT"))
 p2
Row
--------------------------------------------------
### Category vs sale
{r}
da1<-right_join(Product1,Order,by="Row.ID")
p2 <- da1 %>%
 group_by(Category) %>%
 summarise(m = sum(Quantity)) %>%
 plot_ly(
 x = ~Category,
 y = ~m,
 color = c("#7b68ee","#dda0dd","#ba55d3"),
 type = 'bar') %>%
 layout(xaxis = list(title = ""),
 yaxis = list(title = "Quantity"))
 p2
### Average Delivery per year
{r}
p4 <- Time %>%
 group_by(year) %>%
 summarise(z = mean(Deliverydays))%>%
 plot_ly(
 labels = ~year,
 values = ~z,
 marker = list(color = rainbow(4))) %>%
 add_pie(hole=0.2) %>%
 layout(xaxis=list(zeroline = F,
 showline = F,
 showticklabels = F,
 showgrid = F),
 yaxis = list(zeroline = F,
 showline = F,
 showticklabels = F,
 showgrid = F))

 p4
Product And Profit
==================================================
### Profit By Product Name
{r}
p3 <- data %>%
 group_by(Product.Name) %>%
 summarise(m = sum(Profit)) %>%
 plot_ly(
 x = ~m,
 y = ~Product.Name,
 color = '99FF1D',
 type = 'bar') %>%
 layout(xaxis = list(title = "PROFIT"),
 yaxis = list(title = "PRODUCT NAME"))
 p3
Data Table
=========================================================
{r}
datatable(data1,
 caption = "Global SuperStore",
 rownames = T,
 filter = "top",
 options = list(pageLength=50))
Summary{data-orientation=columns}
==========================================================
Column {data-width = 100}
-----------------------------------------------------------
### No. Of Rows
{r}
valueBox(nrow(data),
 icon='fa-store')
### Diff. Type of Products
{r}
valueBox(n_distinct(data$Product.Name),
 icon='fa-product-hunt')
### Diff. Countries for Sales
{r}
valueBox(n_distinct(data$Country),
 icon='fa-flag')
### Quantity Of Sales
{r}
valueBox(sum(data$Quantity),
 icon ='fa-box')
column
-----------------------------------------------------------------
Summary
* This Project Uses following Library
\n
1.flexdashboard
\n
2.knitr
\n
3.DT
\n
4.ggplot2
\n
5.dplyr
\n
6.plotly
\n
7.openintro
* Basic Work Flow is Data Fetching-> Data Cleaning->Data Visualisation
* Here we Plotted Different Graphs and charts using ploty,ggplot etc.
Designed by: Sourav Choubey
Enroll. No.:102016075
Group: 3CS11
Project: DashBoard On General SuperStore