##creates choropleth maps for various social, economic, and medical variables 
##using Area Health Resource File, 2020-2021 county-level data

##read in countydata csv file
countydata <- read.csv(file = "countydata.csv")
#3230 counties (rows) in dataset, 41 variables (columns in dataset)

##create new data frame with only North Carolina counties
nc_countydata <- countydata[countydata$statename == "North Carolina",]
#north carolina has 100 counties

##create new data frame with only CBCS counties
cbcs_counties <- nc_countydata[nc_countydata$cbcs == 1,]
#there's 44 cbcs counties
#there's 41 variables (columns)

##testing if variables have NAs
which(is.na(cbcs_counties$deeppov))
##finds means and medians of variables 
class(cbcs_counties$cbsacode)
mean(cbcs_counties$cbsacode, na.rm = TRUE)
median(cbcs_counties$cbsacode, na.rm = TRUE)

#do this for white to blue (scale_fill_distiller() does white to blue, keep direction = 1 to have low be white and high be blue)
library(RColorBrewer)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pcthisp)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Population That Is Hispanic Per County", subtitle = "Pcthisp Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "%\nHispanic") +
        scale_fill_distiller(palette = "Blues", direction = 1)

###plots for NC counties
library(ggplot2)
library(maps)
library(dplyr) 
nc_countydata$countyname <- tolower(nc_countydata$countyname)
county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymap <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymap2 <- arrange(mymap, group, order)

#rhc with multiple variables

##number of rhcs per 10,000 people in rural counties
#select data for rural counties
ruraldata <- nc_countydata[(nc_countydata$ruralcont > 7),]
#calculates number of rhcs per 10,000 people
rhcr <- ((ruraldata$rhc/ruraldata$popest2020)*10000)
#creates new dataframe with rhcr and ruraldata
ruraldata2 <- cbind(ruraldata,rhcr)
colnames(ruraldata2)[42] <- "rhcr"
#creates new dataframe with ruraldata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, ruraldata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = rhcr)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Rural Health Clinics Per 10,000 People in Rural Counties", subtitle = "Rhc, Popest2020 and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nRHCs") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,2.65))+
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = rhcr)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Rural Health Clinics Per 10,000 People in Rural Counties", subtitle = "Rhc, Popest2020 and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nRHCs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,2.65), direction = 1)  +
        borders(database = "state", regions = "north carolina", colour= "black")

###oncology with multiple variables

##number of hospitals with oncology services per 10,000 people 
#calculates number of hospitals with oncology services per 10,000 people
onc10 <- ((nc_countydata$oncology/nc_countydata$popest2020)*10000)
#creates new dataframe with nc_countydata and number of hospitals with oncology services per 10,000 people
nc_countydata2 <- cbind(nc_countydata,onc10)
colnames(nc_countydata2)[42] <- "onc10"
#creates new dataframe with nc_countydata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = onc10)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per 10,000 People", subtitle = "Oncology and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,0.73))
#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = onc10)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per 10,000 People", subtitle = "Oncology and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_distiller(palette = "Blues", limits = c(0,0.73), direction = 1) 

##number of hospitals with oncology services per 10 square miles
#calculates number of hospitals with oncology services per 10 square miles
oncmi <- ((nc_countydata$oncology/nc_countydata$landarea)*10)
#creates new dataframe with nc_countydata and number of hospitals with oncology services per 10 square miles
nc_countydata2 <- cbind(nc_countydata,oncmi)
colnames(nc_countydata2)[42] <- "oncmi"
#creates new dataframe with nc_countydata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = oncmi)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per 10 Square Miles", subtitle = "Oncology and Landarea Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,0.07))
#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = oncmi)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per 10 Square Miles", subtitle = "Oncology and Landarea Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_distiller(palette = "Blues", limits = c(0,0.07), direction = 1) 

##percent of hospitals that have oncology services
#calculates percent of hospitals with oncology services
oncp <- ((nc_countydata$oncology/nc_countydata$totalhospital)*100)
#creates new dataframe with nc_countydata and percent of hospitals with oncology services
nc_countydata2 <- cbind(nc_countydata,oncp)
colnames(nc_countydata2)[42] <- "oncp"
#replaces all NAs with -1
nc_countydata2$oncp[is.na(nc_countydata2$oncp)] <- -1
#cuts percent of hospitals with oncology variable into intervals and replace -1 with "no hospitals exist in county" in county
#change breaks and labels when you want different intervals or number of cuts
oncpa <- cut(x = nc_countydata2$oncp, breaks= c(-1,-90,10,20,30,40,50,60,70,80,90,100),
             labels = c("No Hospitals\nExist in\nCounty","[0%,10%]","(10%,20%]","(20%,30%]","(30%,40%]","(40%,50%]","(50%,60%]","(60%,70%]","(70%,80%]","(80%,90%]","(90%,100%]"),
             include.lowest = FALSE, right= TRUE)
#creates new dataframe with nc_countydata2 and cut variable
nc_countydata3 <- cbind(nc_countydata2,oncpa)
colnames(nc_countydata3)[43] <- "oncpa"
#creates new dataframe with nc_countydata3 and coordinates of polygons of counties
mymapx <- merge(county_map2, nc_countydata3, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#creates color palette (change (11) when you change intervals or number of cuts)
pal <- colorRampPalette(c("white", "grey", "black"))(11)
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = oncpa)) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = pal, drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "% of\nHospitals") +
        labs(y = "", x = "", title = "Percent of Hospitals That Have Oncology Services By County", subtitle = "Oncology and Totalhospital Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.56), legend.title= element_text(size=10), legend.justification = c("left","bottom"), axis.text = element_blank())

###no care and multiple variables

##no care and persistent poverty
#adds empty column to nc_countydata
nc_countydata['new_col'] <- NA
#uses system below to label entries based on no care and persistent poverty
#blue = no care not persistent poverty (1)
#red = persistent poverty not no care (2)
#purple = no care and persistent poverty (3)
#white = not no care and not persistent poverty (4)
for(i in 1:100){
        if(nc_countydata$nocare[i] == 1 & nc_countydata$perspoverty[i] == 0){
                nc_countydata[i,'new_col'] <- 1
        } else if(nc_countydata$nocare[i] == 0 & nc_countydata$perspoverty[i] == 1){
                nc_countydata[i,'new_col'] <- 2
        } else if(nc_countydata$nocare[i] == 1 & nc_countydata$perspoverty[i] == 1){
                nc_countydata[i,'new_col'] <- 3
        } else if(nc_countydata$nocare[i] == 0 & nc_countydata$perspoverty[i] == 0){
                nc_countydata[i,'new_col'] <- 4
        }
}
#combines nc_countydata with county_map2
mymapx <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("No Care, Not Persistent Poverty", "Not No Care, Persistent Poverty","No Care, Persistent Poverty","Not No Care, Not Persistent Poverty"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "No Care\n& Persistent\nPoverty") +
        labs(y = "", x = "", title = "No Care and Persistent Poverty By County", subtitle = "Nocare and Perspoverty Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.65), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank())

##no care and rural counties
#adds empty column to nc_countydata
nc_countydata['new_col'] <- NA
#uses system below to label entries based on no care and rural counties
#blue = no care not rural county (1)
#red = rural county not no care (2)
#purple = no care and rural county (3)
#white = not no care and not rural county (4)
for(i in 1:100){
        if(nc_countydata$nocare[i] == 1 & nc_countydata$ruralcont[i] < 8){
                nc_countydata[i,'new_col'] <- 1
        } else if(nc_countydata$nocare[i] == 0 & nc_countydata$ruralcont[i] > 7){
                nc_countydata[i,'new_col'] <- 2
        } else if(nc_countydata$nocare[i] == 1 & nc_countydata$ruralcont[i] > 7){
                nc_countydata[i,'new_col'] <- 3
        } else if(nc_countydata$nocare[i] == 0 & nc_countydata$ruralcont[i] < 8){
                nc_countydata[i,'new_col'] <- 4
        }
}
#combines nc_countydata with county_map2
mymapx <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("No Care, Not Rural County", "Not No Care, Rural County","No Care, Rural County","Not No Care, Not Rural County"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "No Care\n& Rural\nCounty") +
        labs(y = "", x = "", title = "No Care and Rural County Status By County", subtitle = "Nocare and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.65), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank())


###Low employment and multiple variables

##low employment and persistent poverty
#adds empty column to nc_countydata
nc_countydata['new_col'] <- NA
#uses system below to label entries based on low employment and persistent poverty
#blue = low employment not persistent poverty (1)
#red = persistent poverty not low employment (2)
#purple = low employment and persistent poverty (3)
#white = not low employment and not persistent poverty (4)
for(i in 1:100){
        if(nc_countydata$lowemply[i] == 1 & nc_countydata$perspoverty[i] == 0){
                nc_countydata[i,'new_col'] <- 1
        } else if(nc_countydata$lowemply[i] == 0 & nc_countydata$perspoverty[i] == 1){
                nc_countydata[i,'new_col'] <- 2
        } else if(nc_countydata$lowemply[i] == 1 & nc_countydata$perspoverty[i] == 1){
                nc_countydata[i,'new_col'] <- 3
        } else if(nc_countydata$lowemply[i] == 0 & nc_countydata$perspoverty[i] == 0){
                nc_countydata[i,'new_col'] <- 4
        }
}
#combines nc_countydata with county_map2
mymapx <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("Low Employment, Not Persistent Poverty", "Not Low Employment, Persistent Poverty","Low Employment, Persistent Poverty","Not Low Employment, Not Persistent Poverty"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "Low Employment\n& Persistent\nPoverty") +
        labs(y = "", x = "", title = "Low Employment and Persistent Poverty By County", subtitle = "Lowemply and Perspoverty Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.65), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank())


###low education and multiple variables

##low education and low employment
#adds empty column to nc_countydata
nc_countydata['new_col'] <- NA
#uses system below to label entries based on low education and low employment
#blue = low education not low employment (1)
#red = low employment not low education (2)
#purple = low education and low employment (3)
#white = not low education and not low employment (4)
for(i in 1:100){
        if(nc_countydata$loweduc[i] == 1 & nc_countydata$lowemply[i] == 0){
                nc_countydata[i,'new_col'] <- 1
        } else if(nc_countydata$loweduc[i] == 0 & nc_countydata$lowemply[i] == 1){
                nc_countydata[i,'new_col'] <- 2
        } else if(nc_countydata$loweduc[i] == 1 & nc_countydata$lowemply[i] == 1){
                nc_countydata[i,'new_col'] <- 3
        } else if(nc_countydata$loweduc[i] == 0 & nc_countydata$lowemply[i] == 0){
                nc_countydata[i,'new_col'] <- 4
        }
}
#combines nc_countydata with county_map2
mymapx <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("Low Education, Not Low Employment", "Not Low Education, Low Employment","Low Education, Low Employment","Not Low Education, Not Low Employment"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "Low Education\n& Low\nEmployment") +
        labs(y = "", x = "", title = "Low Education and Low Employment By County", subtitle = "Loweduc and Lowemply Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.56), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank())


##low education and persistent poverty
#adds empty column to nc_countydata
nc_countydata['new_col'] <- NA
#uses system below to label entries based on low education and persistent poverty
#blue = low education not persistent poverty (1)
#red = persistent poverty not low education (2)
#purple = low education and persistent poverty (3)
#white = not low education and not persistent poverty (4)
for(i in 1:100){
        if(nc_countydata$loweduc[i] == 1 & nc_countydata$perspoverty[i] == 0){
                nc_countydata[i,'new_col'] <- 1
        } else if(nc_countydata$loweduc[i] == 0 & nc_countydata$perspoverty[i] == 1){
                nc_countydata[i,'new_col'] <- 2
        } else if(nc_countydata$loweduc[i] == 1 & nc_countydata$perspoverty[i] == 1){
                nc_countydata[i,'new_col'] <- 3
        } else if(nc_countydata$loweduc[i] == 0 & nc_countydata$perspoverty[i] == 0){
                nc_countydata[i,'new_col'] <- 4
        }
}
#combines nc_countydata with county_map2
mymapx <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("Low Education, Not Persistent Poverty", "Not Low Education, Persistent Poverty","Low Education, Persistent Poverty","Not Low Education, Not Persistent Poverty"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "Low Education\n& Persistent\nPoverty") +
        labs(y = "", x = "", title = "Low Education and Persistent Poverty By County", subtitle = "Loweduc and Perspoverty Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.65), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank())

###high poverty status (highpovstatus) with multiple variables

##high poverty status only shown for rural counties
#select data for rural counties
ruraldata <- nc_countydata[(nc_countydata$ruralcont > 7),]
#combine ruraldata and county_map2
mymapx <- merge(county_map2, ruraldata, by.x = "subregion", by.y= "countyname")
mymap2x <- arrange(mymapx, group, order)
#plot
ggplot(mymap2x, aes(x = long, y = lat, group = group, fill = as.factor(highpovstatus))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "High Poverty Status by Rural County", subtitle = "Highpovstatus Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.57), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "Legend") +
        scale_fill_manual(values = c("1" = "#ef3b2c","0" = "white"), labels = c("High Poverty","Not High Poverty")) +
        borders(database = "state", regions = "north carolina", colour= "black")

##high poverty status and low education 
#adds empty column to nc_countydata
nc_countydata['new_col'] <- NA
#uses system below to label entries based on high pov status and low education
#blue = high poverty status not low education (1)
#red = low education not high poverty status (2)
#purple = high poverty status and low education (3)
#white = not high poverty status and not low education (4)
for(i in 1:100){
        if(nc_countydata$highpovstatus[i] == 1 & nc_countydata$loweduc[i] == 0){
                nc_countydata[i,'new_col'] <- 1
        } else if(nc_countydata$highpovstatus[i] == 0 & nc_countydata$loweduc[i] == 1){
                nc_countydata[i,'new_col'] <- 2
        } else if(nc_countydata$highpovstatus[i] == 1 & nc_countydata$loweduc[i] == 1){
                nc_countydata[i,'new_col'] <- 3
        } else if(nc_countydata$highpovstatus[i] == 0 & nc_countydata$loweduc[i] == 0){
                nc_countydata[i,'new_col'] <- 4
        }
}
#combines nc_countydata with county_map2
mymapx <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("High Poverty, Not Low Education", "Not High Poverty, Low Education","High Poverty, Low Education","Not High Poverty, Not Low Education"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "High Poverty\nStatus &\nLow Education") +
        labs(y = "", x = "", title = "High Poverty Status and Low Education By County", subtitle = "Highpovstatus and Loweduc Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.56), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank())

##high poverty status and low employment 
#adds empty column to nc_countydata
nc_countydata['new_col'] <- NA
#uses system below to label entries based on high pov status and low employment
#blue = high poverty status not low employment (1)
#red = low employment not high poverty status (2)
#purple = high poverty status and low employment (3)
#white = not high poverty status and not low employment (4)
for(i in 1:100){
        if(nc_countydata$highpovstatus[i] == 1 & nc_countydata$lowemply[i] == 0){
                nc_countydata[i,'new_col'] <- 1
        } else if(nc_countydata$highpovstatus[i] == 0 & nc_countydata$lowemply[i] == 1){
                nc_countydata[i,'new_col'] <- 2
        } else if(nc_countydata$highpovstatus[i] == 1 & nc_countydata$lowemply[i] == 1){
                nc_countydata[i,'new_col'] <- 3
        } else if(nc_countydata$highpovstatus[i] == 0 & nc_countydata$lowemply[i] == 0){
                nc_countydata[i,'new_col'] <- 4
        }
}
#combines nc_countydata with county_map2
mymapx <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("High Poverty, Not Low Employment", "Not High Poverty, Low Employment","High Poverty, Low Employment","Not High Poverty, Not Low Employment"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "High Poverty\nStatus &\nLow Employment") +
        labs(y = "", x = "", title = "High Poverty Status and Low Employment By County", subtitle = "Highpovstatus and Lowemply Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.56), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank())

##high poverty status and no care 
#adds empty column to nc_countydata
nc_countydata['new_col'] <- NA
#uses system below to label entries based on high pov status and no care
#blue = high poverty status not no care (1)
#red = no care not high poverty status  (2)
#purple = high poverty status and no care (3)
#white = not high poverty status and not no care (4)
for(i in 1:100){
        if(nc_countydata$highpovstatus[i] == 1 & nc_countydata$nocare[i] == 0){
                nc_countydata[i,'new_col'] <- 1
        } else if(nc_countydata$highpovstatus[i] == 0 & nc_countydata$nocare[i] == 1){
                nc_countydata[i,'new_col'] <- 2
        } else if(nc_countydata$highpovstatus[i] == 1 & nc_countydata$nocare[i] == 1){
                nc_countydata[i,'new_col'] <- 3
        } else if(nc_countydata$highpovstatus[i] == 0 & nc_countydata$nocare[i] == 0){
                nc_countydata[i,'new_col'] <- 4
        }
}
#combines nc_countydata with county_map2
mymapx <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("High Poverty, Not No Care", "Not High Poverty, No Care","High Poverty, No Care","Not High Poverty, Not No Care"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "High Poverty\nStatus &\nNo Care") +
        labs(y = "", x = "", title = "High Poverty Status and No Care By County", subtitle = "Highpovstatus and Nocare Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank())


###federally qualified health centers (fqhc) with multiple variables

##number of federally qualified health centers per 10,000 people below poverty level
#calculates number of people in poverty
poppov <- (nc_countydata$popest2020*(nc_countydata$povrate/100))
#calculates number of federally qualified health centers per 10,000 people below poverty level
fpoppov <- ((nc_countydata$fqhc/poppov)*10000)
#creates new dataframe with nc_countydata and number of federally qualified health centers per 10,000 people below poverty level
nc_countydata2 <- cbind(nc_countydata,fpoppov)
colnames(nc_countydata2)[42] <- "fpoppov"
#creates new dataframe with nc_countydata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = fpoppov)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Federally Qualified Health Centers Per 10,000 People in Poverty", subtitle = "Fqhc, Popest2020 and Povrate Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.6), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nFQHCs") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,22))
#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = fpoppov)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Federally Qualified Health Centers Per 10,000 People in Poverty", subtitle = "Fqhc, Popest2020 and Povrate Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.6), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nFQHCs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,22), direction = 1) 

##number of federally qualified health centers per 10,000 people
#calculates number of federally qualified health centers per 10,000 people
fpop <- ((nc_countydata$fqhc/nc_countydata$popest2020)*10000)
#creates new dataframe with nc_countydata and number of federally qualified health centers per 10,000 people
nc_countydata2 <- cbind(nc_countydata,fpop)
colnames(nc_countydata2)[42] <- "fpop"
#creates new dataframe with nc_countydata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = fpop)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Federally Qualified Health Centers Per 10,000 People ", subtitle = "Fqhc and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.8), plot.subtitle = element_text(hjust = 0.6), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nFQHCs") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,4.5))
#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = fpop)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Federally Qualified Health Centers Per 10,000 People ", subtitle = "Fqhc and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.8), plot.subtitle = element_text(hjust = 0.6), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nFQHCs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,4.5), direction = 1) 

###critical access hospitals with multiple variables
##number of critical access hospitals per 10,000 people in rural counties
#selects for rural counties
ruralcounty <- nc_countydata[which(nc_countydata$ruralcont > 7),]
#calculates number of critical access hospitals per 10,000 people in rural counties
cahr <- ((ruralcounty$cah/ruralcounty$popest2020)*10000)
#creates new dataframe with ruralcounty and number of critical access hospitals per 10,000 people
ruralcounty2 <- cbind(ruralcounty,cahr)
colnames(ruralcounty2)[42] <- "cahr"
#creates new dataframe with ruralcounty2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, ruralcounty2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = cahr)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Critical Access Hospitals per 10,000 People By Rural County", subtitle = "Cah and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nCAHs") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,0.9))+
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = cahr)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Critical Access Hospitals per 10,000 People By Rural County", subtitle = "Cah and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nCAHs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,0.9), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black") 

###breastcancerscreen with multiple variables
##percent of hospitals with breast cancer screen
#calculates percent of hospitals with breast cancer screen
pscreen <- ((nc_countydata$breastcancerscreen/nc_countydata$totalhospital)*100)
#creates new dataframe with nc_countydata and percent of hospitals with screen
nc_countydata2 <- cbind(nc_countydata,pscreen)
colnames(nc_countydata2)[42] <- "pscreen"
#replaces all NAs with -1
nc_countydata2$pscreen[is.na(nc_countydata2$pscreen)] <- -1
#cuts percent of hospitals with screen variable into intervals and replace -1 with "no hospitals exist in county" in county
#change breaks and labels when you want different intervals or number of cuts
pscreena <- cut(x = nc_countydata2$pscreen, breaks= c(-1,-90,10,20,30,40,50,60,70,80,90,100),
                labels = c("No Hospitals\nExist in\nCounty","[0%,10%]","(10%,20%]","(20%,30%]","(30%,40%]","(40%,50%]","(50%,60%]","(60%,70%]","(70%,80%]","(80%,90%]","(90%,100%]"),
                include.lowest = FALSE, right= TRUE)
#creates new dataframe with nc_countydata2 and cut variable
nc_countydata3 <- cbind(nc_countydata2,pscreena)
colnames(nc_countydata3)[43] <- "pscreena"
#creates new dataframe with nc_countydata3 and coordinates of polygons of counties
mymapx <- merge(county_map2, nc_countydata3, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#creates color palette (change (11) when you change intervals or number of cuts)
pal <- colorRampPalette(c("white", "light blue", "blue"))(11)
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = pscreena)) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = pal, drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "% of\nHospitals") +
        labs(y = "", x = "", title = "Percent of Hospitals With Breast Cancer Screening By County", subtitle = "Breastcancerscreen and Totalhospital Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.56), legend.title= element_text(size=10), legend.justification = c("left","bottom"), axis.text = element_blank())

###plot choropleth map for belowpovertylvl with another variable

##percent of poor persons in deep poverty
#calculates percent of poor persons in deep poverty
pdeep <- ((nc_countydata$deeppov/nc_countydata$belowpovertylvl)*100)
#creates new dataframe with nc_countydata and percent of poor persons in deep poverty
nc_countydata2 <- cbind(nc_countydata,pdeep)
colnames(nc_countydata2)[42] <- "pdeep"
#creates new dataframe with nc_countydata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = pdeep)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Poor Persons in Deep Poverty Per County", subtitle = "Belowpovertylvl and Deeppov Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "% of\nPoor\nPersons") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,63))
#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = pdeep)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Poor Persons in Deep Poverty Per County", subtitle = "Belowpovertylvl and Deeppov Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "% of\nPoor\nPersons") +
        scale_fill_distiller(palette = "Blues", limits = c(0,63), direction = 1)

###plot choropleth map for beds with another variable

##number of beds per 10,000 people
#calculates number of hospital beds per 10,000 people
bedsp10 <- ((nc_countydata$beds/nc_countydata$popest2020)*10000)
#creates new dataframe with nc_countydata and number of beds per 10,000 people
nc_countydata2 <- cbind(nc_countydata,bedsp10)
colnames(nc_countydata2)[42] <- "bedsp10"
#creates new dataframe with nc_countydata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsp10)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospital Beds Per 10,000 People Per County", subtitle = "Beds and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospital\nBeds") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,89.27686))
#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsp10)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospital Beds Per 10,000 People Per County", subtitle = "Beds and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospital\nBeds") +
        scale_fill_distiller(palette = "Blues", limits = c(0,89.27686), direction = 1)


##number of beds per hospital
#calculates number of beds per hospital
bedsph <- (nc_countydata$beds/nc_countydata$totalhospital)
#creates new dataframe with nc_countydata and number of beds per hospital variable
nc_countydata2 <- cbind(nc_countydata,bedsph)
colnames(nc_countydata2)[42] <- "bedsph"
#replaces all NAs with -1
nc_countydata2$bedsph[is.na(nc_countydata2$bedsph)] <- -1
#cuts number of beds per hospital variable into intervals and replace -1 with "no hospitals" in county
#change breaks and labels when you want different intervals or number of cuts
bedsqa <- cut(x = nc_countydata2$bedsph, breaks= c(-1,0,100,200,300,400,500,600,700,800,900,1000),
              labels = c("No Hospitals","1-100", "100-200", "200-300", "300-400", "400-500","500-600","600-700","700-800","800-900","900-1000"),
              include.lowest = TRUE)
#creates new dataframe with nc_countydata2 and cut variable
nc_countydata3 <- cbind(nc_countydata2,bedsqa)
colnames(nc_countydata3)[43] <- "bedsqa"
#creates new dataframe with nc_countydata3 and coordinates of polygons of counties
mymapx <- merge(county_map2, nc_countydata3, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#creates color palette (change (11) when you change intervals or number of cuts)
pal <- colorRampPalette(c("white", "grey", "black"))(11)
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsqa)) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = pal, drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "# of\nBeds") +
        labs(y = "", x = "", title = "Number of Beds Per Hospital Per County", subtitle = "Beds and Totalhospital Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.56), legend.title= element_text(size=10), legend.justification = c("left","bottom"), axis.text = element_blank())


##number of beds per 10 square miles
#calculates number of hospital beds per 10 square miles
bedsmi <- ((nc_countydata$beds/nc_countydata$landarea)*10)
#creates new dataframe with nc_countydata and number of hospital beds per 10 square miles
nc_countydata2 <- cbind(nc_countydata,bedsmi)
colnames(nc_countydata2)[42] <- "bedsmi"
#creates new dataframe with nc_countydata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsmi)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospital Beds Per 10 Square Miles Per County", subtitle = "Beds and Landarea Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospital\nBeds") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,54.72411))
#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsmi)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospital Beds Per 10 Square Miles Per County", subtitle = "Beds and Landarea Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospital\nBeds") +
        scale_fill_distiller(palette = "Blues", limits = c(0,54.72411), direction = 1)


##plot choropleth map for oncology (number of hospitals with oncology services)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = oncology)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per County", subtitle = "Oncology Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,4))
#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = oncology)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per County", subtitle = "Oncology Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_distiller(palette = "Blues", limits = c(0,4), direction = 1)

##plot choropleth map for poploss (population loss typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(poploss))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Population Loss by County", subtitle = "Poploss Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.57), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "1990-2000 & 2000-2010") +
        scale_fill_manual(values = c("1" = "grey","0" = "white"), labels = c("Population Loss","No Population Loss"))

##plot choropleth map for perspoverty (persistent poverty typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(perspoverty))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Persistent Poverty Status by County", subtitle = "Perspoverty Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.57), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "Status") +
        scale_fill_manual(values = c("1" = "#ef3b2c","0" = "white"), labels = c("Persistent Poverty","Not Persistent Poverty"))

##plot choropleth map for nocare (access to healthcare)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(nocare))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Access to Healthcare by County", subtitle = "Nocare Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.57), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "Legend") +
        scale_fill_manual(values = c("1" = "grey","0" = "white"), labels = c("No Access","Access"))

##plot choropleth map for highpovstatus (high poverty typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(highpovstatus))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "High Poverty Status by County", subtitle = "Highpovstatus Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.57), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "Status") +
        scale_fill_manual(values = c("1" = "blue","0" = "white"), labels = c("High Poverty","Not High Poverty"))

##plot choropleth map for uninsured18to64 (% of 18-64 year olds without Health Insurance)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = uninsured18to64)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of 18 to 64-year-olds Without Health Insurance Per County", subtitle = "Uninsured18to64 Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nUninsured") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,27.2))
#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = uninsured18to64)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of 18 to 64-year-olds Without Health Insurance Per County", subtitle = "Uninsured18to64 Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nUninsured") +
        scale_fill_distiller(palette = "Blues", limits = c(0,27.2), direction = 1)

##plot choropleth map for unemploy (unemployment rate)

#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = unemploy)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Unemployment Rate", subtitle = "Unemploy Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "Unemployment\nRate") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,12))
#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = unemploy)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Unemployment Rate", subtitle = "Unemploy Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "Unemployment\nRate") +
        scale_fill_distiller(palette = "Blues", limits = c(0,12), direction = 1)

##plot choropleth map for totalhospital (# of hospitals)

#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = totalhospital)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals Per County", subtitle = "Totalhospital Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,10))
#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = totalhospital)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals Per County", subtitle = "Totalhospital Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_distiller(palette = "Blues", limits = c(0,10), direction = 1)

##plot choropleth map for  rhc (# of rural health clinics)

#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = rhc)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Rural Health Clinics Per County", subtitle = "Rhc Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nRHCs") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,7))
#white to blue 
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = rhc)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Rural Health Clinics Per County", subtitle = "Rhc Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nRHCs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,7), direction = 1)

##plot choropleth map for  povrate (poverty rate)

#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = povrate)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Persons in Poverty Per County", subtitle = "Povrate Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "% of\nPersons") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,31.5))
#white to blue 
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = povrate)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Persons in Poverty Per County", subtitle = "Povrate Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "% of\nPersons") +
        scale_fill_distiller(palette = "Blues", limits = c(0,31.5), direction = 1)

##plot choropleth map for percent hispanic (pcthisp)

#not classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pcthisp)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Population That Is Hispanic Per County", subtitle = "Pcthisp Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "%\nHispanic") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,20.6))

#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pcthisp)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Population That Is Hispanic Per County", subtitle = "Pcthisp Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nHispanic") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,20.6))

##plot choropleth map for percent black (pctblack)

#not classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pctblack)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Population That Is Black Per County", subtitle = "Pctblack Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "%\nBlack") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,62.5))

#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pctblack)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Population That Is Black Per County", subtitle = "Pctblack Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nBlack") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,62.5))

##plot choropleth map for percent american indian/alaska native (pctaian)

#not classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pctaian)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Population That Is American Indian/Alaska Native Per County", subtitle = "Pctaian Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "%\nAI/AN") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,38.4))

#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pctaian)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Population That Is American Indian/Alaska Native Per County", subtitle = "Pctaian Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nAI/AN") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,38.4))

##plot choropleth map for median age (medianage)

#not classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = medianage)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Median Age of Residents Per County", subtitle = "Medianage Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Median\nAge") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,49.6))

#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = medianage)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Median Age of Residents Per County", subtitle = "Medianage Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "Median\nAge") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,49.6))

##plot choropleth map for medhouseincome (median household income)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = medhouseincome)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Median Household Income Per County", subtitle = "Medhouseincome Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Median\nIncome") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,86138))

##plot choropleth map for lowemply (low employment typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(lowemply))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of 25 to 64-Year-Olds Employed by County (2008-12)", subtitle = "Lowemply Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent\nEmployed") +
        scale_fill_manual(values = c("1" = "#ef3b2c","0" = "white"), labels = c("<65%","≥65%"))

##plot choropleth map for loweduc (low education typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(loweduc))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of 25 to 64-Year-Olds Without High School Diploma by County", subtitle = "Loweduc Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent") +
        scale_fill_manual(values = c("1" = "#a50f15","0" = "white"), labels = c("≥20%","<20%"))

##plot choropleth map for fqhc (number of federally qualified health centers)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = fqhc)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Federally Qualified Health Centers Per County", subtitle = "Fqhc Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nCenters") +
        scale_fill_gradient(low = "white", high= "green")

##plot choropleth map for deeppov (% of persons in deep poverty)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = deeppov)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Persons in Deep Poverty Per County", subtitle = "Deeppov Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "% of\nPersons") +
        scale_fill_gradient(low = "white", high= "black", limits= c(0,15.8))


##plot choropleth map for cah
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = cah)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Critical Access Hospitals Per County", subtitle = "cah Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nCAHs") +
        scale_fill_gradient(low = "white", high= "green")


##plot choropleth map for % of below poverty level

#goes from lowest to highest poverty level in data, with custom (changeable) colors
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = belowpovertylvl)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent Below Poverty Level", subtitle = "Belowpovertylvl Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nResidents") +
        scale_fill_gradient(low = "white", high= "red")

#goes from 0 to highest poverty level in data, with custom (changeable) colors
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = belowpovertylvl)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent Below Poverty Level", subtitle = "Belowpovertylvl Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nResidents") +
        scale_fill_gradient(low = "white", high= "green", limits= c(0,27.7))

#goes from 0 to highest poverty level in data, with default colors
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = belowpovertylvl)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent Below Poverty Level", subtitle = "Belowpovertylvl Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nResidents") +
        scale_fill_gradient(limits= c(0,27.7))

##plot choropleth map for beds
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = beds)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospital Beds Per County", subtitle = "Beds Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nBeds") +
        scale_fill_gradient(low = "light blue", high= "blue")

##plot choropleth map for transportation
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = transport)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospitals Providing Transport to Health Services", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nHospitals")

#with color
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = transport)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospitals Providing Transport to Health Services", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "yellow", high= "red")
#low = "white", high= "black" looks good- distinct colors (but not very eye-catching)
#low = "yellow", high= "red" didn't look good (kinda ugly)
#geom_polygon(data = mymapx2[mymapx2$subregion == "alexander",], fill = "blue")

##calculate proportion of hospitals in county with transportation
nc_countydata2 <- cbind(nc_countydata, nc_countydata$transport/nc_countydata$totalhospital)
colnames(nc_countydata2)[42] <- "prcnthospital"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#plot choropleth map for proportion of hospitals in county with transportation
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = prcnthospital)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Proportion of Hospitals Providing Transport to Health Services", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.2), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Proportion of\nHospitals") +
        scale_fill_continuous(na.value="grey") 

#adds NAs to legend
ggplot() +
        geom_polygon(data= mymapx2, aes(x = long, y = lat, group = group, fill = prcnthospital, colour = "")) +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Proportion of Hospitals Providing Transport to Health Services", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.2), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Proportion of\nHospitals") +
        scale_fill_gradient(na.value= "grey", guide = guide_legend(order=1)) +
        scale_colour_manual(values=NA) +              
        guides(colour=guide_legend("NA", override.aes=list(colour="grey", fill="grey"), order=2))

##calculate percentage of hospitals in county with transportation
nc_countydata2 <- cbind(nc_countydata, (nc_countydata$transport/nc_countydata$totalhospital)*100)
colnames(nc_countydata2)[42] <- "prcnthospital"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapx <- merge(county_map2, nc_countydata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#plot choropleth map for percentage of hospitals in county with transportation
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = prcnthospital)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percentage of Hospitals Providing Transport to Health Services", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.2), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nHospitals") +
        scale_fill_continuous(na.value="grey")

##calculates # of hospitals in county with transportation per 10,000 people
nc_countydata3 <- cbind(nc_countydata, (nc_countydata$transport/nc_countydata$popest2020)*10000)
colnames(nc_countydata3)[42] <- "transportpppl"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapy <- merge(county_map2, nc_countydata3, by.x = "subregion", by.y= "countyname")
mymapy2 <- arrange(mymapy, group, order)

#plot choropleth map for # of hospitals in county with transportation per 10,000 people
ggplot(mymapy2, aes(x = long, y = lat, group = group, fill = transportpppl)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Hospitals with Transport per 10,000 People", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.2), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Number of\nHospitals") +
        scale_fill_continuous(na.value="grey")

##plot choropleth map for breastcancerscreen
library(ggplot2)
library(maps)
library(dplyr) 
nc_countydata$countyname <- tolower(nc_countydata$countyname)
county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymap <- merge(county_map2, nc_countydata, by.x = "subregion", by.y= "countyname")
mymap2 <- arrange(mymap, group, order)

ggplot(mymap2, aes(x = long, y = lat, group = group, fill = breastcancerscreen)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospitals with Breast Cancer Screen", subtitle = "Breastcancerscreen Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nHospitals")

#with color
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = breastcancerscreen)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospitals with Breast Cancer Screen", subtitle = "Breastcancerscreen Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink")
#low = "white", high= "black" looks good- distinct colors (but not very eye-catching)
#low = "yellow", high= "red" didn't look good (kinda ugly)
#geom_polygon(data = mymapx2[mymapx2$subregion == "alexander",], fill = "blue")

##plot for number of hospitals with breast cancer screen per 10,000 people
nc_countydata4 <- cbind(nc_countydata, (nc_countydata$breastcancerscreen/nc_countydata$popest2020)*10000)
colnames(nc_countydata4)[42] <- "breastcancerpop"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapc <- merge(county_map2, nc_countydata4, by.x = "subregion", by.y= "countyname")
mymap2c <- arrange(mymapc, group, order)

#plot choropleth map 
ggplot(mymap2c, aes(x = long, y = lat, group = group, fill = breastcancerpop)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Hospitals with Breast Cancer Screen per 10,000 People", subtitle = "Breastcancerscreen Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Number of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink", na.value="grey")

##plot for percentage of hospitals in county with breast cancer screen
nc_countydata5 <- cbind(nc_countydata, (nc_countydata$breastcancerscreen/nc_countydata$totalhospital)*100)
colnames(nc_countydata5)[42] <- "prcntscreen"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapd <- merge(county_map2, nc_countydata5, by.x = "subregion", by.y= "countyname")
mymapd2 <- arrange(mymapd, group, order)

#plot choropleth map
ggplot(mymapd2, aes(x = long, y = lat, group = group, fill = prcntscreen)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percentage of Hospitals with Breast Cancer Screen", subtitle = "Breastcancerscreen Variable") + 
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink", na.value="grey")


##plot for percentage of hospitals with oncology services that have breast cancer screen
nc_countydata6 <- cbind(nc_countydata, (nc_countydata$breastcancerscreen/nc_countydata$oncology)*100)
colnames(nc_countydata6)[42] <- "oncscreen"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymape <- merge(county_map2, nc_countydata6, by.x = "subregion", by.y= "countyname")
mymape2 <- arrange(mymape, group, order)

#plot choropleth map
ggplot(mymape2, aes(x = long, y = lat, group = group, fill = oncscreen)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "% of Hospitals with Oncology Services That Have Breast Cancer Screen", subtitle = "Breastcancerscreen and Oncology Variables") + 
        theme(plot.title = element_text(hjust = 0.4), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink", na.value="grey")

##plot for number of hospitals with breast cancer screen per 50 malignant neoplasms
nc_countydata7 <- cbind(nc_countydata, (nc_countydata$breastcancerscreen/nc_countydata$malignantneoplasm)*50)
colnames(nc_countydata7)[42] <- "screenneo"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapf <- merge(county_map2, nc_countydata7, by.x = "subregion", by.y= "countyname")
mymapf2 <- arrange(mymapf, group, order)

#plot choropleth map
ggplot(mymapf2, aes(x = long, y = lat, group = group, fill = screenneo)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "# of Hospitals with Breast Cancer Screen per 50 Malignant Neoplasms", subtitle = "Breastcancerscreen and Malignantneoplasm Variables") + 
        theme(plot.title = element_text(hjust = 0.4), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink", na.value="grey")



###plots for CBCS counties
cbcs_counties$countyname <- tolower(cbcs_counties$countyname)
county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymap <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymap2 <- arrange(mymap, group, order)

#rhc with multiple variables

##number of rhcs per 10,000 people in rural counties
#select data for rural counties
ruraldata <- cbcs_counties[(cbcs_counties$ruralcont > 7),]
#calculates number of rhcs per 10,000 people
rhcr <- ((ruraldata$rhc/ruraldata$popest2020)*10000)
#creates new dataframe with rhcr and ruraldata
ruraldata2 <- cbind(ruraldata,rhcr)
colnames(ruraldata2)[42] <- "rhcr"
#creates new dataframe with ruraldata2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, ruraldata2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = rhcr)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Rural Health Clinics Per 10,000 People in Rural Counties", subtitle = "Rhc, Popest2020 and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nRHCs") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,2.65))+
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = rhcr)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Rural Health Clinics Per 10,000 People in Rural Counties", subtitle = "Rhc, Popest2020 and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nRHCs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,2.65), direction = 1)  +
        borders(database = "state", regions = "north carolina", colour= "black")

###oncology with multiple variables

##number of hospitals with oncology services per 10,000 people 
#calculates number of hospitals with oncology services per 10,000 people
onc10 <- ((cbcs_counties$oncology/cbcs_counties$popest2020)*10000)
#creates new dataframe with nc_countydata and number of hospitals with oncology services per 10,000 people
cbcs_counties2 <- cbind(cbcs_counties,onc10)
colnames(cbcs_counties2)[42] <- "onc10"
#creates new dataframe with cbcs_counties2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = onc10)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per 10,000 People", subtitle = "Oncology and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,0.2)) +
        borders(database = "state", regions = "north carolina", colour= "black") 

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = onc10)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per 10,000 People", subtitle = "Oncology and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_distiller(palette = "Blues", limits = c(0,0.2), direction = 1)  +
        borders(database = "state", regions = "north carolina", colour= "black") 

##number of hospitals with oncology services per 10 square miles
#calculates number of hospitals with oncology services per 10 square miles
oncmi <- ((cbcs_counties$oncology/cbcs_counties$landarea)*10)
#creates new dataframe with nc_countydata and number of hospitals with oncology services per 10 square miles
cbcs_counties2 <- cbind(cbcs_counties,oncmi)
colnames(cbcs_counties2)[42] <- "oncmi"
#creates new dataframe with cbcs_counties2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = oncmi)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per 10 Square Miles", subtitle = "Oncology and Landarea Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,0.07)) +
        borders(database = "state", regions = "north carolina", colour= "black") 

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = oncmi)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per 10 Square Miles", subtitle = "Oncology and Landarea Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_distiller(palette = "Blues", limits = c(0,0.07), direction = 1)  +
        borders(database = "state", regions = "north carolina", colour= "black") 

##percent of hospitals that have oncology services
#calculates percent of hospitals with oncology services
oncp <- ((cbcs_counties$oncology/cbcs_counties$totalhospital)*100)
#creates new dataframe with cbcs_counties and percent of hospitals with oncology services
cbcs_counties2 <- cbind(cbcs_counties,oncp)
colnames(cbcs_counties2)[42] <- "oncp"
#replaces all NAs with -1
cbcs_counties2$oncp[is.na(cbcs_counties2$oncp)] <- -1
#cuts percent of hospitals with oncology variable into intervals and replace -1 with "no hospitals exist in county" in county
#change breaks and labels when you want different intervals or number of cuts
oncpa <- cut(x = cbcs_counties2$oncp, breaks= c(-1,-90,10,20,30,40,50,60,70,80,90,100),
             labels = c("No Hospitals\nExist in\nCounty","[0%,10%]","(10%,20%]","(20%,30%]","(30%,40%]","(40%,50%]","(50%,60%]","(60%,70%]","(70%,80%]","(80%,90%]","(90%,100%]"),
             include.lowest = FALSE, right= TRUE)
#creates new dataframe with cbcs_counties2 and cut variable
cbcs_counties3 <- cbind(cbcs_counties2,oncpa)
colnames(cbcs_counties3)[43] <- "oncpa"
#creates new dataframe with cbcs_counties3 and coordinates of polygons of counties
mymapx <- merge(county_map2, cbcs_counties3, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#creates color palette (change (11) when you change intervals or number of cuts)
pal <- colorRampPalette(c("white", "light blue", "blue"))(11)
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = oncpa)) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = pal, drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "% of\nHospitals") +
        labs(y = "", x = "", title = "Percent of Hospitals That Have Oncology Services By County", subtitle = "Oncology and Totalhospital Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","bottom"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 


###no care and multiple variables

##no care and persistent poverty
#adds empty column to cbcs_counties
cbcs_counties['new_col'] <- NA
#uses system below to label entries based on no care and persistent poverty
#blue = no care not persistent poverty (1)
#red = persistent poverty not no care (2)
#purple = no care and persistent poverty (3)
#white = not no care and not persistent poverty (4)
for(i in 1:44){
        if(cbcs_counties$nocare[i] == 1 & cbcs_counties$perspoverty[i] == 0){
                cbcs_counties[i,'new_col'] <- 1
        } else if(cbcs_counties$nocare[i] == 0 & cbcs_counties$perspoverty[i] == 1){
                cbcs_counties[i,'new_col'] <- 2
        } else if(cbcs_counties$nocare[i] == 1 & cbcs_counties$perspoverty[i] == 1){
                cbcs_counties[i,'new_col'] <- 3
        } else if(cbcs_counties$nocare[i] == 0 & cbcs_counties$perspoverty[i] == 0){
                cbcs_counties[i,'new_col'] <- 4
        }
}
#combines cbcs_counties with county_map2
mymapx <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("No Care, Not Persistent Poverty", "Not No Care, Persistent Poverty","No Care, Persistent Poverty","Not No Care, Not Persistent Poverty"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "No Care\n& Persistent\nPoverty") +
        labs(y = "", x = "", title = "No Care and Persistent Poverty By County", subtitle = "Nocare and Perspoverty Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.65), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 

##no care and rural counties
#adds empty column to cbcs_counties
cbcs_counties['new_col'] <- NA
#uses system below to label entries based on no care and rural counties
#blue = no care not rural county (1)
#red = rural county not no care (2)
#purple = no care and rural county (3)
#white = not no care and not rural county (4)
for(i in 1:44){
        if(cbcs_counties$nocare[i] == 1 & cbcs_counties$ruralcont[i] < 8){
                cbcs_counties[i,'new_col'] <- 1
        } else if(cbcs_counties$nocare[i] == 0 & cbcs_counties$ruralcont[i] > 7){
                cbcs_counties[i,'new_col'] <- 2
        } else if(cbcs_counties$nocare[i] == 1 & cbcs_counties$ruralcont[i] > 7){
                cbcs_counties[i,'new_col'] <- 3
        } else if(cbcs_counties$nocare[i] == 0 & cbcs_counties$ruralcont[i] < 8){
                cbcs_counties[i,'new_col'] <- 4
        }
}
#combines cbcs_counties with county_map2
mymapx <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("No Care, Not Rural County", "Not No Care, Rural County","No Care, Rural County","Not No Care, Not Rural County"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "No Care\n& Rural\nCounty") +
        labs(y = "", x = "", title = "No Care and Rural County Status By County", subtitle = "Nocare and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.65), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 

###Low employment and multiple variables

##low employment and persistent poverty
#adds empty column to cbcs_counties
cbcs_counties['new_col'] <- NA
#uses system below to label entries based on low employment and persistent poverty
#blue = low employment not persistent poverty (1)
#red = persistent poverty not low employment (2)
#purple = low employment and persistent poverty (3)
#white = not low employment and not persistent poverty (4)
for(i in 1:44){
        if(cbcs_counties$lowemply[i] == 1 & cbcs_counties$perspoverty[i] == 0){
                cbcs_counties[i,'new_col'] <- 1
        } else if(cbcs_counties$lowemply[i] == 0 & cbcs_counties$perspoverty[i] == 1){
                cbcs_counties[i,'new_col'] <- 2
        } else if(cbcs_counties$lowemply[i] == 1 & cbcs_counties$perspoverty[i] == 1){
                cbcs_counties[i,'new_col'] <- 3
        } else if(cbcs_counties$lowemply[i] == 0 & cbcs_counties$perspoverty[i] == 0){
                cbcs_counties[i,'new_col'] <- 4
        }
}
#combines cbcs_counties with county_map2
mymapx <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("Low Employment, Not Persistent Poverty", "Not Low Employment, Persistent Poverty","Low Employment, Persistent Poverty","Not Low Employment, Not Persistent Poverty"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "Low Employment\n& Persistent\nPoverty") +
        labs(y = "", x = "", title = "Low Employment and Persistent Poverty By County", subtitle = "Lowemply and Perspoverty Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.65), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 

###low education and multiple variables

##low education and low employment
#adds empty column to cbcs_counties
cbcs_counties['new_col'] <- NA
#uses system below to label entries based on low education and low employment
#blue = low education not low employment (1)
#red = low employment not low education (2)
#purple = low education and low employment (3)
#white = not low education and not low employment (4)
for(i in 1:44){
        if(cbcs_counties$loweduc[i] == 1 & cbcs_counties$lowemply[i] == 0){
                cbcs_counties[i,'new_col'] <- 1
        } else if(cbcs_counties$loweduc[i] == 0 & cbcs_counties$lowemply[i] == 1){
                cbcs_counties[i,'new_col'] <- 2
        } else if(cbcs_counties$loweduc[i] == 1 & cbcs_counties$lowemply[i] == 1){
                cbcs_counties[i,'new_col'] <- 3
        } else if(cbcs_counties$loweduc[i] == 0 & cbcs_counties$lowemply[i] == 0){
                cbcs_counties[i,'new_col'] <- 4
        }
}
#combines cbcs_counties with county_map2
mymapx <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("Low Education, Not Low Employment", "Not Low Education, Low Employment","Low Education, Low Employment","Not Low Education, Not Low Employment"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "Low Education\n& Low\nEmployment") +
        labs(y = "", x = "", title = "Low Education and Low Employment By County", subtitle = "Loweduc and Lowemply Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 


##low education and persistent poverty
#adds empty column to cbcs_counties
cbcs_counties['new_col'] <- NA
#uses system below to label entries based on low education and persistent poverty
#blue = low education not persistent poverty (1)
#red = persistent poverty not low education (2)
#purple = low education and persistent poverty (3)
#white = not low education and not persistent poverty (4)
for(i in 1:44){
        if(cbcs_counties$loweduc[i] == 1 & cbcs_counties$perspoverty[i] == 0){
                cbcs_counties[i,'new_col'] <- 1
        } else if(cbcs_counties$loweduc[i] == 0 & cbcs_counties$perspoverty[i] == 1){
                cbcs_counties[i,'new_col'] <- 2
        } else if(cbcs_counties$loweduc[i] == 1 & cbcs_counties$perspoverty[i] == 1){
                cbcs_counties[i,'new_col'] <- 3
        } else if(cbcs_counties$loweduc[i] == 0 & cbcs_counties$perspoverty[i] == 0){
                cbcs_counties[i,'new_col'] <- 4
        }
}
#combines cbcs_counties with county_map2
mymapx <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("Low Education, Not Persistent Poverty", "Not Low Education, Persistent Poverty","Low Education, Persistent Poverty","Not Low Education, Not Persistent Poverty"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "Low Education\n& Persistent\nPoverty") +
        labs(y = "", x = "", title = "Low Education and Persistent Poverty By County", subtitle = "Loweduc and Perspoverty Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.65), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 


###high poverty status (highpovstatus) with multiple variables

##high poverty status only shown for rural counties
#select data for rural counties
ruraldata <- cbcs_counties[(cbcs_counties$ruralcont > 7),]
#combine ruraldata and county_map2
mymapx <- merge(county_map2, ruraldata, by.x = "subregion", by.y= "countyname")
mymap2x <- arrange(mymapx, group, order)
#plot
ggplot(mymap2x, aes(x = long, y = lat, group = group, fill = as.factor(highpovstatus))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "High Poverty Status by Rural County", subtitle = "Highpovstatus Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.55), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "Legend") +
        scale_fill_manual(values = c("1" = "#ef3b2c","0" = "white"), labels = c("High Poverty","Not High Poverty")) +
        borders(database = "state", regions = "north carolina", colour= "black") 

##high poverty status and low education 
#adds empty column to cbcs_counties
cbcs_counties['new_col'] <- NA
#uses system below to label entries based on high pov status and low education
#blue = high poverty status not low education (1)
#red = low education not high poverty status (2)
#purple = high poverty status and low education (3)
#white = not high poverty status and not low education (4)
for(i in 1:44){
        if(cbcs_counties$highpovstatus[i] == 1 & cbcs_counties$loweduc[i] == 0){
                cbcs_counties[i,'new_col'] <- 1
        } else if(cbcs_counties$highpovstatus[i] == 0 & cbcs_counties$loweduc[i] == 1){
                cbcs_counties[i,'new_col'] <- 2
        } else if(cbcs_counties$highpovstatus[i] == 1 & cbcs_counties$loweduc[i] == 1){
                cbcs_counties[i,'new_col'] <- 3
        } else if(cbcs_counties$highpovstatus[i] == 0 & cbcs_counties$loweduc[i] == 0){
                cbcs_counties[i,'new_col'] <- 4
        }
}
#combines cbcs_counties with county_map2
mymapx <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("High Poverty, Not Low Education", "Not High Poverty, Low Education","High Poverty, Low Education","Not High Poverty, Not Low Education"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "High Poverty\nStatus &\nLow Education") +
        labs(y = "", x = "", title = "High Poverty Status and Low Education By County", subtitle = "Highpovstatus and Loweduc Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 

##high poverty status and low employment 
#adds empty column to cbcs_counties
cbcs_counties['new_col'] <- NA
#uses system below to label entries based on high pov status and low employment
#blue = high poverty status not low employment (1)
#red = low employment not high poverty status (2)
#purple = high poverty status and low employment (3)
#white = not high poverty status and not low employment (4)
for(i in 1:44){
        if(cbcs_counties$highpovstatus[i] == 1 & cbcs_counties$lowemply[i] == 0){
                cbcs_counties[i,'new_col'] <- 1
        } else if(cbcs_counties$highpovstatus[i] == 0 & cbcs_counties$lowemply[i] == 1){
                cbcs_counties[i,'new_col'] <- 2
        } else if(cbcs_counties$highpovstatus[i] == 1 & cbcs_counties$lowemply[i] == 1){
                cbcs_counties[i,'new_col'] <- 3
        } else if(cbcs_counties$highpovstatus[i] == 0 & cbcs_counties$lowemply[i] == 0){
                cbcs_counties[i,'new_col'] <- 4
        }
}
#combines cbcs_counties with county_map2
mymapx <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("High Poverty, Not Low Employment", "Not High Poverty, Low Employment","High Poverty, Low Employment","Not High Poverty, Not Low Employment"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "High Poverty\nStatus &\nLow Employment") +
        labs(y = "", x = "", title = "High Poverty Status and Low Employment By County", subtitle = "Highpovstatus and Lowemply Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.45), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 


##high poverty status and no care 
#adds empty column to cbcs_counties
cbcs_counties['new_col'] <- NA
#uses system below to label entries based on high pov status and no care
#blue = high poverty status not no care (1)
#red = no care not high poverty status  (2)
#purple = high poverty status and no care (3)
#white = not high poverty status and not no care (4)
for(i in 1:44){
        if(cbcs_counties$highpovstatus[i] == 1 & cbcs_counties$nocare[i] == 0){
                cbcs_counties[i,'new_col'] <- 1
        } else if(cbcs_counties$highpovstatus[i] == 0 & cbcs_counties$nocare[i] == 1){
                cbcs_counties[i,'new_col'] <- 2
        } else if(cbcs_counties$highpovstatus[i] == 1 & cbcs_counties$nocare[i] == 1){
                cbcs_counties[i,'new_col'] <- 3
        } else if(cbcs_counties$highpovstatus[i] == 0 & cbcs_counties$nocare[i] == 0){
                cbcs_counties[i,'new_col'] <- 4
        }
}
#combines cbcs_counties with county_map2
mymapx <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#assigns colors to numbers
mypal <- c("1" = "blue", "2" = "red", "3" = "purple", "4" = "white")
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = as.factor(new_col))) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = mypal, labels = c("High Poverty, Not No Care", "Not High Poverty, No Care","High Poverty, No Care","Not High Poverty, Not No Care"), drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "High Poverty\nStatus &\nNo Care") +
        labs(y = "", x = "", title = "High Poverty Status and No Care By County", subtitle = "Highpovstatus and Nocare Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black") 

###federally qualified health centers with multiple variables

##number of federally qualified health centers per 10,000 people below poverty level
#calculates number of people in poverty
poppov <- (cbcs_counties$popest2020*(cbcs_counties$povrate/100))
#calculates number of federally qualified health centers per 10,000 people below poverty level
fpoppov <- ((cbcs_counties$fqhc/poppov)*10000)
#creates new dataframe with cbcs_counties and number of federally qualified health centers per 10,000 people below poverty level
cbcs_counties2 <- cbind(cbcs_counties,fpoppov)
colnames(cbcs_counties2)[42] <- "fpoppov"
#creates new dataframe with cbcs_counties2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = fpoppov)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Federally Qualified Health Centers Per 10,000 People in Poverty", subtitle = "Fqhc, Popest2020 and Povrate Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.6), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nFQHCs") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,12.5))+
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = fpoppov)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Federally Qualified Health Centers Per 10,000 People in Poverty", subtitle = "Fqhc, Popest2020 and Povrate Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.6), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nFQHCs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,12.5), direction = 1)  +
        borders(database = "state", regions = "north carolina", colour= "black")


##number of federally qualified health centers per 10,000 people
#calculates number of federally qualified health centers per 10,000 people
fpop <- ((cbcs_counties$fqhc/cbcs_counties$popest2020)*10000)
#creates new dataframe with cbcs_counties and number of federally qualified health centers per 10,000 people
cbcs_counties2 <- cbind(cbcs_counties,fpop)
colnames(cbcs_counties2)[42] <- "fpop"
#creates new dataframe with cbcs_counties2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = fpop)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Federally Qualified Health Centers Per 10,000 People ", subtitle = "Fqhc and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.8), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nFQHCs") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,2.4)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = fpop)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Federally Qualified Health Centers Per 10,000 People ", subtitle = "Fqhc and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.8), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nFQHCs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,2.4), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")


###critical access hospitals with multiple variables
##number of critical access hospitals per 10,000 people in rural counties
#selects for rural counties
ruralcounty <- cbcs_counties[which(cbcs_counties$ruralcont > 7),]
#calculates number of critical access hospitals per 10,000 people in rural counties
cahr <- ((ruralcounty$cah/ruralcounty$popest2020)*10000)
#creates new dataframe with ruralcounty and number of critical access hospitals per 10,000 people
ruralcounty2 <- cbind(ruralcounty,cahr)
colnames(ruralcounty2)[42] <- "cahr"
#creates new dataframe with ruralcounty2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, ruralcounty2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = cahr)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Critical Access Hospitals per 10,000 People By Rural County", subtitle = "Cah and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nCAHs") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,0.9))+
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = cahr)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Critical Access Hospitals per 10,000 People By Rural County", subtitle = "Cah and Ruralcont Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nCAHs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,0.9), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")

###breastcancerscreen with multiple variables
##percent of hospitals with breast cancer screen
#calculates percent of hospitals with breast cancer screen
pscreen <- ((cbcs_counties$breastcancerscreen/cbcs_counties$totalhospital)*100)
#creates new dataframe with cbcs_counties and percent of hospitals with screen
cbcs_counties2 <- cbind(cbcs_counties,pscreen)
colnames(cbcs_counties2)[42] <- "pscreen"
#replaces all NAs with -1
cbcs_counties2$pscreen[is.na(cbcs_counties2$pscreen)] <- -1
#cuts percent of hospitals with screen variable into intervals and replace -1 with "no hospitals exist in county" in county
#change breaks and labels when you want different intervals or number of cuts
pscreena <- cut(x = cbcs_counties2$pscreen, breaks= c(-1,-90,10,20,30,40,50,60,70,80,90,100),
                labels = c("No Hospitals\nExist in\nCounty","[0%,10%]","(10%,20%]","(20%,30%]","(30%,40%]","(40%,50%]","(50%,60%]","(60%,70%]","(70%,80%]","(80%,90%]","(90%,100%]"),
                include.lowest = FALSE, right= TRUE)
#creates new dataframe with cbcs_counties2 and cut variable
cbcs_counties3 <- cbind(cbcs_counties2,pscreena)
colnames(cbcs_counties3)[43] <- "pscreena"
#creates new dataframe with cbcs_counties3 and coordinates of polygons of counties
mymapx <- merge(county_map2, cbcs_counties3, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#creates color palette (change (11) when you change intervals or number of cuts)
pal <- colorRampPalette(c("white", "pink", "hot pink"))(11)
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = pscreena)) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = pal, drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "% of\nHospitals") +
        labs(y = "", x = "", title = "Percent of Hospitals With Breast Cancer Screening By County", subtitle = "Breastcancerscreen and Totalhospital Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.56), legend.title= element_text(size=10), legend.justification = c("left","bottom"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot for number of hospitals with breast cancer screen per 10,000 people
cbcs_counties2 <- cbind(cbcs_counties, (cbcs_counties$breastcancerscreen/cbcs_counties$popest2020)*10000)
colnames(cbcs_counties2)[42] <- "breastcancerpop"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapc <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymap2c <- arrange(mymapc, group, order)

#plot choropleth map 
ggplot(mymap2c, aes(x = long, y = lat, group = group, fill = breastcancerpop)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Hospitals With Breast Cancer Screening Per 10,000 People By County", subtitle = "Breastcancerscreen and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink", limits = c(0,0.54)) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot for number of hospitals with breast cancer screen per 50 malignant neoplasms

cbcs_counties2 <- cbind(cbcs_counties, (cbcs_counties$breastcancerscreen/cbcs_counties$malignantneoplasm)*50)
colnames(cbcs_counties2)[42] <- "screenneo"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapf <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapf2 <- arrange(mymapf, group, order)

#plot choropleth map
ggplot(mymapf2, aes(x = long, y = lat, group = group, fill = screenneo)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Hospitals With Breast Cancer Screening Per 50 Malignant Neoplasms", subtitle = "Breastcancerscreen and Malignantneoplasm Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink", limits = c(0,0.9)) +
        borders(database = "state", regions = "north carolina", colour= "black")



###plot choropleth map for belowpovertylvl with another variable

##percent of poor persons in deep poverty
#calculates percent of poor persons in deep poverty
pdeep <- ((cbcs_counties$deeppov/cbcs_counties$belowpovertylvl)*100)
#creates new dataframe with cbcs_counties and percent of poor persons in deep poverty
cbcs_counties2 <- cbind(cbcs_counties,pdeep)
colnames(cbcs_counties2)[42] <- "pdeep"
#creates new dataframe with cbcs_counties2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = pdeep)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Poor Persons in Deep Poverty Per County", subtitle = "Belowpovertylvl and Deeppov Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "% of\nPoor\nPersons") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,57.38397)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = pdeep)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Poor Persons in Deep Poverty Per County", subtitle = "Belowpovertylvl and Deeppov Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "% of\nPoor\nPersons") +
        scale_fill_distiller(palette = "Blues", limits = c(0,57.38397), direction = 1)+
        borders(database = "state", regions = "north carolina", colour= "black")

###plot choropleth map for beds with another variable
##number of beds per 10,000 people
#calculates number of hospital beds per 10,000 people
bedsp10 <- ((cbcs_counties$beds/cbcs_counties$popest2020)*10000)
#creates new dataframe with cbcs_counties and number of beds per 10,000 people
cbcs_counties2 <- cbind(cbcs_counties,bedsp10)
colnames(cbcs_counties2)[42] <- "bedsp10"
#creates new dataframe with cbcs_counties2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsp10)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospital Beds Per 10,000 People Per County", subtitle = "Beds and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospital\nBeds") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,89.27686)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsp10)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospital Beds Per 10,000 People Per County", subtitle = "Beds and Popest2020 Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospital\nBeds") +
        scale_fill_distiller(palette = "Blues", limits = c(0,89.27686), direction = 1)+
        borders(database = "state", regions = "north carolina", colour= "black")


##number of beds per hospital
#calculates number of beds per hospital
bedsph <- (cbcs_counties$beds/cbcs_counties$totalhospital)
#creates new dataframe with cbcs_counties and number of beds per hospital variable
cbcs_counties2 <- cbind(cbcs_counties,bedsph)
colnames(cbcs_counties2)[42] <- "bedsph"
#replaces all NAs with -1
cbcs_counties2$bedsph[is.na(cbcs_counties2$bedsph)] <- -1
#cuts number of beds per hospital variable into intervals and replace -1 with "no hospitals" in county
#change breaks and labels when you want different intervals or number of cuts
bedsqa <- cut(x = cbcs_counties2$bedsph, breaks= c(-1,0,100,200,300,400,500,600,700,800,900,1000),
              labels = c("No Hospitals","1-100", "100-200", "200-300", "300-400", "400-500","500-600","600-700","700-800","800-900","900-1000"),
              include.lowest = TRUE)
#creates new dataframe with cbcs_counties2 and cut variable
cbcs_counties3 <- cbind(cbcs_counties2,bedsqa)
colnames(cbcs_counties3)[43] <- "bedsqa"
#creates new dataframe with nc_countydata3 and coordinates of polygons of counties
mymapx <- merge(county_map2, cbcs_counties3, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)
#creates color palette (change (11) when you change intervals or number of cuts)
pal <- colorRampPalette(c("white", "light blue", "blue"))(11)
#plots map
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsqa)) +
        geom_polygon(colour = "black") +
        scale_fill_manual(values = pal, drop= FALSE) +
        expand_limits(x = county_map2$long, y = county_map2$lat) +
        coord_map("polyconic") +
        labs(fill = "# of\nBeds") +
        labs(y = "", x = "", title = "Number of Beds Per Hospital Per County", subtitle = "Beds and Totalhospital Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.56), legend.title= element_text(size=10), legend.justification = c("left","bottom"), axis.text = element_blank()) +
        borders(database = "state", regions = "north carolina", colour= "black")

##number of beds per 10 square miles
#calculates number of hospital beds per 10 square miles
bedsmi <- ((cbcs_counties$beds/cbcs_counties$landarea)*10)
#creates new dataframe with cbcs_counties and number of hospital beds per 10 square miles
cbcs_counties2 <- cbind(cbcs_counties,bedsmi)
colnames(cbcs_counties2)[42] <- "bedsmi"
#creates new dataframe with cbcs_counties2 and county_map2 (coordinates of polygons of counties)
mymapx <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapx2 <- arrange(mymapx, group, order)

#classic theme
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsmi)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospital Beds Per 10 Square Miles Per County", subtitle = "Beds and Landarea Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospital\nBeds") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,54.72411)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymapx2, aes(x = long, y = lat, group = group, fill = bedsmi)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospital Beds Per 10 Square Miles Per County", subtitle = "Beds and Landarea Variables") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospital\nBeds") +
        scale_fill_distiller(palette = "Blues", limits = c(0,54.72411), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for oncology (number of hospitals with oncology services)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = oncology)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per County", subtitle = "Oncology Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,4)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = oncology)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals With Oncology Services Per County", subtitle = "Oncology Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_distiller(palette = "Blues", limits = c(0,4), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for poploss (population loss typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(poploss))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Population Loss by County", subtitle = "Poploss Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.57), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "1990-2000 & 2000-2010") +
        scale_fill_manual(values = c("1" = "black","0" = "white"), labels = c("Population Loss","No Population Loss")) +
        borders(database = "state", regions = "north carolina", colour= "black")


##plot choropleth map for perspoverty (persistent poverty typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(perspoverty))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Persistent Poverty Status by County", subtitle = "Perspoverty Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.55), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "Status") +
        scale_fill_manual(values = c("1" = "red","0" = "white"), labels = c("Persistent Poverty","Not Persistent Poverty")) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for nocare (lack of healthcare access typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(nocare))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Access to Healthcare by County", subtitle = "Nocare Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.6), plot.subtitle = element_text(hjust = 0.57), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "Legend") +
        scale_fill_manual(values = c("1" = "grey","0" = "white"), labels = c("No Access","Access")) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for highpovstatus (high poverty status typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(highpovstatus))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "High Poverty Status by County", subtitle = "Highpovstatus Variable") + 
        theme_classic(base_line_size = 0) +
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +        
        labs(fill = "Status") +
        scale_fill_manual(values = c("1" = "grey","0" = "white"), labels = c("High Poverty","Not High Poverty")) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for uninsured18to64 (% of 18-64 year olds without Health Insurance)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = uninsured18to64)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of 18 to 64-year-olds Without Health Insurance Per County", subtitle = "Uninsured18to64 Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nUninsured") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,27.2)) +
        borders(database = "state", regions = "north carolina", colour= "black")
#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = uninsured18to64)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of 18 to 64-year-olds Without Health Insurance Per County", subtitle = "Uninsured18to64 Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nUninsured") +
        scale_fill_distiller(palette = "Blues", limits = c(0,27.2), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for unemploy (unemployment rate in each county)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = unemploy)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Unemployment Rate", subtitle = "Unemploy Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "Unemployment\nRate") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,10.6)) +
        borders(database = "state", regions = "north carolina", colour= "black")
#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = unemploy)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Unemployment Rate", subtitle = "Unemploy Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "Unemployment\nRate") +
        scale_fill_distiller(palette = "Blues", limits = c(0,10.6), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for total hospital (# of hospitals in county)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = totalhospital)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals Per County", subtitle = "Totalhospital Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,10)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = totalhospital)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Hospitals Per County", subtitle = "Totalhospital Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nHospitals") +
        scale_fill_distiller(palette = "Blues", limits = c(0,10), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for rhc (# of rural health clinics)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = rhc)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Rural Health Clinics Per County", subtitle = "Rhc Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nRHCs") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,7)) +
        borders(database = "state", regions = "north carolina", colour= "black")
#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = rhc)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Number of Rural Health Clinics Per County", subtitle = "Rhc Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "# of\nRHCs") +
        scale_fill_distiller(palette = "Blues", limits = c(0,7), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for povrate (poverty rate)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = povrate)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Persons in Poverty Per County", subtitle = "Povrate Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "% of\nPersons") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,25.4)) +
        borders(database = "state", regions = "north carolina", colour= "black")
#white to blue
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = povrate)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Persons in Poverty Per County", subtitle = "Povrate Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "% of\nPersons") +
        scale_fill_distiller(palette = "Blues", limits = c(0,25.4), direction = 1) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for percent hispanic (hispanic)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pcthisp)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Population That Is Hispanic Per County", subtitle = "Pcthisp Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nHispanic") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,20.6)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#not classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pcthisp)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Population That Is Hispanic Per County", subtitle = "Pcthisp Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "%\nHispanic") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,20.6)) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for percent black (pctblack)
#classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pctblack)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Population That Is Black Per County", subtitle = "Pctblack Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nBlack") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,62.5)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#not classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pctblack)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Population That Is Black Per County", subtitle = "Pctblack Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "%\nBlack") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,62.5)) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for pctaian (percent native american/alaska native)
#classic theme pctaian plot (white background, no gridlines, no axes)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pctaian)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Percent of Population That Is American Indian/Alaska Native Per County", subtitle = "Pctaian Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "%\nAI/AN") +
        scale_fill_gradient(low = "white", high= "black", limits = c(0,3.2)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#standard pctaian plot
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = pctaian)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Population That Is American Indian/Alaska Native Per County", subtitle = "Pctaian Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "%\nAI/AN") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,3.2)) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for medianage (median age of residents)
#classic theme median age plot (white background, no gridlines, no axes)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = medianage)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Median Age of Residents Per County", subtitle = "Medianage Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "Median\nAge") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,48.3)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#adjusted titles for classic theme
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = medianage)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "", x = "", title = "Median Age of Residents Per County", subtitle = "Medianage Variable") + 
        theme_classic(base_line_size = 0)+
        theme(plot.title = element_text(hjust = 0.7), plot.subtitle = element_text(hjust = 0.6), legend.title= element_text(size=10), legend.justification = c("left","top"), axis.text = element_blank()) +
        labs(fill = "Median\nAge") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,48.3)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#standard median age plot
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = medianage)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Median Age of Residents Per County", subtitle = "Medianage Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Median\nAge") +
        scale_fill_gradient(limits = c(0,48.3)) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for medhouseincome (median household income)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = medhouseincome)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Median Household Income Per County", subtitle = "Medhouseincome Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Median\nIncome") +
        scale_fill_gradient(low = "white", high= "red", limits = c(0,84377)) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for lowemply (low employment typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(lowemply))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of 25 to 64-Year-Olds Employed by County (2008-12)", subtitle = "Lowemply Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent\n Employed") +
        scale_fill_manual(values = c("1" = "black","0" = "white"), labels = c("<65%","≥65%")) +
        borders(database = "state", regions = "north carolina", colour= "black")

##plot choropleth map for loweduc (low education typology code)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = as.factor(loweduc))) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of 25 to 64-Year-Olds Without High School Diploma by County", subtitle = "Loweduc Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent") +
        scale_fill_manual(values = c("1" = "red","0" = "white"), labels = c("≥20%","<20%")) + 
        borders(database = "state", regions = "north carolina", colour= "black")

#plot choropleth map for fqhc (number of federally qualified health centers)
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = fqhc)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Federally Qualified Health Centers Per County", subtitle = "Fqhc Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nCenters") +
        scale_fill_gradient(low = "white", high= "green") +
        borders(database = "state", regions = "north carolina", colour= "black")
#remove scale_fill_gradient line if you want default coloring (light blue to dark blue)

#plot choropleth map for deep poverty
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = deeppov)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent of Persons in Deep Poverty Per County", subtitle = "Deeppov Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "% of\nPersons") +
        scale_fill_gradient(low = "white", high= "green", limits = c(0,13.6)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#plot choropleth map for CAHs
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = cah)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Critical Access Hospitals Per County", subtitle = "cah Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nCAHs") +
        scale_fill_gradient(low = "white", high= "black") +
        borders(database = "state", regions = "north carolina", colour= "black")

#plot choropleth map for below poverty level
#custom coloring going from 0 to highest poverty level
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = belowpovertylvl)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent Below Poverty level", subtitle = "Belowpovertylvl Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nResidents") +
        scale_fill_gradient(low = "white", high= "green", limits= c(0,24.5)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#default coloring going from 0 to highest poverty level
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = belowpovertylvl)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percent Below Poverty level", subtitle = "Belowpovertylvl Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nResidents") +
        scale_fill_gradient(limits= c(0,24.5)) +
        borders(database = "state", regions = "north carolina", colour= "black")

#plot choropleth map for beds
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = beds)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospital Beds Per County", subtitle = "Beds Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nBeds") +
        scale_fill_gradient(low = "white", high= "red") +
        borders(database = "state", regions = "north carolina", colour= "black")

#plot choropleth map for transportation
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = transport)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospitals Providing Transport to Health Services", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nHospitals") 
#add + borders(database = "county", regions = "north carolina", colour= "black")
#if want outline of ALL NC counties added (counties outside CBCS aren't filled in)

#calculate proportion of hospitals in county with transportation
cbcs_counties2 <- cbind(cbcs_counties, cbcs_counties$transport/cbcs_counties$totalhospital)
colnames(cbcs_counties2)[42] <- "prcnthospital"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapz <- merge(county_map2, cbcs_counties2, by.x = "subregion", by.y= "countyname")
mymapz2 <- arrange(mymapz, group, order)

#plot choropleth map for proportion of hospitals in county with transportation
ggplot(mymapz2, aes(x = long, y = lat, group = group, fill = prcnthospital)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Proportion of Hospitals Providing Transport to Health Services", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.2), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Proportion of\nHospitals") +
        scale_fill_continuous(na.value="grey")


##calculate percentage of hospitals in county with transportation
cbcs_counties3 <- cbind(cbcs_counties, (cbcs_counties$transport/cbcs_counties$totalhospital)*100)
colnames(cbcs_counties3)[42] <- "prcnthospital"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapa <- merge(county_map2, cbcs_counties3, by.x = "subregion", by.y= "countyname")
mymapa2 <- arrange(mymapa, group, order)

#plot choropleth map for percentage of hospitals in county with transportation
ggplot(mymapa2, aes(x = long, y = lat, group = group, fill = prcnthospital)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Percentage of Hospitals Providing Transport to Health Services", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Percent of\nHospitals") +
        scale_fill_continuous(na.value="grey")

##calculates # of hospitals in county with transportation per 10,000 people
cbcs_counties4 <- cbind(cbcs_counties, (cbcs_counties$transport/cbcs_counties$popest2020)*10000)
colnames(cbcs_counties4)[42] <- "transportpppl"

county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymapb <- merge(county_map2, cbcs_counties4, by.x = "subregion", by.y= "countyname")
mymapb2 <- arrange(mymapb, group, order)

#plot choropleth map for # of hospitals in county with transportation per 10,000 people
ggplot(mymapb2, aes(x = long, y = lat, group = group, fill = transportpppl)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospitals with Transport per 10,000 People", subtitle = "Transport Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "Number of\nHospitals") +
        scale_fill_continuous(na.value="grey")

#plot choropleth map for breast cancer screening
cbcs_counties$countyname <- tolower(cbcs_counties$countyname)
county_map <- map_data("county")
county_map2 <- county_map[which(county_map$region == "north carolina"),]
mymap <- merge(county_map2, cbcs_counties, by.x = "subregion", by.y= "countyname")
mymap2 <- arrange(mymap, group, order)

#plot choropleth map
ggplot(mymap2, aes(x = long, y = lat, group = group, fill = breastcancerscreen)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospitals Providing Breast Cancer Screening", subtitle = "Breastcancerscreen Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink", na.value="grey") +
        borders(database = "county", regions = "north carolina", colour= "black")

ggplot(mymap2, aes(x = long, y = lat, group = group, fill = breastcancerscreen)) +
        geom_polygon(colour = "black") +
        coord_map("polyconic") +
        labs(y = "Latitude", x = "Longitude", title = "Number of Hospitals Providing Breast Cancer Screening", subtitle = "Breastcancerscreen Variable") + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title= element_text(size=10)) +
        labs(fill = "# of\nHospitals") +
        scale_fill_gradient(low = "white", high= "hot pink", na.value="grey") +
        borders(database = "state", regions = "north carolina", colour= "black")

