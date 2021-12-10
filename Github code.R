#### All the code for our analysing  the weather on Mars group project ####

####Note that each coder has different data frame and data sets names####

####Further note that data was tidied up in excel to save time ####

#### (e.g. removed any unnecessary columns, changed the format from csv to Excel workbook to make our code run with the packages and correct date formats) ####

#### library(tidyverse) ####

#### library(openair) ####

#### library(MLMETRICS) ####

#### Temperature Lag Cross Correlation Graph ####

acf(LAG3TEMP$HOURLYTEMP, lag.max = 100,  main ="Lag Correlation Graph of Temperature for 3 consecutive sols", sub ="Hourly lags of Sol 478,479,480")

#### Temperature Lag Cross Correlation Graph ####

acf(LAG3PRESSURE$HOURLYPRESSURE, lag.max = 100,  main ="Lag Correlation Graph of Pressure for 3 consecutive sols", sub ="Hourly lags of Sol 478,479,480")

#### Diurnal temperature across seasons ####

ggplot()+
  geom_line(data=T113, aes(x=LMST, y=BMY_AIR_TEMP, colour=SOL), size=0.8)+
  geom_line(data=T307, aes(x=LMST, y=BMY_AIR_TEMP, colour=SOL), size=0.8)+
  geom_line(data=T485, aes(x=LMST, y=BMY_AIR_TEMP, colour=SOL), size=0.8)+
  geom_line(data=T634, aes(x=LMST, y=BMY_AIR_TEMP, colour=SOL), size=0.8)+
  theme_bw()+
  scale_x_datetime(date_labels ="%H:%M", date_breaks ="2 hour")+
  scale_colour_manual(values=c("lightgreen", "violet", "darkorange", "steelblue1"))+
  labs(title="Daily temperature variation across seasons ",
       subtitle ="Sp.E=Spring Equinox, Su.S=Summer Solstice,  Au.E=Autumn Equinox,  Wi.S=Winter Solstice")+
  theme(plot.title = element_text(size=18, face="bold"), plot.subtitle = element_text(size=12))+
  xlab("Local Mean Solar Time") + ylab("Temperature (K)")

#### Defined Values for Solar radiation Plots ####

e <- 0.0935
A <- 0.17
L <- (0:360)
S <- 590.8*((1+e*cos((L-251)*pi/180))^2/(1-e^2)^2)
q <- 5.67*(10^(-8))
df <- data.frame(L=L, S=S)
temp <-((S*(0.83))/(4*q))^0.25
tempdf <- data.frame(L=L, temp=temp)

#### Solar radiation as Mars Orbits the Sun ####

ggplot(data=df, aes(x=L, y=S)) +
    geom_line(size = 1.25) +
   labs(title="Solar radiation as Mars orbits The Sun",x="Solar Longitude (degrees)", y = "Solar Radiation (Wm-2)") +
  scale_x_continuous(breaks = seq(0, 360, by = 45)) +
   scale_y_continuous(breaks = seq(450, 750, by = 50)) + expand_limits(y=450) +
   theme_bw() +
   geom_segment(aes(x = 71, y = -Inf, xend = 71, yend = 494.0864), linetype="dashed") +
   geom_segment(aes(x = 251, y = -Inf, xend = 251, yend = 718.9602), linetype="dashed") +
   geom_text(aes(x=71, y= 450, label = "71"), hjust=-0.3, vjust=1, colour="grey44", size = 3.2) +
   geom_text(aes(x=251, y= 450, label = "251"), hjust=-0.2, vjust=1, colour="grey44", size = 3.2) +
   geom_segment(aes(x = -Inf, y = 494.0864, xend = 71, yend = 494.0864), linetype="dashed") +
  geom_segment(aes(x = -Inf, y = 718.9602, xend = 251, yend = 718.9602), linetype="dashed") +
  geom_text(aes(x=0, y= 494, label = "494.1"), hjust=0.65, vjust=1.2, colour="grey44", size = 3.2) +
  geom_text(aes(x=0, y= 718, label = "719.0"), hjust=0.65, vjust=1.2, colour="grey44", size = 3.2) +
   geom_segment(aes(x = -Inf, y = 590.8, xend = 346, yend = 590.8), linetype="dashed") +
  geom_text(aes(x=0, y= 590, label = "590.8"), hjust=0.65, vjust=1.2, colour="grey44", size = 3.2) +
   geom_segment(aes(x = 156, y = -Inf, xend = 156, yend = 590.8), linetype="dashed") +
  geom_segment(aes(x = 346, y = -Inf, xend = 346, yend = 590.8), linetype="dashed") +
  geom_text(aes(x=156, y= 450, label = "156"), hjust=-0.2, vjust=1, colour="grey44", size = 3.2) +
  geom_text(aes(x=346, y= 450, label = "346"), hjust=-0.2, vjust=1, colour="grey44", size = 3.2)

#### Expected vs Actual temperature as Mars orbits the Sun ####

ggplot(data=tempdf, aes(x=L, y=temp)) +
  geom_line(size = 0.6, linetype = "dashed", color="grey1") +
  labs(title="Expected vs Actual temperature as Mars orbits the Sun",x="Solar Longitude (degrees)", y = "Temperature (K)") +
  scale_x_continuous(breaks = seq(0, 360, by = 45)) +
  scale_y_continuous(breaks = seq(200, 230, by = 2.5)) +
  theme_bw() +
  geom_segment(aes(x = 71, y = -Inf, xend = 71, yend = 206.2), linetype="dotted") +
  geom_segment(aes(x = 251, y = -Inf, xend = 251, yend = 226.5), linetype="dotted") +
  geom_text(aes(x=71, y= 200, label = "71"), hjust=-0.3, vjust=1, colour="grey44", size = 3.2) +
  geom_text(aes(x=251, y= 200, label = "251"), hjust=-0.2, vjust=1, colour="grey44", size = 3.2) +
  geom_segment(aes(x = -Inf, y = 206.2, xend = 71, yend = 206.2), linetype="dotted") +
  geom_segment(aes(x = -Inf, y = 226.5, xend = 251, yend = 226.5), linetype="dotted") +
  geom_text(aes(x=0, y= 206.2, label = "206.2"), hjust=0.65, vjust=1.2, colour="grey44", size = 3.2) +
  geom_text(aes(x=0, y= 226.5, label = "226.5"), hjust=0.65, vjust=1.2, colour="grey44", size = 3.2) +
  geom_line(data=Ls_actual_temps, aes(x=Ls, y=AvTemp), color="grey20", size=1.2) +
  geom_point(data=Ls_actual_temps, aes(x=Ls, y=AvTemp, color=Season), size=2.2) +
  scale_color_manual(values = c("Spring"='green4',"Summer"='firebrick1',"Autumn"='orange3',"Winter"='cadetblue3')) +
   scale_linetype_manual(values = c("Actual" = "solid", "Expected" = "dashed"))

#### Pressure variation as Mars orbits the Sun ####

ggplot() +
  labs(title="Pressure variation as Mars orbits the Sun",x="Solar Longitude (degrees)", y = "Pressure (Pa)") +
  scale_x_continuous(breaks = seq(0, 360, by = 45)) +
  scale_y_continuous(breaks = seq(600, 800, by = 25)) + expand_limits(y=600:800) +
  theme_bw() +
  geom_line(data=Ls_Actual_Pressures, aes(x=Ls, y=AvP), color="grey20", size=0.8) +
  geom_line(data=Ls_Actual_Pressures, aes(x=Ls, y=MinP), color="grey50", size=0.8, linetype="dashed") +
  geom_line(data=Ls_Actual_Pressures, aes(x=Ls, y=MaxP), color="grey50", size=0.8, linetype="dashed") +
  geom_point(data=Ls_Actual_Pressures, aes(x=Ls, y=AvP, color=Season), size=2.2) +
  scale_color_manual(values = c("Spring"='green3',"Summer"='firebrick1',"Autumn"='orange',"Winter"='cadetblue3'))

####Pressure across season ####


ggplot()+
  geom_line(data=P113e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  geom_line(data=P207e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  geom_line(data=P307e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  geom_line(data=P403e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  geom_line(data=P485e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  geom_line(data=P559e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  geom_line(data=P634e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  geom_line(data=P700e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  geom_line(data=P782e, aes(x=LMST, y=PRESSURE, colour=SOL), size=1.35)+
  theme_bw()+
  scale_x_datetime(date_labels ="%H:%M", date_breaks ="2 hour")+
  scale_colour_manual(values=c("lightgreen", "darkgreen", "violet", "violetred", "darkorange", "darkorange4", "steelblue1", "blue", "blue4"))+
  labs(title="Daily pressure variation across seasons ",
       subtitle ="Sp.E=Spring Equinox, Sp.M=Mid-Spring, Su.S=Summer Solstice, Su.M=Mid-Summer, Au.E=Autumn Equinox, Au.M=Mid-Autumn, Wi.S=Winter Solstice, Wi.M=Mid-Winter, Sp.EE= 2nd Spring Equinox")+
  theme(plot.title = element_text(size=18, face="bold"), plot.subtitle = element_text(size=12))+
  xlab("Local Mean Solar Time") + ylab("Pressure (Pa)")

#### Winter WindRose for Sol 634 ####
windRose(twins_model_0634_01, ws = "HORIZONTAL_WIND_SPEED", wd = "WIND_DIRECTION",
         breaks = c(0,3,6,9,12,15,18),
         auto.text = TRUE,
         paddle = FALSE,
         annotate = FALSE,
         grid.line = 10,
         key = list(labels = c(">0-3",
                               ">3-6",
                               ">6-9",
                               ">9-12",
                               ">12-15",
                               ">15-18")),
         key.footer = "Wind Speed [m/s]",
         key.position = "bottom",
         par.settings = list(axis,line=list(col="lightgray")),
         cols = c("#98F5FF","#82D2EE","#6CAFDD","#568CCD","#4169BC","#2B46AC","#15239B","#00008B"), main="WindRose For Sol 634 in Winter")
#### Autumn WindRose for Sol 485 ####
windRose(twins_model_0485_01, ws = "HORIZONTAL_WIND_SPEED", wd = "WIND_DIRECTION",
         breaks = c(0,3,6,9,12,15,18,22),
         auto.text = TRUE,
         paddle = FALSE,
         annotate = FALSE,
         grid.line = 10,
         key = list(labels = c(">0-3",
                               ">3-6",
                               ">6-9",
                               ">9-12",
                               ">12-15",
                               ">15-18",
                               ">18-22")),,
         key.footer = "Wind Speed [m/s]",
         key.position = "bottom",
         par.settings = list(axis,line=list(col="black")),
         cols = c("#CAFF70", "#B0EB61", "#97D854" ,"#7EC446" ,"#65B138", "#4B9E29" ,"#328A1C" ,"#19770E", "#006400"), main="WindRose For Sol 485 in Autumn")
#### Summer WindRose for Sol 307 ####
windRose(twins_model_0307_01, ws = "HORIZONTAL_WIND_SPEED", wd = "WIND_DIRECTION",
         breaks = c(0,3,6,9,12,15,18,22),
         auto.text = TRUE,
         paddle = FALSE,
         annotate = FALSE,
         grid.line = 5,
         key = list(labels = c(">0-3",
                               ">3-6",
                               ">6-9",
                               ">9-12",
                               ">12-15",
                               ">15-18",
                               ">18-22")),,
         key.footer = "Wind Speed [m/s]",
         key.position = "bottom",
         par.settings = list(axis,line=list(col="lightgray")),
         cols = c("#FFB90F","#F0A10D", "#E28A0B", "#D37309" ,"#C45C07", "#B64505", "#A82E03" ,"#991701", "#8B0000"), main="WindRose For Sol 307 in Summer")
#### Spring WindRose for Sol 114 ####
windRose(twins_modelevent_0114_03, ws = "HORIZONTAL_WIND_SPEED", wd = "WIND_DIRECTION",
         breaks = c(0,3,6,9,12,15,18,22),
         auto.text = TRUE,
         paddle = FALSE,
         annotate = FALSE,
         grid.line = 10,
         key = list(labels = c(">0-3",
                               ">3-6",
                               ">6-9",
                               ">9-12",
                               ">12-15",
                               ">15-18")),
         key.footer = "Wind Speed [m/s]",
         key.position = "bottom",
         par.settings = list(axis,line=list(col="lightgray")),
         cols = c("#FFB6C1", "#EE9DB0" ,"#DD84A0", "#CD6C90", "#BC5380", "#AC3B70","#9B2260" ,"#8B0A50"), main="WindRose For Sol 114 in Spring")

#### Wind speed across seasons (unsmoothed) ####
ggplot()+
  geom_point(data=T119, aes(x=LMST, y=HORIZONTAL_WIND_SPEED, colour=SOL), size=0.65, alpha=0.85)+
  geom_point(data=T307, aes(x=LMST, y=HORIZONTAL_WIND_SPEED, colour=SOL), size=0.65, alpha=0.75)+
  geom_point(data=T485, aes(x=LMST, y=HORIZONTAL_WIND_SPEED, colour=SOL), size=0.65, alpha=1)+
  geom_point(data=T634, aes(x=LMST, y=HORIZONTAL_WIND_SPEED, colour=SOL), size=0.65, alpha=0.69)+
  theme_bw()+
  scale_x_datetime(date_labels ="%H:%M", date_breaks ="2 hour")+
  scale_colour_manual(values=c("lightgreen", "violet", "darkorange", "steelblue1"))+
  labs(title="Daily wind speeds across seasons ",
       subtitle ="Green dots=Spring Equinox, Purple dots=Summer Solstice, Orange dots=Autumn Equinox, Wi.S=Winter Solstice")+
  theme(plot.title = element_text(size=18, face="bold"), plot.subtitle = element_text(size=12))+
  xlab("Local Mean Solar Time") + ylab("Wind Speed (m/s)")

#### Wind speed across seasons (smoothed) ####

TT119 <-as.data.frame(T119)
TT207 <-as.data.frame(T207)
TT307 <-as.data.frame(T307)
TT403 <-as.data.frame(T403)
TT485 <-as.data.frame(T485)
TT559 <-as.data.frame(T559)
TT634 <-as.data.frame(T634)
TT700 <-as.data.frame(T700)
TT755 <-as.data.frame(T755)
ggplot()+
  geom_smooth(data = TT119, size=1.2, aes(x=LMST, y=HORIZONTAL_WIND_SPEED, colour=SOL), method = lm, formula =  y ~ splines::bs(x,8),se =FALSE)+
  geom_smooth(data = TT307, size=1.2, aes(x=LMST, y=HORIZONTAL_WIND_SPEED, colour=SOL), method = lm, formula =  y ~ splines::bs(x,8),se =FALSE)+
  geom_smooth(data = TT485, size=1.2, aes(x=LMST, y=HORIZONTAL_WIND_SPEED, colour=SOL), method = lm, formula =  y ~ splines::bs(x,8),se =FALSE)+
  geom_smooth(data = TT634, size=1.2, aes(x=LMST, y=HORIZONTAL_WIND_SPEED, colour=SOL), method = lm, formula =  y ~ splines::bs(x,8),se =FALSE)+
  theme_bw()+
  scale_x_datetime(date_labels ="%H:%M", date_breaks ="2 hour")+
  scale_colour_manual(values=c("lightgreen", "violet",  "darkorange", "steelblue1"))+
  labs(title="Daily Wind variation across seasons ",
       subtitle ="Sp.E=Spring Equinox, Su.S=Summer Solstice, Au.E=Autumn Equinox, Wi.S=Winter Solstice")+
  theme(plot.title = element_text(size=18, face="bold"), plot.subtitle = element_text(size=12))+
  xlab("Local Mean Solar Time") + ylab("Wind Speed (m/s)")

#### Diurnal Cycle of Wind Direction for a week during Winter ####
W15na <- W15[!(is.na(W15$WIND_DIRECTION)), ]
W16na <- W16[!(is.na(W16$WIND_DIRECTION)), ]
W17na <- W17[!(is.na(W17$WIND_DIRECTION)), ]
W18na <- W18[!(is.na(W18$WIND_DIRECTION)), ]
W19na <- W19[!(is.na(W19$WIND_DIRECTION)), ]
W20na <- W20[!(is.na(W20$WIND_DIRECTION)), ]
W21na <- W21[!(is.na(W21$WIND_DIRECTION)), ]
ggplot() +
  geom_jitter(data = W15na, aes(x = LMST, y = WIND_DIRECTION), color='dodgerblue1', size=0.5, height=4) +
  geom_jitter(data = W16na, aes(x = LMST, y = WIND_DIRECTION), color='dodgerblue1', size=0.5, height=4) +
  geom_jitter(data = W17na, aes(x = LMST, y = WIND_DIRECTION), color='dodgerblue1', size=0.5, height=4) +
  geom_jitter(data = W18na, aes(x = LMST, y = WIND_DIRECTION), color='dodgerblue1', size=0.5, height=4) +
  geom_jitter(data = W19na, aes(x = LMST, y = WIND_DIRECTION), color='dodgerblue1', size=0.5, height=4) +
  geom_jitter(data = W20na, aes(x = LMST, y = WIND_DIRECTION), color='dodgerblue1', size=0.5, height=4) +
  geom_jitter(data = W21na, aes(x = LMST, y = WIND_DIRECTION), color='dodgerblue1', size=0.5, height=4) +
  theme_bw() +
  theme(plot.title = element_text(size=18, face="bold"), plot.subtitle = element_text(size=10)) +
  labs(title="Diurnal Cycle of Wind Direction for a week during Winter", subtitle = "0° and 360° = Northerly, 90° = Westerly, 180° = Southerly, 270° = Easterly",x="LMST", y = "Wind Direction (°)") +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") +
  scale_y_continuous(breaks = seq(0, 360, by = 45), expand = c(0, 0))

#### Summer Meridional Component (V) ####
windradians<- (SUMMER3V$WINDDIRECTION)*(pi/180) 
v <- (SUMMER3V$WINDSPEED) * sin(windradians)
ggplot(data= SUMMER3V, aes(x=LMST, y= v, group=1)) +
  geom_line(color="darkorange", size = 1) + theme_bw() + labs(
    x = "LMST",
    y = "Meridional Wind Component (V)",
    title ="Meridional Wind in Summer for 3 Consecutive Sols",
    subtitle = "") + scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hour")
#### Winter Meridional Component (V) ####
windradians1 <- (WINTER3V$WINDDIRECTION)*(pi/180) 
v1 <- (WINTER3V$WINDSPEED) * sin(windradians1)
ggplot(data= WINTER3V, aes(x=LMST, y=v1, group=1)) +
  geom_line(color="darkblue", size = 1) + theme_bw() + labs(
    x = "LMST",
    y = "Meridional Wind Component (V)",
    title = "Meridional Wind in Winter for 3 Consecutive Sols  ",
    subtitle = "")+ scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hour")
