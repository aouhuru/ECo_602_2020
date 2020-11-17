plot(x=dat_bird_hab$elev, y=dat_bird_hab$ba.tot)
data_center_x= mean(dat_bird_hab$elev)
data_center_y=mean(dat_bird_hab$ba.tot)
c(data_center_x, data_center_y)
plot(x=dat_bird_hab$elev, y=dat_bird_hab$ba.tot)
points(x=data_center_x, y=data_center_y, col= "red")


plot(x=dat_bird_hab$slope, y=dat_bird_hab$ba.tot)
plot(x=dat_bird_hab$aspect, y=dat_bird_hab$ba.tot)
