install.packages(devtools)
library(devtools)
install_github('fawda123/CTDplot')
library(CTDplot)

load(file = 'C:/Users/mbeck/Desktop/ctd_dat.RData')

# get all unique sample dates, select one to plot
dts <- unique(ctd_dat$Date)
dt <- dts[2]

# plot
# ?ctd_plot for help file
ctd_plot(ctd_dat, 'Salinity', date = dt)
