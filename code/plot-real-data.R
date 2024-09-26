## load required packages
library(ggplot2)
library(RColorBrewer)
library(lemon)
library(cowplot)

## set the ggplot palette
theme_set(theme_bw())
theme_replace(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.title.x = element_text(size=11, colour="black", margin=margin(t=8)),
              axis.title.y = element_text(size=11, colour="black", angle=90, margin=margin(r=8)),
              axis.text.y = element_text(size = 7, colour="black"),
              axis.text.x = element_text(size = 7, colour="black"),
              panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank(), 
              legend.text = element_text(size=8),
              plot.margin = margin(t=35, r=20, b=0, l=20))

## read data
raw_data <- read.csv(here::here("data", "test-raw.csv"))
raw_data <- raw_data[3:nrow(raw_data),18:23]

## exclude some participants
participants_to_exclude <- c(1)
raw_data <- raw_data[-participants_to_exclude,]
n_participants <- nrow(raw_data)

## add a baseline column to the raw data
raw_data <- cbind(rep(100, times=n_participants), raw_data)

## specify the timesteps (in years) and amount
timesteps <- c(0,1/12,2/12,3/12,6/12,1,2)
ss_comparison_amount <- 100

## convert data
indiv_data <- data.frame(
  id                 = as.factor(rep(1:n_participants, each=length(timesteps))),
  timestep           = rep(timesteps, times=n_participants),
  indifference_point = as.numeric(as.vector(t(as.matrix(raw_data))))
)
indiv_data$present_equivalent <- 100 * 100 / indiv_data$indifference_point

## specify the hyperbolic discounting function and its inverse
hyperbolic_discount_fun <- function(amount, delay, k){
  return(amount * (1 / (1 + k * delay)))
}

## plot individual data
indiv_plot_present_equiv <- ggplot(data = indiv_data, mapping = aes(x=timestep, y=present_equivalent, group=id, colour=id)) +
  geom_line() +
  geom_point(size=2) +
  scale_colour_manual(values=rep(brewer.pal(name="Set3", n=11)[c(1,3:11)], times=5)) + 
  scale_x_continuous(name = "Delay time (in months)", breaks=timesteps,labels=c(0,1,2,3,6,12,24)) + 
  scale_y_continuous(name = "Subjective value of $100\n") +
  coord_capped_cart(left="both", bottom="both", xlim=c(0,2), ylim=c(0,100)) +
  theme(legend.position="none")
indiv_plot_present_equiv


indiv_plot_indiff_point <- ggplot(data = indiv_data, mapping = aes(x=timestep, y=indifference_point, group=id, colour=id)) +
  geom_line() +
  geom_point(size=2) +
  scale_colour_manual(values=rep(brewer.pal(name="Set3", n=11)[c(1,3:11)], times=5)) + 
  scale_x_continuous(name = "Delay time (in months)", breaks=timesteps,labels=c(0,1,2,3,6,12,24)) +
  scale_y_continuous(name = "Indifference point\n") +
  coord_capped_cart(left="both", bottom="both", xlim=c(0,2), ylim=c(0,1000)) +
  theme(legend.position="none")
indiv_plot_indiff_point

joint_plot_indiv <- cowplot::plot_grid(indiv_plot_indiff_point, indiv_plot_present_equiv, nrow=1, label_fontface="bold", label_size=14, labels=c("A) Survey responses", "B) Subjective value at different delays"), hjust=0) 
cowplot::save_plot(plot=joint_plot_indiv, filename=here::here("plots", "indiv-plot.png"),base_height = 9, base_width=20, units="cm")

## plot