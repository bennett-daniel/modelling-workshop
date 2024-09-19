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

## set the rng seed
set.seed(112358)

## specify the timesteps (in years) and amount
timesteps <- c(0,1/12,2/12,3/12,6/12,1,2)
ss_comparison_amount <- 100

## specify the hyperbolic discounting function and its inverse
hyperbolic_discount_fun <- function(amount, delay, k){
  return(amount * (1 / (1 + k * delay)))
}

## simulate some participants
group_mean_k <- 0.7
group_sd_k <- 0.35
n_participants <- 10
all_k <- rnorm(n=n_participants, mean=group_mean_k, sd=group_sd_k)

## simulate group-mean indifference points
group_present_equivalents <- hyperbolic_discount_fun(amount=ss_comparison_amount, delay=timesteps, k=group_mean_k)
group_indifference_point  <- ss_comparison_amount * ss_comparison_amount / hyperbolic_discount_fun(amount=ss_comparison_amount, delay=timesteps, k=group_mean_k)

## simulate some indifference points for each participant
all_present_equivalents <- matrix(NA, nrow=n_participants, ncol=length(timesteps))
all_indifference_point  <- matrix(NA, nrow=n_participants, ncol=length(timesteps))

for (p_ix in 1:n_participants){
  all_present_equivalents[p_ix,] <- hyperbolic_discount_fun(amount=ss_comparison_amount, delay=timesteps, k=all_k[p_ix])
  all_indifference_point[p_ix,]  <- ss_comparison_amount * ss_comparison_amount / hyperbolic_discount_fun(amount=ss_comparison_amount, delay=timesteps, k=all_k[p_ix])
}

## convert data for plotting
group_data <- data.frame(
  timestep           = timesteps,
  present_equivalent = group_present_equivalents,
  indifference_point = group_indifference_point
)

indiv_data <- data.frame(
  id                 = as.factor(rep(1:n_participants, each=length(timesteps))),
  timestep           = rep(timesteps, times=n_participants),
  present_equivalent = as.vector(t(all_present_equivalents)),
  indifference_point  = as.vector(t(all_indifference_point))
)

## plot group-mean data
group_plot_present_equiv <- ggplot(data = group_data, mapping = aes(x=timestep, y=present_equivalent)) +
                              geom_line(colour=brewer.pal(name="Set1", n=9)[2]) +
                              geom_point(size=2, colour=brewer.pal(name="Set1", n=9)[2]) +
                              scale_x_continuous(name = "Delay time (in months)", breaks=timesteps,labels=c(0,1,2,3,6,12,24)) + 
                              scale_y_continuous(name = "Subjective value of $100\n") +
                              coord_capped_cart(left="both", bottom="both", xlim=c(0,2), ylim=c(0,100))
# group_plot_present_equiv


group_plot_indiff_point <- ggplot(data = group_data, mapping = aes(x=timestep, y=indifference_point)) +
                            geom_line(colour=brewer.pal(name="Set1", n=9)[2]) +
                            geom_point(size=2, colour=brewer.pal(name="Set1", n=9)[2]) +
                            scale_x_continuous(name = "Delay time (in months)", breaks=timesteps,labels=c(0,1,2,3,6,12,24)) +
                            scale_y_continuous(name = "Indifference point\n") +
                            coord_capped_cart(left="both", bottom="both", xlim=c(0,2), ylim=c(0,1000))
# group_plot_indiff_point

joint_plot_group <- cowplot::plot_grid(group_plot_indiff_point, group_plot_present_equiv, nrow=1, label_fontface="bold", label_size=14, labels=c("A) Survey responses", "B) Subjective value at different delays"), hjust=0) 
cowplot::save_plot(plot=joint_plot_group, filename=here::here("plots", "placeholder-group-plot.png"),base_height = 9, base_width=20, units="cm")

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
cowplot::save_plot(plot=joint_plot_indiv, filename=here::here("plots", "placeholder-indiv-plot.png"),base_height = 9, base_width=20, units="cm")

## plot