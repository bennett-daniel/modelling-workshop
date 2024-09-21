## load packages
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

## load in data
bruder_data <- read.csv(here::here("data", "bruder-all.csv"))

## grab a single participant
p_ix <- 4
p_data <- subset(bruder_data, bruder_data$SubID == unique(bruder_data$SubID)[p_ix])

## specify the hyperbolic discounting function
hyperbolic_discount_fun <- function(amount, delay, k){
  return(amount * (1 / (1 + k * delay)))
}

amount = seq(from=20, to = 80, length.out = 20)
delay = seq(from=00, to = 125, length.out = 20)
background_shading_frame <- data.frame(expand.grid(amount, delay))
colnames(background_shading_frame) <- c("amount", "delay")

k <- 3.73
delay = seq(from=0, to = 125, length.out = 100)
critical_amount = 20 *(1 + k * (delay/365))
# critical_amount = 20 / (k * (delay/365))
background_shading_frame <- data.frame (delay = delay, critical_amount = critical_amount)

data_prediction_plot <- ggplot() +
                          geom_ribbon(data = background_shading_frame, mapping = aes(x = delay, ymin = critical_amount, ymax = Inf), fill = "grey30") +
                          geom_ribbon(data = background_shading_frame, mapping = aes(x = delay, ymax = critical_amount, ymin = -Inf), fill = "grey80") +
                          geom_point(data = p_data, mapping = aes(x=Value.later.delay, y=Value.later, fill=as.factor(Response)), shape=21) +
                          scale_fill_manual(values=c("0" = "white", "1" = "black"), labels = c("chose SS", "chose LL")) +
                          scale_x_continuous(name = "Delay of LL option") +
                          scale_y_continuous(name = "Amount of LL option") +
                          coord_capped_cart(xlim=c(0,125), ylim=c(20,80), left = "both", bottom="both") +
                          theme(legend.position = "bottom")

## plot the implied discount function
plot_delays <- seq(from=0, to = 125, length.out = 30)
discount_function_frame <- data.frame(
  delay            = plot_delays,
  subjective_value = hyperbolic_discount_fun(amount = 20, delay = plot_delays/365, k = k) 
)

implied_discount_plot <- ggplot(data = discount_function_frame, mapping = aes(x=delay, y=subjective_value)) +
                          geom_line(colour=brewer.pal(name="Set1", n=9)[2]) +
                          geom_point(size=2, colour=brewer.pal(name="Set1", n=9)[2]) +
                          scale_x_continuous(name = "Delay time (in days)", breaks=c(0,40,80,120)) + 
                          scale_y_continuous(name = "Subjective value of $20\n") +
                          coord_capped_cart(left="both", bottom="both", xlim=c(0,120), ylim=c(0,20)) +
                          theme(plot.margin = margin(t=35, r=20, b=40, l=20))


joint_plot <- cowplot::plot_grid(implied_discount_plot, data_prediction_plot, nrow=1, rel_widths=c(0.45,0.55), label_fontface="bold", label_size=12, labels=c("A) Subjective value at different delays", "B) Data and predictions"), hjust=0) 
cowplot::save_plot(plot=joint_plot, filename=here::here("plots", sprintf("data_plot_p%.0f_k%0.2f.png", p_ix, k)),base_height = 10, base_width=25, units="cm")


data_prediction_plot_unshaded <- ggplot() +
                                  geom_point(data = p_data, mapping = aes(x=Value.later.delay, y=Value.later, fill=as.factor(Response)), shape=21) +
                                  scale_fill_manual(values=c("0" = "white", "1" = "black"), labels = c("chose SS", "chose LL")) +
                                  scale_x_continuous(name = "Delay of LL option") +
                                  scale_y_continuous(name = "Amount of LL option") +
                                  coord_capped_cart(xlim=c(0,125), ylim=c(20,80), left = "both", bottom="both") +
                                  theme(legend.position = "bottom")

cowplot::save_plot(plot=data_prediction_plot_unshaded, filename=here::here("plots", sprintf("data_plot_p%.0f.png", p_ix)),base_height = 10, base_width=12, units="cm")


