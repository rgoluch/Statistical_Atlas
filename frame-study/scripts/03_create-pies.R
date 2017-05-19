library(RColorBrewer)
library(munsell)
cols = c(brewer.pal(n = 12, name = "Paired")[c(1,3,5,7,9,11)], "grey80")

#cols <- c("#ffcc80","#ff9900")
#cols <- "#ccf2ff"

colRGB <- t(col2rgb(cols))/256
colMNSL <- rgb2mnsl(colRGB)
colMNSL <- c(colMNSL,
             darker(colMNSL, steps = 2),
             darker(colMNSL, steps = 4))

colHEX <- mnsl(colMNSL)
colHEX <- colHEX[rep(0:6, each = 3) + c(1, 8, 15)]
colHEX <- c(colHEX, "grey60")

church <- read.csv("../../Data/denominations-1874.csv")
cl <- church %>% gather(key = "Denomination",
                        value = "Number",
                        c(4:22, 26))
cl <- cl %>% mutate(Denomination =
                      reorder(Denomination, Number, na.rm = TRUE))

cl$Denomination <- factor(cl$Denomination,
                          c("Unaccommodated", levels(cl$Denomination)[-20]))
levels(cl$Denomination) <- gsub("\\.", " ", levels(cl$Denomination))

cl_data <- cl %>% filter(as.character(STATEICP)==as.character(State)) %>%
  group_by(STATEICP) %>% nest()

cl_data$data <- cl_data$data %>% purrr::map(.f = function(x) {
  x %>%  arrange(desc(Number)) %>% mutate(
    group = c(as.character(Denomination[1:5]), rep("Other", n() - 5))
  ) %>% group_by(group) %>% summarize(Number = sum(Number, na.rm=TRUE))
})

cl_data <- cl_data %>% unnest()



createPie <- function(data, state_name) {
  spine_df <- data %>% filter(STATEICP == state_name)
  if (nrow(spine_df) == 0) return()

  get_plots <- function(sub_data) {
    sub_data <- sub_data %>% mutate(
      group = factor(group),
      group = reorder(group, -Number)
    )
    lvls <- levels(sub_data$group)
    sub_data$group <- factor(sub_data$group,
                             levels=c(c("Unaccommodated", "Other"),
                                      rev(setdiff(lvls, c("Other", "Unaccommodated")))))
    sub_data %>% arrange(desc(group)) %>%
      ggplot() +
      geom_bar(aes(weight = Number,  x = STATEICP, fill = group),
               position = "fill", colour="white", size=0.1,
               width=1) +
      scale_fill_manual("Denomination", values = rev(colHEX)[-c(2:4)]) +
      geomnet::theme_net() +
      theme(legend.position = "none") + ylab("") +
      ggtitle(state_name) + coord_polar(theta="y")
  }
  plot2 <- spine_df %>% get_plots()
  scalars <- spine_df %>% mutate(
    type = group=="Unaccommodated"
  ) %>% group_by(type) %>% summarize(
    Number = sum(Number, na.rm = TRUE)
  )
  if (nrow(scalars)==1) plot1 <- plot2
  else {
  scalars$weight <- scalars$Number/sum(scalars$Number)
  scalars$weight[1] <- sqrt(scalars$weight[1])
  scalars$weight[2] <- 1 - scalars$weight[1]
  spine <- spine_df %>% mutate(Number = replace(Number, group=="Unaccommodated", 0)) %>%
    get_plots()

  spine_wo_df <- ggplot_build(spine)$data[[1]] %>% mutate(
    xmin = 0,
    xmax = scalars$weight[1]
  )
  plot1 <- spine_wo_df %>%
    ggplot(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill="grey60") +
    geom_rect(aes(fill=fill),  colour="white", size=0.1) +
    coord_polar(theta="y") +
    scale_fill_identity() +
    geomnet::theme_net() +
    ggtitle(state_name)
  }
  list(plot1=plot1, plot2=plot2)
}


#########

states <- createPie(data = cl_data, state_name = "Florida")
states[1]
states[2]


# save plots in folder
lvls <- unique(cl_data$STATEICP)
purrr::map(1:length(lvls), function(k) {
  states <- createPie(data = cl_data, state_name = lvls[k])
  if (is.null(states[1])) return()

  print(states[1])
  ggsave( filename = paste0("../test-images/", lvls[k],"-pie_with_frame.png"))
  print(states[2])
  ggsave( filename = paste0("../test-images/", lvls[k],"-pie_without_frame.png"))
})
