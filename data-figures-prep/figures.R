library(sf)
library(tidyverse)
library(glue)
library(units)


# detection-nondetection.png ----

set.seed(1)

# generate random points
grid_size <- 0.125
pts <- data.frame(x = runif(1000, 0, 1) %>% sqrt(),
                  y = runif(1000, 0, 1)^2) %>%
  mutate(obs = c(rep("detection", 10), rep("non-detection", 990)) %>%
           factor(levels = c("detection", "non-detection")),
         x_grid = x %/% grid_size,
         y_grid = y %/% grid_size,
         grid_cell = paste(x_grid, y_grid, sep = "-"))

# only detections
pts_detections <- filter(pts, obs == "detection")
gg_detections <- ggplot(pts_detections) +
  aes(x = x, y, color = obs, size = obs) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.background = element_rect(),
        panel.grid = element_blank())

# high frequency
pts_high <- pts %>%
  filter(obs == "non-detection") %>%
  slice_sample(n = 40) %>%
  bind_rows(pts_detections)
gg_high <- ggplot(pts_high) +
  aes(x = x, y, color = obs, size = obs) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.background = element_rect(),
        panel.grid = element_blank())

# medium frequency
pts_medium <- pts %>%
  filter(obs == "non-detection") %>%
  slice_sample(n = 240) %>%
  bind_rows(pts_detections)
gg_medium <- ggplot(pts_medium) +
  aes(x = x, y, color = obs, size = obs) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.background = element_rect(),
        panel.grid = element_blank())

# low frequency
gg_low <- ggplot(pts) +
  aes(x = x, y, color = obs, size = obs) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.background = element_rect(),
        panel.grid = element_blank())
ggsave("images/detection-nondetection_detections.png", gg_detections,
       width = 200, height = 200, units = "px",
       scale = 7, bg = "white")
ggsave("images/detection-nondetection_high.png", gg_high,
       width = 200, height = 200, units = "px",
       scale = 7, bg = "white")
ggsave("images/detection-nondetection_medium.png", gg_medium,
       width = 200, height = 200, units = "px",
       scale = 7, bg = "white")
ggsave("images/detection-nondetection_low.png", gg_low,
       width = 200, height = 200, units = "px",
       scale = 7, bg = "white")
