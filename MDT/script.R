# ASC ---------------------------------------------------------------------

library(dplyr)
library(raster)
r <- raster::raster("~/Downloads/MDT_064_5_1.asc")
plot(r)

df <- as.data.frame(r, xy=TRUE)

f <- 100
levels_x <- df %>% dplyr::pull(x) %>% unique
levels_y <- df %>% dplyr::pull(y) %>% unique
levels_y[seq(1,length(levels_y), by=10)]
df %>% 
  dplyr::mutate(z = MDT_064_5_1) %>% 
  dplyr::group_by(x) %>% 
  dplyr::mutate(level_x = 1:n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(y) %>% 
  dplyr::mutate(level_y = 1:n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(level_y %in% seq(1, max(level_y), by=10)) %>% 
  dplyr::filter(level_x %in% seq(1, max(level_x), by=10)) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=x, y=y + 1*z, group=y), alpha=0.1, size=0.5, na.rm=T)+
  # ggplot2::coord_equal(ratio = diff(range(df$y))/diff(range(df$x)))+
  ggplot2::coord_equal()+
  ggplot2::theme_minimal()
  


# Multiple ASC ------------------------------------------------------------

library(dplyr)
library(raster)
dir <- "data_unzip/"
files <- c("MDT_064_4_1", "MDT_064_4_2", "MDT_064_5_1", "MDT_064_5_2", "MDT_064_6_1", "MDT_064_6_2")
# files <- c("MDT_064_4_1", "MDT_064_4_2") #, "MDT_064_5_1", "MDT_064_5_2", "MDT_064_6_1", "MDT_064_6_2")
ext <- ".asc"

limits <- lapply(files, function(f){
  r <- raster::raster(paste0(dir, f, ext))
  data.frame(xmin=r@extent@xmin, xmax=r@extent@xmax, 
             ymin=r@extent@ymin, ymax=r@extent@ymax)
}) %>% bind_rows()

res_x <- 20
res_y <- 20

res_x <- 50
res_y <- 100

levels_x <- seq(min(limits$xmin), max(limits$xmax), by=res_x)
levels_y <- seq(min(limits$ymin), max(limits$ymax), by=res_y)


read_asc <- function(file){
  raster::raster(file) %>% 
  raster::as.data.frame(xy=T) %>% 
    `colnames<-`(c("x","y","z"))
}

filter_asc <- function(data, levels_x, levels_y){
  data %>% 
    dplyr::filter(round(x) %in% levels_x) %>%
    dplyr::filter(round(y) %in% levels_y)
    # dplyr::filter(y %in% levels_y[seq(1, max(levels_y), by=res_y)])
}

df <- pbapply::pblapply(files, function(f){
  paste0(dir, f, ext) %>% 
    read_asc() %>% 
    filter_asc(levels_x, levels_y)
}, cl = 4) %>% dplyr::bind_rows(.id = "id")


theta <- 0 #pi/6 + pi/4
df %>% 
  dplyr::mutate(z = ifelse(z < 0.5, NA, z)) %>%
  dplyr::distinct(x,y,z) %>% 
  dplyr::mutate(y = cos(theta)*y) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=x, y=y + 1*z, group=y, color=z), alpha=0.7, size=0.5, na.rm=T)+
  ggplot2::scale_color_viridis_c(guide=F)+
  ggplot2::coord_equal()+
  ggplot2::theme_void()+
  ggplot2::theme(panel.grid = ggplot2::element_blank())+
  ggplot2::ggsave("test_color.png", width=16, height=9)

theta <- 0
df %>% 
  dplyr::mutate(z = ifelse(z < 0.5, NA, z)) %>%
  dplyr::distinct(x,y,z) %>% 
  dplyr::mutate(y = cos(theta)*y) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=x, y=y + 1*z, group=y), color="white", alpha=0.35, size=0.3, na.rm=T)+
  ggplot2::scale_x_continuous(expand = c(0,0))+
  ggplot2::scale_y_continuous(expand = c(0,0))+
  ggplot2::coord_equal()+
  ggplot2::theme_void()+
  ggplot2::theme(panel.background = ggplot2::element_rect(fill="#161616"),
                 plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  ggplot2::ggsave("test_bw.png", width=16, height=9)

theta <- 0
df %>% 
  dplyr::mutate(z = ifelse(z < 0.5, NA, z)) %>%
  dplyr::distinct(x,y,z) %>% 
  dplyr::mutate(y = cos(theta)*y) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=x, y=y + 1*z, group=y, color=z), alpha=0.3, size=0.1, na.rm=T)+
  ggplot2::scale_color_gradient(guide=F, low="#161616", high="#7F8C8D", limits=c(0, 100))+
  ggplot2::scale_x_continuous(expand = c(0,0))+
  ggplot2::scale_y_continuous(expand = c(0,0))+
  ggplot2::coord_equal()+
  ggplot2::theme_void()+
  ggplot2::theme(panel.background = ggplot2::element_rect(fill="#0a0a0a"),
                 plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  ggplot2::ggsave("test_bw_gradient.png", width=16, height=9, dpi=400)


theta <- 5*pi/12
df %>% 
  dplyr::mutate(z = ifelse(z < 0.5, NA, z)) %>%
  dplyr::distinct(x,y,z) %>% 
  dplyr::mutate(y = cos(theta)*y) %>%
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=x, y=y + -1*z, group=y, color=z), alpha=0.3, size=0.1, na.rm=T)+
  ggplot2::scale_color_gradient(guide=F, low="#161616", high="#7F8C8D", limits=c(0, 100))+
  ggplot2::scale_x_continuous(expand = c(0,0))+
  ggplot2::scale_y_continuous(expand = c(0,0))+
  ggplot2::coord_equal()+
  ggplot2::theme_void()+
  ggplot2::theme(panel.background = ggplot2::element_rect(fill="#0a0a0a"),
                 plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  ggplot2::ggsave("test_bw_gradient_out.png", width=16, height=9, dpi=400)

df %>% 
  dplyr::mutate(z = ifelse(z < 0.5, NA, z)) %>%
  dplyr::distinct(x,y,z) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=-y, y=x + 1*z, group=x, color=z), alpha=0.5, size=0.3, na.rm=T)+
  ggplot2::scale_color_gradient(guide=F, low="#000000", high="#7F8C8D", limits=c(0, 100))+
  ggplot2::scale_x_continuous(expand = c(0,0))+
  ggplot2::scale_y_continuous(expand = c(0,0))+
  ggplot2::coord_equal()+
  ggplot2::theme_void()+
  ggplot2::theme(panel.background = ggplot2::element_rect(fill="#161616"),
                 plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  ggplot2::ggsave("test_bw_gradient_inv.png", width=16, height=9, dpi=400)
