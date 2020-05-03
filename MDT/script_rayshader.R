library(dplyr)
library(raster)
r <- raster::raster("data_unzip/mdt_lidar_2017_25m_etrs89.tif")
elmat <- rayshader::raster_to_matrix(r)
plot(r, maxpixels=50000)

box <- spatstat::clickbox(add=T)
poly <- spatstat::clickpoly(add=T)

x_range <- round(r@ncols*(box$xrange - r@extent@xmin)/(r@extent@xmax-r@extent@xmin))
y_range <- round(r@nrows*(box$yrange - r@extent@ymin)/(r@extent@ymax-r@extent@ymin))

x_range <- round(r@ncols*(poly$xrange - r@extent@xmin)/(r@extent@xmax-r@extent@xmin))
y_range <- round(r@nrows*(poly$yrange - r@extent@ymin)/(r@extent@ymax-r@extent@ymin))

x_bdry <- round(r@ncols*(poly$bdry[[1]]$x - r@extent@xmin)/(r@extent@xmax-r@extent@xmin))
y_bdry <- round(r@nrows*(poly$bdry[[1]]$y - r@extent@ymin)/(r@extent@ymax-r@extent@ymin))

elmat_subset <- elmat[seq(x_range[1], x_range[2]), r@nrows - rev(seq(y_range[1], y_range[2]))]
elmat_subset[is.na(elmat_subset)] <- 0.0

# point in polygon
# eg <- expand.grid(1:nrow(elmat),1:ncol(elmat))
# pip <- sp::point.in.polygon(eg[,1], eg[,2], rep(x_range, 2), rep(y_range, each=2))
# which(pip != 0)
# elmat_subset <- elmat[pip != 0]

# manual rectangle
x0 <- 4600
y0 <- 500
w <- 1000
h <- 500
elmat_subset <- elmat[x0:min(x0+w, nrow(elmat)), y0:min(y0+h,ncol(elmat))]



# elmat_subset[is.na(elmat_subset)] <- 0.0
Burg <-     "#ffc6c4,#f4a3a8,#e38191,#cc607d,#ad466c,#8b3058,#672044" %>% strsplit(",") %>% unlist()
BurgYl <-   "#fbe6c5,#f5ba98,#ee8a82,#dc7176,#c8586c,#9c3f5d,#70284a" %>% strsplit(",") %>% unlist()
RedOr <-    "#f6d2a9,#f5b78e,#f19c7c,#ea8171,#dd686c,#ca5268,#b13f64" %>% strsplit(",") %>% unlist()
OrYel <-    "#ecda9a,#efc47e,#f3ad6a,#f7945d,#f97b57,#f66356,#ee4d5a" %>% strsplit(",") %>% unlist()
Peach <-    "#fde0c5,#facba6,#f8b58b,#f59e72,#f2855d,#ef6a4c,#eb4a40" %>% strsplit(",") %>% unlist()
PinkYl <-   "#fef6b5,#ffdd9a,#ffc285,#ffa679,#fa8a76,#f16d7a,#e15383" %>% strsplit(",") %>% unlist()
Mint <-     "#e4f1e1,#b4d9cc,#89c0b6,#63a6a0,#448c8a,#287274,#0d585f" %>% strsplit(",") %>% unlist()
BluGrn <-   "#c4e6c3,#96d2a4,#6dbc90,#4da284,#36877a,#266b6e,#1d4f60" %>% strsplit(",") %>% unlist()
DarkMint <- "#d2fbd4,#a5dbc2,#7bbcb0,#559c9e,#3a7c89,#235d72,#123f5a" %>% strsplit(",") %>% unlist()
Emrld <-    "#d3f2a3,#97e196,#6cc08b,#4c9b82,#217a79,#105965,#074050" %>% strsplit(",") %>% unlist()
BluYl <-    "#f7feae,#b7e6a5,#7ccba2,#46aea0,#089099,#00718b,#045275" %>% strsplit(",") %>% unlist()
Teal <-     "#d1eeea,#a8dbd9,#85c4c9,#68abb8,#4f90a6,#3b738f,#2a5674" %>% strsplit(",") %>% unlist()
TealGrn <-  "#b0f2bc,#89e8ac,#67dba5,#4cc8a3,#38b2a3,#2c98a0,#257d98" %>% strsplit(",") %>% unlist()
Purp <-     "#f3e0f7,#e4c7f1,#d1afe8,#b998dd,#9f82ce,#826dba,#63589f" %>% strsplit(",") %>% unlist()
PurpOr <-   "#f9ddda,#f2b9c4,#e597b9,#ce78b3,#ad5fad,#834ba0,#573b88" %>% strsplit(",") %>% unlist()
Sunset <-   "#f3e79b,#fac484,#f8a07e,#eb7f86,#ce6693,#a059a0,#5c53a5" %>% strsplit(",") %>% unlist()
Magenta <-  "#f3cbd3,#eaa9bd,#dd88ac,#ca699d,#b14d8e,#91357d,#6c2167" %>% strsplit(",") %>% unlist()
SubsetDk <- "#fcde9c,#faa476,#f0746e,#e34f6f,#dc3977,#b9257a,#7c1d6f" %>% strsplit(",") %>% unlist()
BrwnYl <-   "#ede5cf,#e0c2a2,#d39c83,#c1766f,#a65461,#813753,#541f3f" %>% strsplit(",") %>% unlist()

order <- c(3,6,5,4,2)

texture <- "desert"
water <- "#003f68"
texture <- do.call(rayshader::create_texture, as.list(BluYl[order]))

elmat_subset %>% 
  rayshader::sphere_shade(sunangle = 45, texture = texture) %>%
  rayshader::add_water(rayshader::detect_water(elmat_subset), color = water) %>%
  rayshader::add_shadow(rayshader::ray_shade(elmat_subset, zscale = 25, sunaltitude = 60, sunangle = 150), max_darken = 0.7) %>%
  # rayshader::add_shadow(rayshader::ambient_shade(elmat_subset, zscale = 25), max_darken = 0.7) %>%
  rayshader::plot_map()

elmat_subset %>% 
  rayshader::sphere_shade(sunangle = 45, texture = texture) %>%
  rayshader::add_water(rayshader::detect_water(elmat_subset), color = water) %>%
  rayshader::add_shadow(rayshader::ray_shade(elmat_subset, zscale = 25, sunaltitude = 60, sunangle = 150), max_darken = 0.7) %>%
  # rayshader::add_shadow(rayshader::ambient_shade(elmat_subset), max_darken = 0.7) %>%
  rayshader:: plot_3d(elmat_subset, zscale = 25, fov = 0, theta = -135, 
                      zoom = 0.75, phi = 35, windowsize = c(1000, 800),
                      baseshape = "rectangle", lineantialias=T)
# rayshader::render_label(elmat_subset, x = 350, y = 160, z = 1000, zscale = 50,
#              text = "Moss Landing", textsize = 2, linewidth = 5)
Sys.sleep(1)
rayshader::render_snapshot(clear=TRUE, filename = "tmp.png")
# rayshader::render_depth(preview_focus = T, focus = 0.6, focallength = 200, clear = TRUE)
# rayshader::render_highquality(samples = 10, clear=TRUE)


theta_array <- seq(0, 360, by = 1)
for(theta in theta_array){
  elmat_subset %>% 
    rayshader::sphere_shade(sunangle = 45, texture = texture) %>%
    rayshader::add_water(rayshader::detect_water(elmat_subset), color = water) %>%
    rayshader::add_shadow(rayshader::ray_shade(elmat_subset, zscale = 25, sunaltitude = 60, sunangle = 150), max_darken = 0.7) %>%
    # rayshader::add_shadow(rayshader::ambient_shade(elmat_subset), max_darken = 0.7) %>%
    rayshader:: plot_3d(elmat_subset, zscale = 25, fov = 0, theta = theta, 
                        zoom = 0.75, phi = 35, windowsize = c(1000, 800),
                        baseshape = "rectangle", lineantialias=T)
  # rayshader::render_label(elmat_subset, x = 350, y = 160, z = 1000, zscale = 50,
  #              text = "Moss Landing", textsize = 2, linewidth = 5)
  Sys.sleep(1)
  fn <- paste0("tmp_theta_", theta, ".png")
  print(fn)
  rayshader::render_snapshot(clear=TRUE, filename = fn)

}
      