library(sf)
library(tidyverse)
library(plot3D)
library(rayshader)
library(abind)
library(stars)
library(rgl)
library(MetBrewer)
library(colorspace)
library(rayrender)
data <- st_read("C://Users//Rahul Ahirwar//Desktop//random_thoughts//maps//population_map//data//kontur_population_IN_20220630.gpkg")
crsLAEA <- "+proj=lcc +lat_0=24 +lon_0=80 +lat_1=12.472955 +lat_2=35.1728044444444 +x_0=4000000 +y_0=4000000"
projected <- st_transform(data,crsLAEA)

head(projected)

bb <- st_bbox(projected)

height <- st_distance(
  st_point(c(bb[["xmin"]], bb[["ymin"]])),
  st_point(c(bb[["xmin"]], bb[["ymax"]]))
)
width <- st_distance(
  st_point(c(bb[["xmin"]], bb[["ymin"]])),
  st_point(c(bb[["xmax"]], bb[["ymin"]]))
)

if (height > width) {
  height_ratio <- 1
  width_ratio <- width / height
} else {
  width_ratio <- 1
  height_ratio <- height / width
}

height_ratio
width_ratio

size <- 1000

rast_ <- st_rasterize(projected,nx=floor(size*width_ratio),
                      ny=floor(size*height_ratio))

mat <- matrix(rast_$population,
              nrow = floor(size * width_ratio),
              ncol = floor(size * height_ratio))


c1 <- met.brewer("Hokusai2")
c2<-met.brewer("OKeeffe2")
print(c2)
print(colorRampPalette(c2,bias=0.1)(256))
texture_ <- grDevices::colorRampPalette(c1,bias=4)(256)
swatchplot(texture_)
print(texture_)

rgl.close()

rgl.clear()

mat |>
  height_shade(texture = texture_) |>
  plot_3d(heightmap = mat ,
          zscale=100,
          solid=FALSE,
          shadowdepth = 0,
          shadow_darkness=.95,
          background="#FBE3C2")
render_camera(theta = -25,phi=60,zoom=.9)

rgl.snapshot("C://Users//Rahul Ahirwar//Desktop//random_thoughts//maps//population_map//images//population_map.png",fmt="png")















rgl.close()


