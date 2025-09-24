# creating hex stickers for my projects page in my Quarto lab notebook

# install or call the packages you need to create the stickers
#install.packages("hexSticker")
#install.packages("magick")
library("hexSticker")
library("magick", "lattice", "ggplot")
library(dplyr)

# make sure you know where your images are and you define the output path for when it is generated.
getwd()

# biomarkers
image<- image_read("img/biomarker_bench.jpeg")

biomarker_hex<-sticker(image, package="Biomarkers", p_size=15,
                   p_y = 1.5,
                   s_x=1, s_y=0.8, s_width=1.1,
                   s_height = 14,
                   filename="img/biomarker_hex.png",h_fill="#32006e",h_color = "#32006e")

print(biomarker_hex)

# methylation
image<- image_read("img/mytilustrossulus.png")
methylation_hex<-sticker(image, package="Methylation", p_size=15,
                       p_y = 1.5,
                       s_x=1, s_y=0.7, s_width=0.8,
                       s_height = 8,
                       filename="img/methylation.png",h_fill="#32006e",h_color = "#32006e")
print(methylation_hex)

# mormyrids
image<- image_read("img/mormyrid.jpeg")
mormyrid_hex<-sticker(image, package="Mormyrids", p_size=15,
                       p_y = 1.5,
                       s_x=1, s_y=0.8, s_width=1.1,
                       s_height = 14,
                       filename="img/mormyrid_hex.png",h_fill="#32006e",h_color = "#32006e")
print(mormyrid_hex)

# yellow
image<- image_read("img/hex_chiton.png")
yellow_hex<-sticker(image, package="Yellow Island", p_size=10,
                       p_y = 1.5,
                       s_x=1, s_y=0.8, s_width=1.1,
                       s_height = 14,
                       filename="img/yellow_hex.png",h_fill="#32006e",h_color = "#32006e")
print(yellow_hex)

# presentations
image<- image_read("img/projector.jpeg")
prez_hex<-sticker(image, package="Presentations", p_size=14,
                       p_y = 1.5,
                       s_x=1, s_y=0.8, s_width=0.8,
                       s_height = 14,
                       filename="img/prez_hex.png",h_fill="#32006e",h_color = "#32006e")
print(prez_hex)