pkgs <- c("hexSticker", "tidyverse", "here", "magick")
invisible(lapply(pkgs, library, character.only = TRUE))

sticker(here("junk", "ijtiff.png"),
        package = "ijtiff",
        filename = here("junk", "sticker.png"),
        s_width = 0.7,
        s_x = 0.99, s_y = 0.8,
        p_y = 1.63,
        url = "github.com/ropensci/ijtiff",
        u_x = 1.13, u_color = "white", u_size = 0.8)
image_read(here("junk", "sticker.png"))
