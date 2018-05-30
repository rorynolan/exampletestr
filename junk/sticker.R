pkgs <- c("hexSticker", "tidyverse", "here", "magick")
invisible(lapply(pkgs, library, character.only = TRUE))

sticker(here("junk", "exampletestr.png"),
        package = "exampletestr",
        filename = here("junk", "sticker.png"),
        s_width = 0.48,
        s_x = 1.035, s_y = 0.86,
        p_y = 1.6, p_size = 5,
        url = "github.com/rorynolan/exampletestr",
        u_x = 1.13, u_color = "white", u_size = 0.8)
image_read(here("junk", "sticker.png"))
