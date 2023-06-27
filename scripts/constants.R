colors.voicing = c("blue", "red")

# For plotting
myGplot.defaults("paper")

# Set plotting aesthetics
# change from mm to point scale; keeping the size of geom_text the same as the rest of text in ggplot
geom_text.size <- theme_get()$text$size * 5/14

options(width = 200, digits = 2, OutDec = ".", tinytex.verbose = TRUE)
base.width = 2.15
base.height = 2.15
