lc_theme <- function()
{
    theme(
        axis.line         = element_blank(),
        axis.text.x       = element_text(lineheight = 0.9, vjust = 1),
        axis.text.y       = element_text(lineheight = 0.9, hjust = 1, face = "bold"),
        axis.title.x      = element_text(vjust = 1),
        axis.title.y      = element_text(angle = 90, vjust = 0.5),
        axis.ticks.length = unit(0.3, "lines"),
        
        legend.background = element_rect(colour=NA), 
        legend.key        = element_rect(colour = "grey80"),
        legend.key.size   = unit(1.2, "lines"),
        legend.title      = element_text(face = "bold", hjust = 0),
        legend.position   = "right",
        
        panel.background = element_rect(fill = "white", colour = NA), 
        panel.border     = element_rect(fill = NA, colour = "black"), 
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = "grey98", size = 0.5),
        
        strip.background = element_rect(fill = "grey80", colour = "grey50"), 
        strip.text.y     = element_text(angle = -90),
        
        plot.background  = element_rect(colour = NA))
}