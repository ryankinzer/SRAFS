


# change strip text color
# library(grid)
# 
# g <- grid.force(ggplotGrob(qet_fig))
# # Get the names of grobs and their gPaths into a data.frame structure
# grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# # Build optimal gPaths that will be later used to identify grobs and edit them
# grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
# grobs_df$gPath_full <- gsub(pattern = "layout::", 
#                             replacement = "", 
#                             x = grobs_df$gPath_full, 
#                             fixed = TRUE)
# 
# strip_txt_gpath <- tibble(path = grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
#                                              x = grobs_df$gPath_full)]
#                           ) %>%
#   mutate(c = str_sub(path,9,9),
#          r = str_sub(path,11,11)) %>%
#   arrange(r, c) %>%
#   pull(path)#strip_t-'column'-'row'
# 
# #tmp <- strip_txt_gpath[as.numeric(qet_pops)]
# 
# tmp <- strip_txt_gpath[c(1,5,10,13,19,35)]
# txt_colors <- 'red'
# 
# # Edit the grobs
# for (i in 1:length(tmp)){
#   #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
#   g <- editGrob(grob = g, gPath = tmp[i], gp = gpar(col = txt_colors))
# }
# 
# # Draw the edited plot
# grid.newpage(); grid.draw(g)
# # Save the edited plot
# ggsave(paste0('./figures/',yr,'/',gsub(' ','_',spp),'_sa_qet_',qet_value,'_fig2_',yr,'.png'), plot = g, width = 16, height = 10, dpi = 300)