# should have svm_ss, randomforest_ss, neuralnet_ss


# fname = paste0(proj_dir, "svm_ss.png")
# png(filename = fname)
plot2 = ggplot() + theme_bw() + 
  ylim(0, 1) + 
  xlim(0, 1) + 
  theme(legend.justification=c(0,0), legend.position=c(0.01,0), 
        legend.text = element_text(size = 20), 
        legend.title=element_blank()) + 
  guides(col = guide_legend(ncol = 1)) + 
  theme(axis.text.x=element_text( size=20, vjust = 0.5)) + 
  theme(axis.text.y=element_text( size=20, vjust=0.5)) + 
  scale_size(range=c(3,5), guide="none") +
  guides(colour = guide_legend(override.aes = list(size=5))) + 
  labs(fill="") + 
  labs(title = 'Sens/Spec Curve for SVM',
       x = 'Sensitivity',
       y = 'Specificity') + 
  geom_point(data = svm_ss,
             aes(x = sens, y = spec, size = 2, color = 'SVM')) + 
  theme(plot.title = element_text( color="#666666", face="bold", size=32, hjust=0.5)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=22)) 

print(plot2)

# dev.off()