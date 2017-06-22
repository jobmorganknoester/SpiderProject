# PCA plot unused function :

pcaplot <- function (pca, firstaxis, secondaxis) {
  
  pcares <- cbind(pca$li,proportion$Plot,proportion$Transect) 
  names(pcares)[c(dim(pca$li)[2]+1,dim(pca$li)[2]+2)] <- c("Plot","Transect")
  eig <- pca$eig
  names(eig) <- paste("Axis", c(1:length(eig)))
  eig <- as.data.frame(eig)
  
  
  plot <- ggplot() + 
    theme_bw() + 
    geom_vline(aes(xintercept=0), color="black", size=0.5) +
    geom_hline(aes(yintercept=0), color="black", size=0.5) +
    geom_segment(data=pca$co, aes(x = 0, xend = pca$co[,firstaxis]*10, y = 0, yend = pca$co[,secondaxis]*10), color="black", linetype = "dashed") +
    geom_text(data=pca$co, aes(x=pca$co[,firstaxis]*10, y=pca$co[,secondaxis]*10, label=rownames(pca$co)), size = 3, color="black") +
    geom_text(data=eig, aes(x=max(c(max(pcares[,firstaxis])+0.5,1.5)), y=0, label=paste("Axis",firstaxis,":",round(eig[firstaxis]/pca$rank,3)*100,"%")), size = 3, vjust=1, color="blue") +
    geom_text(data=eig, aes(x=0, y=max(c(max(pcares[,secondaxis])+0.5,1.5)), label=paste("Axis",secondaxis,":",round(eig[secondaxis]/sum(eig),3)*100,"%")), size = 3, vjust=1, color="blue") +
    geom_point(data=pcares, aes(x = pcares[,firstaxis], y =pcares[,secondaxis], color = Plot, shape = Transect), size = 1.5) +
    scale_shape(solid = FALSE)
  
  return (ggplotly(plot))
}

pcaproportion <- dudi.pca(proportion[,c(10:13)], center=T, scale=T, scannf = F, nf = 4)
barplot((pcaproportionlog$eig/length(pcaproportionlog$eig))*100)
# pcaproportionlog$eig
# pcaproportionlog$co

pcaplot(pca = pcaproportion, firstaxis = 1, secondaxis = 2)
pcaplot(pca = pcaproportion, firstaxis = 2, secondaxis = 3)
pcaplot(pca = pcaproportion, firstaxis = 1, secondaxis = 3)
pcaplot(pca = pcaproportion, firstaxis = 1, secondaxis = 4)
pcaplot(pca = pcaproportion, firstaxis = 2, secondaxis = 4)
pcaplot(pca = pcaproportion, firstaxis = 3, secondaxis = 4)





