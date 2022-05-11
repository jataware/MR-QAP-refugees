setwd("C:/Users/jscho/OneDrive/MRQAP")


library(foreign)
library(igraph) ### Remember this conflicts with statnet & sna
library(tidyverse)
library(corrr)
library(ggraph)
library(dplyr)
library(rlang)
library(naniar)
library(statnet)
library(sna)
library(network)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(ggplot2)
library(fmsb)
library(sjmisc)

edges<- read.csv("Data_for_correlation network.csv")

########################################################
### Correlation Networks
########################################################
ref<- edges %>%
  dplyr::select(ref1990.flow,ref1991.flow,ref1992.flow,ref1993.flow,ref1994.flow,
         ref1995.flow,ref1996.flow,ref1997.flow,ref1998.flow,ref1999.flow,
         ref2000.flow,ref2001.flow,ref2002.flow,ref2003.flow,ref2004.flow,
         ref2005.flow,ref2006.flow,ref2007.flow,ref2008.flow,ref2009.flow,
         ref2010.flow,ref1991.flow,ref2012.flow,ref2013.flow,ref2014.flow,
         ref2015.flow,ref2016.flow)

tidy_cors <- ref %>% 
  correlate() %>% 
  corrr::stretch()

graph_cors <- tidy_cors %>% 
  filter(abs(r) > 0.2) %>% 
  graph_from_data_frame(directed = FALSE)

tiff("refugees.1990.2016_3colors.tiff", units="in", width=8, height=6, res=300)

ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2","green")) +
  geom_node_point(color = "black", size = 5) +
  geom_node_text(aes(label = c("1991","1992","1993","1994",
                               "1995","1996","1997","1998","1999",
                               "2000","2001","2002","2003","2004",
                               "2005","2006","2007","2008","2009",
                               "2010","2011","2012","2013","2014",
                               "2015","2016")
                     ), repel = TRUE) +
  theme_graph() +
  labs(title = "Refugee flow correlations \nacross years")

dev.off()

#####################################################
# Matrix Creation
#####################################################
edges.1<- read.csv("Data_1.csv")
edges.2<- read.csv("Data_2.csv")
edges.3<- read.csv("Data_3.csv")
edges.4<- read.csv("Data_4.csv")
edges.5<- read.csv("Data_5.csv")

edges.1<- edges.1 %>%
  select(-state.destination.name,-state.destination.abb,-state.origin.name,
         -state.origin.abb,-dyad.id)

edges.2<- edges.2 %>%
  select(-state.destination.name,-state.destination.abb,-state.origin.name,
         -state.origin.abb,-dyad.id)

edges.3<- edges.3 %>%
  select(-state.destination.name,-state.destination.abb,-state.origin.name,
         -state.origin.abb,-dyad.id)

edges.4<- edges.4 %>%
  select(-state.destination.name,-state.destination.abb,-state.origin.name,
         -state.origin.abb,-dyad.id)

edges.5<- edges.5 %>%
  select(-state.destination.name,-state.destination.abb,-state.origin.name,
         -state.origin.abb,-dyad.id)


edges.m1<- merge(edges.1,edges.2,by.x=c("ccode1","ccode2"),by.y=c("ccode1","ccode2"))
edges.m2<- merge(edges.m1,edges.3,by.x=c("ccode1","ccode2"),by.y=c("ccode1","ccode2"))
edges.m3<- merge(edges.m2,edges.4,by.x=c("ccode1","ccode2"),by.y=c("ccode1","ccode2"))
edges.m4<- merge(edges.m3,edges.5,by.x=c("ccode1","ccode2"),by.y=c("ccode1","ccode2"))

rm(edges.m1,edges.m2,edges.m3)

edges.m4$trade.2010<- as.character(edges.m4$trade.2010)
edges.m4$trade.2010[edges.m4$trade.2010=="#N/A"]<-""
edges.m4$trade.2010<- as.numeric(edges.m4$trade.2010)

edges.m4$trade.2009<- as.character(edges.m4$trade.2009)
edges.m4$trade.2009[edges.m4$trade.2009=="#N/A"]<-""
edges.m4$trade.2009<- as.numeric(edges.m4$trade.2009)

edges.m4$trade.2008<- as.character(edges.m4$trade.2008)
edges.m4$trade.2008[edges.m4$trade.2008=="#N/A"]<-""
edges.m4$trade.2008<- as.numeric(edges.m4$trade.2008)

edges.m4$trade.2007<- as.character(edges.m4$trade.2007)
edges.m4$trade.2007[edges.m4$trade.2007=="#N/A"]<-""
edges.m4$trade.2007<- as.numeric(edges.m4$trade.2007)

edges.m4$trade.2006<- as.character(edges.m4$trade.2006)
edges.m4$trade.2006[edges.m4$trade.2006=="#N/A"]<-""
edges.m4$trade.2006<- as.numeric(edges.m4$trade.2006)

edges.m4$trade.2005<- as.character(edges.m4$trade.2005)
edges.m4$trade.2005[edges.m4$trade.2005=="#N/A"]<-""
edges.m4$trade.2005<- as.numeric(edges.m4$trade.2005)

edges.m4$trade.2004<- as.character(edges.m4$trade.2004)
edges.m4$trade.2004[edges.m4$trade.2004=="#N/A"]<-""
edges.m4$trade.2004<- as.numeric(edges.m4$trade.2004)

edges.m4$trade.2003<- as.character(edges.m4$trade.2003)
edges.m4$trade.2003[edges.m4$trade.2003=="#N/A"]<-""
edges.m4$trade.2003<- as.numeric(edges.m4$trade.2003)

edges.m4$trade.2002<- as.character(edges.m4$trade.2002)
edges.m4$trade.2002[edges.m4$trade.2002=="#N/A"]<-""
edges.m4$trade.2002<- as.numeric(edges.m4$trade.2002)

edges.m4$trade.2001<- as.character(edges.m4$trade.2001)
edges.m4$trade.2001[edges.m4$trade.2001=="#N/A"]<-""
edges.m4$trade.2001<- as.numeric(edges.m4$trade.2001)

edges.m4$trade.2000<- as.character(edges.m4$trade.2000)
edges.m4$trade.2000[edges.m4$trade.2000=="#N/A"]<-""
edges.m4$trade.2000<- as.numeric(edges.m4$trade.2000)

edges.m4$trade.1999<- as.character(edges.m4$trade.1999)
edges.m4$trade.1999[edges.m4$trade.1999=="#N/A"]<-""
edges.m4$trade.1999<- as.numeric(edges.m4$trade.1999)

edges.m4$trade.1998<- as.character(edges.m4$trade.1998)
edges.m4$trade.1998[edges.m4$trade.1998=="#N/A"]<-""
edges.m4$trade.1998<- as.numeric(edges.m4$trade.1998)

edges.m4$trade.1997<- as.character(edges.m4$trade.1997)
edges.m4$trade.1997[edges.m4$trade.1997=="#N/A"]<-""
edges.m4$trade.1997<- as.numeric(edges.m4$trade.1997)

edges.m4$trade.1996<- as.character(edges.m4$trade.1996)
edges.m4$trade.1996[edges.m4$trade.1996=="#N/A"]<-""
edges.m4$trade.1996<- as.numeric(edges.m4$trade.1996)

edges.m4$trade.1995<- as.character(edges.m4$trade.1995)
edges.m4$trade.1995[edges.m4$trade.1995=="#N/A"]<-""
edges.m4$trade.1995<- as.numeric(edges.m4$trade.1995)

edges.m4$trade.1994<- as.character(edges.m4$trade.1994)
edges.m4$trade.1994[edges.m4$trade.1994=="#N/A"]<-""
edges.m4$trade.1994<- as.numeric(edges.m4$trade.1994)

edges.m4$trade.1993<- as.character(edges.m4$trade.1993)
edges.m4$trade.1993[edges.m4$trade.1993=="#N/A"]<-""
edges.m4$trade.1993<- as.numeric(edges.m4$trade.1993)

edges.m4$trade.1992<- as.character(edges.m4$trade.1992)
edges.m4$trade.1992[edges.m4$trade.1992=="#N/A"]<-""
edges.m4$trade.1992<- as.numeric(edges.m4$trade.1992)

edges.m4$trade.1991<- as.character(edges.m4$trade.1991)
edges.m4$trade.1991[edges.m4$trade.1991=="#N/A"]<-""
edges.m4$trade.1991<- as.numeric(edges.m4$trade.1991)

igraph.edges.m4<- graph_from_data_frame(edges.m4,directed=T)

#2016
ref2016.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2016.flow"))
ref2015.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2015.flow"))
defense.alliance.2016.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2016"))
trade.2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2014"))
riv.strategic.2016.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2016"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2016.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2016.gradient"))
pts.2015.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.2015.gradient"))
income.2015.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2015.gradient"))
armsflows.inverse.2016.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2016"))
immigrant.pop.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2010"))


#2015
ref2015.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2015.flow"))
ref2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2014.flow"))
defense.alliance.2015.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2015"))
trade.2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2014"))
riv.strategic.2015.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2015"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2015.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2015.gradient"))
pts.2015.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.2015.gradient"))
income.2015.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2015.gradient"))
armsflows.inverse.2015.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2015"))
immigrant.pop.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2010"))

#2014
ref2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2014.flow"))
ref2013.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2013.flow"))
defense.alliance.2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2014"))
trade.2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2014"))
riv.strategic.2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2014"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2014.gradient"))
pts.2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.2014.gradient"))
income.2014.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2014.gradient"))
armsflows.inverse.2014.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2014"))
immigrant.pop.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2010"))

#2013
ref2013.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2013.flow"))
ref2012.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2012.flow"))
defense.alliance.2013.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2013"))
trade.2013.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2013"))
riv.strategic.2013.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2013"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2013.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2013.gradient"))
pts.2013.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.2013.gradient"))
income.2013.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2013.gradient"))
armsflows.inverse.2013.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2013"))
immigrant.pop.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2010"))

#2012
ref2012.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2012.flow"))
ref2011.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2011.flow"))
defense.alliance.2012.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2012"))
trade.2012.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2012"))
riv.strategic.2012.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2012"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2012.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2012.gradient"))
pts.2012.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.2012.gradient"))
income.2012.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2012.gradient"))
armsflows.inverse.2012.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2012"))
immigrant.pop.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2010"))

#2011
ref2011.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2011.flow"))
ref2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2010.flow"))
defense.alliance.2011.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2011"))
trade.2011.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2011"))
riv.strategic.2011.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2011"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2011.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2011.gradient"))
pts.2011.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.2011.gradient"))
income.2011.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2011.gradient"))
armsflows.inverse.2011.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2011"))
immigrant.pop.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2010"))

#2010
ref2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2010.flow"))
ref2009.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2009.flow"))
defense.alliance.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2010"))
trade.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2010"))
riv.strategic.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2010"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2010.gradient"))
pts.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2010"))
income.2010.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2010.gradient"))
armsflows.inverse.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2010"))
immigrant.pop.2010.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2010"))

#2009
ref2009.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2009.flow"))
ref2008.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2008.flow"))
defense.alliance.2009.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2009"))
trade.2009.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2009"))
riv.strategic.2009.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2009"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2009.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2009.gradient"))
pts.2009.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2009"))
income.2009.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2009.gradient"))
armsflows.inverse.2009.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2009"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2008
ref2008.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2008.flow"))
ref2007.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2007.flow"))
defense.alliance.2008.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2008"))
trade.2008.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2008"))
riv.strategic.2008.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2008"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2008.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2008.gradient"))
pts.2008.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2008"))
income.2008.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2008.gradient"))
armsflows.inverse.2008.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2008"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2007
ref2007.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2007.flow"))
ref2006.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2006.flow"))
defense.alliance.2007.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2007"))
trade.2007.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2007"))
riv.strategic.2007.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2007"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2007.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2007.gradient"))
pts.2007.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2007"))
income.2007.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2007.gradient"))
armsflows.inverse.2007.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2007"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2006
ref2006.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2006.flow"))
ref2005.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2005.flow"))
defense.alliance.2006.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2006"))
trade.2006.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2006"))
riv.strategic.2006.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2006"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2006.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2006.gradient"))
pts.2006.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2006"))
income.2006.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2006.gradient"))
armsflows.inverse.2006.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2006"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2005
ref2005.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2005.flow"))
ref2004.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2004.flow"))
defense.alliance.2005.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2005"))
trade.2005.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2005"))
riv.strategic.2005.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2005"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2005.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2005.gradient"))
pts.2005.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2005"))
income.2005.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2005.gradient"))
armsflows.inverse.2005.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2005"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2004
ref2004.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2004.flow"))
ref2003.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2003.flow"))
defense.alliance.2004.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2004"))
trade.2004.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2004"))
riv.strategic.2004.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2004"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2004.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2004.gradient"))
pts.2004.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2004"))
income.2004.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2004.gradient"))
armsflows.inverse.2004.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2004"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2003
ref2003.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2003.flow"))
ref2002.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2002.flow"))
defense.alliance.2003.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2003"))
trade.2003.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2003"))
riv.strategic.2003.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2003"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2003.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2003.gradient"))
pts.2003.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2003"))
income.2003.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2003.gradient"))
armsflows.inverse.2003.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2003"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2002
ref2002.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2002.flow"))
ref2001.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2001.flow"))
defense.alliance.2002.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2002"))
trade.2002.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2002"))
riv.strategic.2002.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2002"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2002.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2002.gradient"))
pts.2002.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2002"))
income.2002.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2002.gradient"))
armsflows.inverse.2002.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2002"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2001
ref2001.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2001.flow"))
ref2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2000.flow"))
defense.alliance.2001.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2001"))
trade.2001.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2001"))
riv.strategic.2001.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2001"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2001.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2001.gradient"))
pts.2001.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2001"))
income.2001.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2001.gradient"))
armsflows.inverse.2001.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2001"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#2000
ref2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref2000.flow"))
ref1999.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1999.flow"))
defense.alliance.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.2000"))
trade.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.2000"))
riv.strategic.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.2000"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.2000.gradient"))
pts.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.2000"))
income.2000.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.2000.gradient"))
armsflows.inverse.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.2000"))
immigrant.pop.2000.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.2000"))

#1999
ref1999.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1999.flow"))
ref1998.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1998.flow"))
defense.alliance.1999.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1999"))
trade.1999.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1999"))
riv.strategic.1999.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1999"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1999.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1999.gradient"))
pts.1999.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1999"))
income.1999.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1999.gradient"))
armsflows.inverse.1999.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1999"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))

#1998
ref1998.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1998.flow"))
ref1997.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1997.flow"))
defense.alliance.1998.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1998"))
trade.1998.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1998"))
riv.strategic.1998.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1998"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1998.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1998.gradient"))
pts.1998.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1998"))
income.1998.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1998.gradient"))
armsflows.inverse.1998.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1998"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))

#1997
ref1997.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1997.flow"))
ref1996.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1996.flow"))
defense.alliance.1997.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1997"))
trade.1997.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1997"))
riv.strategic.1997.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1997"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1997.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1997.gradient"))
pts.1997.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1997"))
income.1997.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1997.gradient"))
armsflows.inverse.1997.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1997"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))

#1996
ref1996.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1996.flow"))
ref1995.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1995.flow"))
defense.alliance.1996.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1996"))
trade.1996.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1996"))
riv.strategic.1996.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1996"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1996.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1996.gradient"))
pts.1996.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1996"))
income.1996.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1996.gradient"))
armsflows.inverse.1996.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1996"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))

#1995
ref1995.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1995.flow"))
ref1994.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1994.flow"))
defense.alliance.1995.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1995"))
trade.1995.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1995"))
riv.strategic.1995.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1995"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1995.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1995.gradient"))
pts.1995.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1995"))
income.1995.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1995.gradient"))
armsflows.inverse.1995.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1995"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))

#1994
ref1994.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1994.flow"))
ref1993.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1993.flow"))
defense.alliance.1994.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1994"))
trade.1994.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1994"))
riv.strategic.1994.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1994"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1994.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1994.gradient"))
pts.1994.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1994"))
income.1994.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1994.gradient"))
armsflows.inverse.1994.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1994"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))

#1993
ref1993.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1993.flow"))
ref1992.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1992.flow"))
defense.alliance.1993.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1993"))
trade.1993.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1993"))
riv.strategic.1993.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1993"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1993.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1993.gradient"))
pts.1993.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1993"))
income.1993.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1993.gradient"))
armsflows.inverse.1993.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1993"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))

#1992
ref1992.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1992.flow"))
ref1991.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1991.flow"))
defense.alliance.1992.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1992"))
trade.1992.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1992"))
riv.strategic.1992.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1992"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1992.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1992.gradient"))
pts.1992.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1992"))
income.1992.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1992.gradient"))
armsflows.inverse.1992.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1992"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))

#1991
ref1991.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1991.flow"))
ref1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                            attr="ref1990.flow"))
defense.alliance.1991.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                          attr="alliance.defense.1991"))
trade.1991.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                               attr="trade.1991"))
riv.strategic.1991.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="riv.strategic.1991"))
contiguity.any.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="contiguity.any"))
polyarchy.1991.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                   attr="polyarchy.additive.1991.gradient"))
pts.1991.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                             attr="pts.gradient.1991"))
income.1991.gradient.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                         attr="gdppc.1991.gradient"))
armsflows.inverse.1991.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                           attr="arms.inverse.1991"))
immigrant.pop.1990.mat<- as.matrix(as_adjacency_matrix(igraph.edges.m4,type="both",names=T,
                                                       attr="immigrant.population.1990"))


#####################################################
# MRQAPs
#####################################################

nl.2016.rivagg<-netlm(ref2016.mat,           # Dependent variable/network
                      list(ref2015.mat,
                           polyarchy.2016.mat, 
                           armsflows.inverse.2016.mat,   
                           riv.strategic.2016.mat,
                           contiguity.any.mat,
                           income.2015.gradient.mat,
                           pts.2015.mat,   
                           defense.alliance.2016.mat,
                           trade.2014.mat,   
                           immigrant.pop.2010.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value") 

nl.2015.rivagg<-netlm(ref2015.mat,           
                      list(ref2014.mat,
                           polyarchy.2015.mat, 
                           armsflows.inverse.2015.mat,   
                           riv.strategic.2015.mat,
                           contiguity.any.mat,
                           income.2015.gradient.mat, 
                           pts.2015.mat,   
                           defense.alliance.2015.mat,  
                           trade.2014.mat,   
                           immigrant.pop.2010.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value") 


nl.2014.rivagg<-netlm(ref2014.mat,           
                      list(ref2013.mat,
                           polyarchy.2014.mat, 
                           armsflows.inverse.2014.mat,   
                           riv.strategic.2014.mat,
                           contiguity.any.mat,
                           income.2014.gradient.mat, 
                           pts.2014.mat,    
                           defense.alliance.2014.mat,  
                           trade.2014.mat, 
                           immigrant.pop.2010.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value") 

nl.2013.rivagg<-netlm(ref2013.mat,           
                      list(ref2012.mat,
                           polyarchy.2013.mat, 
                           armsflows.inverse.2013.mat,   
                           riv.strategic.2013.mat,
                           contiguity.any.mat,
                           income.2013.gradient.mat, 
                           pts.2013.mat,   
                           defense.alliance.2013.mat,  
                           trade.2013.mat, 
                           immigrant.pop.2010.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value") 


nl.2012.rivagg<-netlm(ref2012.mat,           
                      list(ref2011.mat,
                           polyarchy.2012.mat,
                           armsflows.inverse.2012.mat,   
                           riv.strategic.2012.mat,
                           contiguity.any.mat,
                           income.2012.gradient.mat, 
                           pts.2012.mat,   
                           defense.alliance.2012.mat,  
                           trade.2012.mat, 
                           immigrant.pop.2010.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value") 


nl.2011.rivagg<-netlm(ref2011.mat,           
                      list(ref2010.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2011.mat, 
                           armsflows.inverse.2011.mat,   
                           riv.strategic.2011.mat,
                           contiguity.any.mat,
                           income.2011.gradient.mat, 
                           pts.2011.mat,   
                           defense.alliance.2011.mat,  
                           trade.2011.mat, 
                           immigrant.pop.2010.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2010.rivagg<-netlm(ref2010.mat,           
                      list(ref2009.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2010.mat, 
                           armsflows.inverse.2010.mat,   
                           riv.strategic.2010.mat,
                           contiguity.any.mat,
                           income.2010.gradient.mat, 
                           pts.2010.mat,   
                           defense.alliance.2010.mat,  
                           trade.2010.mat, 
                           immigrant.pop.2010.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2009.rivagg<-netlm(ref2009.mat,           
                      list(ref2008.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2009.mat, 
                           armsflows.inverse.2009.mat,   
                           riv.strategic.2009.mat,
                           contiguity.any.mat,
                           income.2009.gradient.mat, 
                           pts.2009.mat,   
                           defense.alliance.2009.mat,  
                           trade.2009.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2008.rivagg<-netlm(ref2008.mat,           
                      list(ref2007.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2008.mat, 
                           armsflows.inverse.2008.mat,   
                           riv.strategic.2008.mat,
                           contiguity.any.mat,
                           income.2008.gradient.mat, 
                           pts.2008.mat,   
                           defense.alliance.2008.mat,  
                           trade.2008.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2007.rivagg<-netlm(ref2007.mat,           
                      list(ref2006.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2007.mat, 
                           armsflows.inverse.2007.mat,   
                           riv.strategic.2007.mat,
                           contiguity.any.mat,
                           income.2007.gradient.mat, 
                           pts.2007.mat,   
                           defense.alliance.2007.mat,  
                           trade.2007.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2006.rivagg<-netlm(ref2006.mat,           
                      list(ref2005.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2006.mat, 
                           armsflows.inverse.2006.mat,   
                           riv.strategic.2006.mat,
                           contiguity.any.mat,
                           income.2006.gradient.mat, 
                           pts.2006.mat,   
                           defense.alliance.2006.mat,  
                           trade.2006.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2005.rivagg<-netlm(ref2005.mat,           
                      list(ref2004.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2005.mat, 
                           armsflows.inverse.2005.mat,   
                           riv.strategic.2005.mat,
                           contiguity.any.mat,
                           income.2005.gradient.mat, 
                           pts.2005.mat,   
                           defense.alliance.2005.mat,  
                           trade.2005.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2004.rivagg<-netlm(ref2004.mat,           
                      list(ref2003.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2004.mat, 
                           armsflows.inverse.2004.mat,   
                           riv.strategic.2004.mat,
                           contiguity.any.mat,
                           income.2004.gradient.mat, 
                           pts.2004.mat,   
                           defense.alliance.2004.mat,  
                           trade.2004.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2003.rivagg<-netlm(ref2003.mat,           
                      list(ref2002.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2003.mat, 
                           armsflows.inverse.2003.mat,   
                           riv.strategic.2003.mat,
                           contiguity.any.mat,
                           income.2003.gradient.mat, 
                           pts.2003.mat,   
                           defense.alliance.2003.mat,  
                           trade.2003.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2002.rivagg<-netlm(ref2002.mat,           
                      list(ref2001.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2002.mat, 
                           armsflows.inverse.2002.mat,   
                           riv.strategic.2002.mat,
                           contiguity.any.mat,
                           income.2002.gradient.mat, 
                           pts.2002.mat,   
                           defense.alliance.2002.mat,  
                           trade.2002.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2001.rivagg<-netlm(ref2001.mat,           
                      list(ref2000.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2001.mat, 
                           armsflows.inverse.2001.mat,   
                           riv.strategic.2001.mat,
                           contiguity.any.mat,
                           income.2001.gradient.mat, 
                           pts.2001.mat,   
                           defense.alliance.2001.mat,  
                           trade.2001.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.2000.rivagg<-netlm(ref2000.mat,           
                      list(ref1999.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.2000.mat, 
                           armsflows.inverse.2000.mat,   
                           riv.strategic.2000.mat,
                           contiguity.any.mat,
                           income.2000.gradient.mat, 
                           pts.2000.mat,   
                           defense.alliance.2000.mat,  
                           trade.2000.mat, 
                           immigrant.pop.2000.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1999.rivagg<-netlm(ref1999.mat,           
                      list(ref1998.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1999.mat, 
                           armsflows.inverse.1999.mat,   
                           riv.strategic.1999.mat,
                           contiguity.any.mat,
                           income.1999.gradient.mat, 
                           pts.1999.mat,   
                           defense.alliance.1999.mat,  
                           trade.1999.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1998.rivagg<-netlm(ref1998.mat,           
                      list(ref1997.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1998.mat,
                           armsflows.inverse.1998.mat,   
                           riv.strategic.1998.mat,
                           contiguity.any.mat,
                           income.1998.gradient.mat, 
                           pts.1998.mat,   
                           defense.alliance.1998.mat,  
                           trade.1998.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1997.rivagg<-netlm(ref1997.mat,           
                      list(ref1996.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1997.mat, 
                           armsflows.inverse.1997.mat,   
                           riv.strategic.1997.mat,
                           contiguity.any.mat,
                           income.1997.gradient.mat, 
                           pts.1997.mat,   
                           defense.alliance.1997.mat,  
                           trade.1997.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1996.rivagg<-netlm(ref1996.mat,           
                      list(ref1995.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1996.mat, 
                           armsflows.inverse.1996.mat,   
                           riv.strategic.1996.mat,
                           contiguity.any.mat,
                           income.1996.gradient.mat, 
                           pts.1996.mat,   
                           defense.alliance.1996.mat,  
                           trade.1996.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1995.rivagg<-netlm(ref1995.mat,           
                      list(ref1994.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1995.mat, 
                           armsflows.inverse.1995.mat,   
                           riv.strategic.1995.mat,
                           contiguity.any.mat,
                           income.1995.gradient.mat, 
                           pts.1995.mat,   
                           defense.alliance.1995.mat,  
                           trade.1995.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1994.rivagg<-netlm(ref1994.mat,           
                      list(ref1993.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1994.mat, 
                           armsflows.inverse.1994.mat,   
                           riv.strategic.1994.mat,
                           contiguity.any.mat,
                           income.1994.gradient.mat, 
                           pts.1994.mat,   
                           defense.alliance.1994.mat,  
                           trade.1994.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1993.rivagg<-netlm(ref1993.mat,           
                      list(ref1992.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1993.mat, 
                           armsflows.inverse.1993.mat,   
                           riv.strategic.1993.mat,
                           contiguity.any.mat,
                           income.1993.gradient.mat, 
                           pts.1993.mat,   
                           defense.alliance.1993.mat,  
                           trade.1993.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1992.rivagg<-netlm(ref1992.mat,           
                      list(ref1991.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1992.mat, 
                           armsflows.inverse.1992.mat,   
                           riv.strategic.1992.mat,
                           contiguity.any.mat,
                           income.1992.gradient.mat, 
                           pts.1992.mat,   
                           defense.alliance.1992.mat,  
                           trade.1992.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

nl.1991.rivagg<-netlm(ref1991.mat,           
                      list(ref1990.mat,   # Add, account for no South Sudan (missing value)
                           polyarchy.1991.mat, 
                           armsflows.inverse.1991.mat,   
                           riv.strategic.1991.mat,
                           contiguity.any.mat,
                           income.1991.gradient.mat, 
                           pts.1991.mat,   
                           defense.alliance.1991.mat,  
                           trade.1991.mat, 
                           immigrant.pop.1990.mat
                      ), # List the independent variables/networks
                      reps=1000, test.statistic="t-value")

a<- cbind(nl.1991.rivagg$coefficients,nl.1991.rivagg$pgreqabs,
          nl.1992.rivagg$coefficients,nl.1992.rivagg$pgreqabs,
          nl.1993.rivagg$coefficients,nl.1993.rivagg$pgreqabs,
          nl.1994.rivagg$coefficients,nl.1994.rivagg$pgreqabs,
          nl.1995.rivagg$coefficients,nl.1995.rivagg$pgreqabs,
          nl.1996.rivagg$coefficients,nl.1996.rivagg$pgreqabs,
          nl.1997.rivagg$coefficients,nl.1997.rivagg$pgreqabs,
          nl.1998.rivagg$coefficients,nl.1998.rivagg$pgreqabs,
          nl.1999.rivagg$coefficients,nl.1999.rivagg$pgreqabs,
          nl.2000.rivagg$coefficients,nl.2000.rivagg$pgreqabs,
          nl.2001.rivagg$coefficients,nl.2001.rivagg$pgreqabs,
          nl.2002.rivagg$coefficients,nl.2002.rivagg$pgreqabs,
          nl.2003.rivagg$coefficients,nl.2003.rivagg$pgreqabs,
          nl.2004.rivagg$coefficients,nl.2004.rivagg$pgreqabs,
          nl.2005.rivagg$coefficients,nl.2005.rivagg$pgreqabs,
          nl.2006.rivagg$coefficients,nl.2006.rivagg$pgreqabs,
          nl.2007.rivagg$coefficients,nl.2007.rivagg$pgreqabs,
          nl.2008.rivagg$coefficients,nl.2008.rivagg$pgreqabs,
          nl.2009.rivagg$coefficients,nl.2009.rivagg$pgreqabs,
          nl.2010.rivagg$coefficients,nl.2010.rivagg$pgreqabs,
          nl.2011.rivagg$coefficients,nl.2011.rivagg$pgreqabs,
          nl.2012.rivagg$coefficients,nl.2012.rivagg$pgreqabs, 
          nl.2013.rivagg$coefficients,nl.2013.rivagg$pgreqabs,
          nl.2014.rivagg$coefficients,nl.2014.rivagg$pgreqabs,
          nl.2015.rivagg$coefficients,nl.2015.rivagg$pgreqabs, 
          nl.2016.rivagg$coefficients,nl.2016.rivagg$pgreqabs)

###Use t-statistic for HCA
b<- cbind(nl.1991.rivagg$tstat,
          nl.1992.rivagg$tstat,
          nl.1993.rivagg$tstat,
          nl.1994.rivagg$tstat,
          nl.1995.rivagg$tstat,
          nl.1996.rivagg$tstat,
          nl.1997.rivagg$tstat,
          nl.1998.rivagg$tstat,
          nl.1999.rivagg$tstat,
          nl.2000.rivagg$tstat,
          nl.2001.rivagg$tstat,
          nl.2002.rivagg$tstat,
          nl.2003.rivagg$tstat,
          nl.2004.rivagg$tstat,
          nl.2005.rivagg$tstat,
          nl.2006.rivagg$tstat,
          nl.2007.rivagg$tstat,
          nl.2008.rivagg$tstat,
          nl.2009.rivagg$tstat,
          nl.2010.rivagg$tstat,
          nl.2011.rivagg$tstat,
          nl.2012.rivagg$tstat, 
          nl.2013.rivagg$tstat,
          nl.2014.rivagg$tstat,
          nl.2015.rivagg$tstat, 
          nl.2016.rivagg$tstat)

########################################################
### Cluster Analysis
########################################################
library(corrr)
library(ggraph)
library(rlang)
library(naniar)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(ggplot2)
library(fmsb)
library(sjmisc)
library(ggpubr)

b.nonvector<- b[-c(1),]
b.nonvector<- b.nonvector %>%
  correlate() %>%
  select(-rowname)
diag(b.nonvector)<- 1
dissim<- as.dist(1-b.nonvector)

plot(hclust(dissim, method = "centroid"), labels = c("1991 Refugee Flows","1992 Refugee Flows",
                                                     "1993 Refugee Flows","1994 Refugee Flows",
                                                     "1995 Refugee Flows","1996 Refugee Flows",
                                                     "1997 Refugee Flows","1998 Refugee Flows",
                                                     "1999 Refugee Flows","2000 Refugee Flows",
                                                     "2001 Refugee Flows","2002 Refugee Flows",
                                                     "2003 Refugee Flows","2004 Refugee Flows",
                                                     "2005 Refugee Flows","2006 Refugee Flows",
                                                     "2007 Refugee Flows","2008 Refugee Flows",
                                                     "2009 Refugee Flows","2010 Refugee Flows",
                                                     "2011 Refugee Flows","2012 Refugee Flows",
                                                     "2013 Refugee Flows","2014 Refugee Flows",
                                                     "2015 Refugee Flows","2016 Refugee Flows"))



fit <- cmdscale(dissim,eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
mds<- data.frame(fit$points[,1:2])
colnames(mds) <- c("Dim.1", "Dim.2")
clust<- kmeans(mds,4)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)

rownames(mds)<- c("1991","1992",
                  "1993","1994",
                  "1995","1996",
                  "1997","1998",
                  "1999","2000",
                  "2001","2002",
                  "2003","2004",
                  "2005","2006",
                  "2007","2008",
                  "2009","2010",
                  "2011","2012",
                  "2013","2014",
                  "2015","2016")


ggscatter(mds, x = "Dim.1", y = "Dim.2", color = "groups",
          label = rownames(mds),
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)


prior.ref<- rbind(nl.1991.rivagg$tstat[2],
                  nl.1992.rivagg$tstat[2],
                  nl.1993.rivagg$tstat[2],
                  nl.1994.rivagg$tstat[2],
                  nl.1995.rivagg$tstat[2],
                  nl.1996.rivagg$tstat[2],
                  nl.1997.rivagg$tstat[2],
                  nl.1998.rivagg$tstat[2],
                  nl.1999.rivagg$tstat[2],
                  nl.2000.rivagg$tstat[2],
                  nl.2001.rivagg$tstat[2],
                  nl.2002.rivagg$tstat[2],
                  nl.2003.rivagg$tstat[2],
                  nl.2004.rivagg$tstat[2],
                  nl.2005.rivagg$tstat[2],
                  nl.2006.rivagg$tstat[2],
                  nl.2007.rivagg$tstat[2],
                  nl.2008.rivagg$tstat[2],
                  nl.2009.rivagg$tstat[2],
                  nl.2010.rivagg$tstat[2],
                  nl.2011.rivagg$tstat[2],
                  nl.2012.rivagg$tstat[2], 
                  nl.2013.rivagg$tstat[2],
                  nl.2014.rivagg$tstat[2],
                  nl.2015.rivagg$tstat[2], 
                  nl.2016.rivagg$tstat[2])

regime.type<- rbind(nl.1991.rivagg$tstat[3],
                    nl.1992.rivagg$tstat[3],
                    nl.1993.rivagg$tstat[3],
                    nl.1994.rivagg$tstat[3],
                    nl.1995.rivagg$tstat[3],
                    nl.1996.rivagg$tstat[3],
                    nl.1997.rivagg$tstat[3],
                    nl.1998.rivagg$tstat[3],
                    nl.1999.rivagg$tstat[3],
                    nl.2000.rivagg$tstat[3],
                    nl.2001.rivagg$tstat[3],
                    nl.2002.rivagg$tstat[3],
                    nl.2003.rivagg$tstat[3],
                    nl.2004.rivagg$tstat[3],
                    nl.2005.rivagg$tstat[3],
                    nl.2006.rivagg$tstat[3],
                    nl.2007.rivagg$tstat[3],
                    nl.2008.rivagg$tstat[3],
                    nl.2009.rivagg$tstat[3],
                    nl.2010.rivagg$tstat[3],
                    nl.2011.rivagg$tstat[3],
                    nl.2012.rivagg$tstat[3], 
                    nl.2013.rivagg$tstat[3],
                    nl.2014.rivagg$tstat[3],
                    nl.2015.rivagg$tstat[3], 
                    nl.2016.rivagg$tstat[3])

af.inverse<- rbind(nl.1991.rivagg$tstat[4],
                   nl.1992.rivagg$tstat[4],
                   nl.1993.rivagg$tstat[4],
                   nl.1994.rivagg$tstat[4],
                   nl.1995.rivagg$tstat[4],
                   nl.1996.rivagg$tstat[4],
                   nl.1997.rivagg$tstat[4],
                   nl.1998.rivagg$tstat[4],
                   nl.1999.rivagg$tstat[4],
                   nl.2000.rivagg$tstat[4],
                   nl.2001.rivagg$tstat[4],
                   nl.2002.rivagg$tstat[4],
                   nl.2003.rivagg$tstat[4],
                   nl.2004.rivagg$tstat[4],
                   nl.2005.rivagg$tstat[4],
                   nl.2006.rivagg$tstat[4],
                   nl.2007.rivagg$tstat[4],
                   nl.2008.rivagg$tstat[4],
                   nl.2009.rivagg$tstat[4],
                   nl.2010.rivagg$tstat[4],
                   nl.2011.rivagg$tstat[4],
                   nl.2012.rivagg$tstat[4], 
                   nl.2013.rivagg$tstat[4],
                   nl.2014.rivagg$tstat[4],
                   nl.2015.rivagg$tstat[4], 
                   nl.2016.rivagg$tstat[4])

rivalry<- rbind(nl.1991.rivagg$tstat[5],
                nl.1992.rivagg$tstat[5],
                nl.1993.rivagg$tstat[5],
                nl.1994.rivagg$tstat[5],
                nl.1995.rivagg$tstat[5],
                nl.1996.rivagg$tstat[5],
                nl.1997.rivagg$tstat[5],
                nl.1998.rivagg$tstat[5],
                nl.1999.rivagg$tstat[5],
                nl.2000.rivagg$tstat[5],
                nl.2001.rivagg$tstat[5],
                nl.2002.rivagg$tstat[5],
                nl.2003.rivagg$tstat[5],
                nl.2004.rivagg$tstat[5],
                nl.2005.rivagg$tstat[5],
                nl.2006.rivagg$tstat[5],
                nl.2007.rivagg$tstat[5],
                nl.2008.rivagg$tstat[5],
                nl.2009.rivagg$tstat[5],
                nl.2010.rivagg$tstat[5],
                nl.2011.rivagg$tstat[5],
                nl.2012.rivagg$tstat[5], 
                nl.2013.rivagg$tstat[5],
                nl.2014.rivagg$tstat[5],
                nl.2015.rivagg$tstat[5], 
                nl.2016.rivagg$tstat[5])

cont<- rbind(nl.1991.rivagg$tstat[6],
             nl.1992.rivagg$tstat[6],
             nl.1993.rivagg$tstat[6],
             nl.1994.rivagg$tstat[6],
             nl.1995.rivagg$tstat[6],
             nl.1996.rivagg$tstat[6],
             nl.1997.rivagg$tstat[6],
             nl.1998.rivagg$tstat[6],
             nl.1999.rivagg$tstat[6],
             nl.2000.rivagg$tstat[6],
             nl.2001.rivagg$tstat[6],
             nl.2002.rivagg$tstat[6],
             nl.2003.rivagg$tstat[6],
             nl.2004.rivagg$tstat[6],
             nl.2005.rivagg$tstat[6],
             nl.2006.rivagg$tstat[6],
             nl.2007.rivagg$tstat[6],
             nl.2008.rivagg$tstat[6],
             nl.2009.rivagg$tstat[6],
             nl.2010.rivagg$tstat[6],
             nl.2011.rivagg$tstat[6],
             nl.2012.rivagg$tstat[6], 
             nl.2013.rivagg$tstat[6],
             nl.2014.rivagg$tstat[6],
             nl.2015.rivagg$tstat[6], 
             nl.2016.rivagg$tstat[6])

income<- rbind(nl.1991.rivagg$tstat[7],
               nl.1992.rivagg$tstat[7],
               nl.1993.rivagg$tstat[7],
               nl.1994.rivagg$tstat[7],
               nl.1995.rivagg$tstat[7],
               nl.1996.rivagg$tstat[7],
               nl.1997.rivagg$tstat[7],
               nl.1998.rivagg$tstat[7],
               nl.1999.rivagg$tstat[7],
               nl.2000.rivagg$tstat[7],
               nl.2001.rivagg$tstat[7],
               nl.2002.rivagg$tstat[7],
               nl.2003.rivagg$tstat[7],
               nl.2004.rivagg$tstat[7],
               nl.2005.rivagg$tstat[7],
               nl.2006.rivagg$tstat[7],
               nl.2007.rivagg$tstat[7],
               nl.2008.rivagg$tstat[7],
               nl.2009.rivagg$tstat[7],
               nl.2010.rivagg$tstat[7],
               nl.2011.rivagg$tstat[7],
               nl.2012.rivagg$tstat[7], 
               nl.2013.rivagg$tstat[7],
               nl.2014.rivagg$tstat[7],
               nl.2015.rivagg$tstat[7], 
               nl.2016.rivagg$tstat[7])

pts<- rbind(nl.1991.rivagg$tstat[8],
            nl.1992.rivagg$tstat[8],
            nl.1993.rivagg$tstat[8],
            nl.1994.rivagg$tstat[8],
            nl.1995.rivagg$tstat[8],
            nl.1996.rivagg$tstat[8],
            nl.1997.rivagg$tstat[8],
            nl.1998.rivagg$tstat[8],
            nl.1999.rivagg$tstat[8],
            nl.2000.rivagg$tstat[8],
            nl.2001.rivagg$tstat[8],
            nl.2002.rivagg$tstat[8],
            nl.2003.rivagg$tstat[8],
            nl.2004.rivagg$tstat[8],
            nl.2005.rivagg$tstat[8],
            nl.2006.rivagg$tstat[8],
            nl.2007.rivagg$tstat[8],
            nl.2008.rivagg$tstat[8],
            nl.2009.rivagg$tstat[8],
            nl.2010.rivagg$tstat[8],
            nl.2011.rivagg$tstat[8],
            nl.2012.rivagg$tstat[8], 
            nl.2013.rivagg$tstat[8],
            nl.2014.rivagg$tstat[8],
            nl.2015.rivagg$tstat[8], 
            nl.2016.rivagg$tstat[8])

def<- rbind(nl.1991.rivagg$tstat[9],
            nl.1992.rivagg$tstat[9],
            nl.1993.rivagg$tstat[9],
            nl.1994.rivagg$tstat[9],
            nl.1995.rivagg$tstat[9],
            nl.1996.rivagg$tstat[9],
            nl.1997.rivagg$tstat[9],
            nl.1998.rivagg$tstat[9],
            nl.1999.rivagg$tstat[9],
            nl.2000.rivagg$tstat[9],
            nl.2001.rivagg$tstat[9],
            nl.2002.rivagg$tstat[9],
            nl.2003.rivagg$tstat[9],
            nl.2004.rivagg$tstat[9],
            nl.2005.rivagg$tstat[9],
            nl.2006.rivagg$tstat[9],
            nl.2007.rivagg$tstat[9],
            nl.2008.rivagg$tstat[9],
            nl.2009.rivagg$tstat[9],
            nl.2010.rivagg$tstat[9],
            nl.2011.rivagg$tstat[9],
            nl.2012.rivagg$tstat[9], 
            nl.2013.rivagg$tstat[9],
            nl.2014.rivagg$tstat[9],
            nl.2015.rivagg$tstat[9], 
            nl.2016.rivagg$tstat[9])

tr<- rbind(nl.1991.rivagg$tstat[10],
           nl.1992.rivagg$tstat[10],
           nl.1993.rivagg$tstat[10],
           nl.1994.rivagg$tstat[10],
           nl.1995.rivagg$tstat[10],
           nl.1996.rivagg$tstat[10],
           nl.1997.rivagg$tstat[10],
           nl.1998.rivagg$tstat[10],
           nl.1999.rivagg$tstat[10],
           nl.2000.rivagg$tstat[10],
           nl.2001.rivagg$tstat[10],
           nl.2002.rivagg$tstat[10],
           nl.2003.rivagg$tstat[10],
           nl.2004.rivagg$tstat[10],
           nl.2005.rivagg$tstat[10],
           nl.2006.rivagg$tstat[10],
           nl.2007.rivagg$tstat[10],
           nl.2008.rivagg$tstat[10],
           nl.2009.rivagg$tstat[10],
           nl.2010.rivagg$tstat[10],
           nl.2011.rivagg$tstat[10],
           nl.2012.rivagg$tstat[10], 
           nl.2013.rivagg$tstat[10],
           nl.2014.rivagg$tstat[10],
           nl.2015.rivagg$tstat[10], 
           nl.2016.rivagg$tstat[10])

imm<- rbind(nl.1991.rivagg$tstat[11],
            nl.1992.rivagg$tstat[11],
            nl.1993.rivagg$tstat[11],
            nl.1994.rivagg$tstat[11],
            nl.1995.rivagg$tstat[11],
            nl.1996.rivagg$tstat[11],
            nl.1997.rivagg$tstat[11],
            nl.1998.rivagg$tstat[11],
            nl.1999.rivagg$tstat[11],
            nl.2000.rivagg$tstat[11],
            nl.2001.rivagg$tstat[11],
            nl.2002.rivagg$tstat[11],
            nl.2003.rivagg$tstat[11],
            nl.2004.rivagg$tstat[11],
            nl.2005.rivagg$tstat[11],
            nl.2006.rivagg$tstat[11],
            nl.2007.rivagg$tstat[11],
            nl.2008.rivagg$tstat[11],
            nl.2009.rivagg$tstat[11],
            nl.2010.rivagg$tstat[11],
            nl.2011.rivagg$tstat[11],
            nl.2012.rivagg$tstat[11], 
            nl.2013.rivagg$tstat[11],
            nl.2014.rivagg$tstat[11],
            nl.2015.rivagg$tstat[11], 
            nl.2016.rivagg$tstat[11])

c<- as.data.frame(cbind(prior.ref,regime.type,af.inverse,rivalry,
                        cont,income,pts,def,tr,imm))

variables<- c %>%
  correlate() %>%
  select(-rowname)
diag(variables)<- 1
dissim.var<- as.dist(1-variables)

fit.v <- cmdscale(dissim.var,eig=TRUE, k=2)
x.v <- fit.v$points[,1]
y.v <- fit.v$points[,2]
mds.v<- data.frame(fit.v$points[,1:2])
colnames(mds.v) <- c("Dim.1", "Dim.2")
clust.v<- kmeans(mds.v,4)$cluster %>%
  as.factor()
mds.v <- mds.v %>%
  mutate(groups = clust.v)

rownames(mds.v)<- c("Prior Refugees",
                    "Democracy Gain",
                    "Arms Flows",
                    "Strategic Rivalry",
                    "Contiguity",
                    "Wage Gain",
                    "Security Gain",
                    "Alliance",
                    "Trade",
                    "Immigrant Population")


ggscatter(mds.v, x = "Dim.1", y = "Dim.2", color = "groups",
          label = rownames(mds.v),
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)




#### Reverse engineer MR-QAP t-statistics
#### qt(1-p-value/2,n-1) where n-1=degrees of freedom

prior.ref.p<- rbind(nl.1991.rivagg$pgreqabs[2],
                    nl.1992.rivagg$pgreqabs[2],
                    nl.1993.rivagg$pgreqabs[2],
                    nl.1994.rivagg$pgreqabs[2],
                    nl.1995.rivagg$pgreqabs[2],
                    nl.1996.rivagg$pgreqabs[2],
                    nl.1997.rivagg$pgreqabs[2],
                    nl.1998.rivagg$pgreqabs[2],
                    nl.1999.rivagg$pgreqabs[2],
                    nl.2000.rivagg$pgreqabs[2],
                    nl.2001.rivagg$pgreqabs[2],
                    nl.2002.rivagg$pgreqabs[2],
                    nl.2003.rivagg$pgreqabs[2],
                    nl.2004.rivagg$pgreqabs[2],
                    nl.2005.rivagg$pgreqabs[2],
                    nl.2006.rivagg$pgreqabs[2],
                    nl.2007.rivagg$pgreqabs[2],
                    nl.2008.rivagg$pgreqabs[2],
                    nl.2009.rivagg$pgreqabs[2],
                    nl.2010.rivagg$pgreqabs[2],
                    nl.2011.rivagg$pgreqabs[2],
                    nl.2012.rivagg$pgreqabs[2], 
                    nl.2013.rivagg$pgreqabs[2],
                    nl.2014.rivagg$pgreqabs[2],
                    nl.2015.rivagg$pgreqabs[2], 
                    nl.2016.rivagg$pgreqabs[2])

regime.type.p<- rbind(nl.1991.rivagg$pgreqabs[3],
                      nl.1992.rivagg$pgreqabs[3],
                      nl.1993.rivagg$pgreqabs[3],
                      nl.1994.rivagg$pgreqabs[3],
                      nl.1995.rivagg$pgreqabs[3],
                      nl.1996.rivagg$pgreqabs[3],
                      nl.1997.rivagg$pgreqabs[3],
                      nl.1998.rivagg$pgreqabs[3],
                      nl.1999.rivagg$pgreqabs[3],
                      nl.2000.rivagg$pgreqabs[3],
                      nl.2001.rivagg$pgreqabs[3],
                      nl.2002.rivagg$pgreqabs[3],
                      nl.2003.rivagg$pgreqabs[3],
                      nl.2004.rivagg$pgreqabs[3],
                      nl.2005.rivagg$pgreqabs[3],
                      nl.2006.rivagg$pgreqabs[3],
                      nl.2007.rivagg$pgreqabs[3],
                      nl.2008.rivagg$pgreqabs[3],
                      nl.2009.rivagg$pgreqabs[3],
                      nl.2010.rivagg$pgreqabs[3],
                      nl.2011.rivagg$pgreqabs[3],
                      nl.2012.rivagg$pgreqabs[3], 
                      nl.2013.rivagg$pgreqabs[3],
                      nl.2014.rivagg$pgreqabs[3],
                      nl.2015.rivagg$pgreqabs[3], 
                      nl.2016.rivagg$pgreqabs[3])

af.inverse.p<- rbind(nl.1991.rivagg$pgreqabs[4],
                     nl.1992.rivagg$pgreqabs[4],
                     nl.1993.rivagg$pgreqabs[4],
                     nl.1994.rivagg$pgreqabs[4],
                     nl.1995.rivagg$pgreqabs[4],
                     nl.1996.rivagg$pgreqabs[4],
                     nl.1997.rivagg$pgreqabs[4],
                     nl.1998.rivagg$pgreqabs[4],
                     nl.1999.rivagg$pgreqabs[4],
                     nl.2000.rivagg$pgreqabs[4],
                     nl.2001.rivagg$pgreqabs[4],
                     nl.2002.rivagg$pgreqabs[4],
                     nl.2003.rivagg$pgreqabs[4],
                     nl.2004.rivagg$pgreqabs[4],
                     nl.2005.rivagg$pgreqabs[4],
                     nl.2006.rivagg$pgreqabs[4],
                     nl.2007.rivagg$pgreqabs[4],
                     nl.2008.rivagg$pgreqabs[4],
                     nl.2009.rivagg$pgreqabs[4],
                     nl.2010.rivagg$pgreqabs[4],
                     nl.2011.rivagg$pgreqabs[4],
                     nl.2012.rivagg$pgreqabs[4], 
                     nl.2013.rivagg$pgreqabs[4],
                     nl.2014.rivagg$pgreqabs[4],
                     nl.2015.rivagg$pgreqabs[4], 
                     nl.2016.rivagg$pgreqabs[4])

rivalry.p<- rbind(nl.1991.rivagg$pgreqabs[5],
                  nl.1992.rivagg$pgreqabs[5],
                  nl.1993.rivagg$pgreqabs[5],
                  nl.1994.rivagg$pgreqabs[5],
                  nl.1995.rivagg$pgreqabs[5],
                  nl.1996.rivagg$pgreqabs[5],
                  nl.1997.rivagg$pgreqabs[5],
                  nl.1998.rivagg$pgreqabs[5],
                  nl.1999.rivagg$pgreqabs[5],
                  nl.2000.rivagg$pgreqabs[5],
                  nl.2001.rivagg$pgreqabs[5],
                  nl.2002.rivagg$pgreqabs[5],
                  nl.2003.rivagg$pgreqabs[5],
                  nl.2004.rivagg$pgreqabs[5],
                  nl.2005.rivagg$pgreqabs[5],
                  nl.2006.rivagg$pgreqabs[5],
                  nl.2007.rivagg$pgreqabs[5],
                  nl.2008.rivagg$pgreqabs[5],
                  nl.2009.rivagg$pgreqabs[5],
                  nl.2010.rivagg$pgreqabs[5],
                  nl.2011.rivagg$pgreqabs[5],
                  nl.2012.rivagg$pgreqabs[5], 
                  nl.2013.rivagg$pgreqabs[5],
                  nl.2014.rivagg$pgreqabs[5],
                  nl.2015.rivagg$pgreqabs[5], 
                  nl.2016.rivagg$pgreqabs[5])

cont.p<- rbind(nl.1991.rivagg$pgreqabs[6],
               nl.1992.rivagg$pgreqabs[6],
               nl.1993.rivagg$pgreqabs[6],
               nl.1994.rivagg$pgreqabs[6],
               nl.1995.rivagg$pgreqabs[6],
               nl.1996.rivagg$pgreqabs[6],
               nl.1997.rivagg$pgreqabs[6],
               nl.1998.rivagg$pgreqabs[6],
               nl.1999.rivagg$pgreqabs[6],
               nl.2000.rivagg$pgreqabs[6],
               nl.2001.rivagg$pgreqabs[6],
               nl.2002.rivagg$pgreqabs[6],
               nl.2003.rivagg$pgreqabs[6],
               nl.2004.rivagg$pgreqabs[6],
               nl.2005.rivagg$pgreqabs[6],
               nl.2006.rivagg$pgreqabs[6],
               nl.2007.rivagg$pgreqabs[6],
               nl.2008.rivagg$pgreqabs[6],
               nl.2009.rivagg$pgreqabs[6],
               nl.2010.rivagg$pgreqabs[6],
               nl.2011.rivagg$pgreqabs[6],
               nl.2012.rivagg$pgreqabs[6], 
               nl.2013.rivagg$pgreqabs[6],
               nl.2014.rivagg$pgreqabs[6],
               nl.2015.rivagg$pgreqabs[6], 
               nl.2016.rivagg$pgreqabs[6])

income.p<- rbind(nl.1991.rivagg$pgreqabs[7],
                 nl.1992.rivagg$pgreqabs[7],
                 nl.1993.rivagg$pgreqabs[7],
                 nl.1994.rivagg$pgreqabs[7],
                 nl.1995.rivagg$pgreqabs[7],
                 nl.1996.rivagg$pgreqabs[7],
                 nl.1997.rivagg$pgreqabs[7],
                 nl.1998.rivagg$pgreqabs[7],
                 nl.1999.rivagg$pgreqabs[7],
                 nl.2000.rivagg$pgreqabs[7],
                 nl.2001.rivagg$pgreqabs[7],
                 nl.2002.rivagg$pgreqabs[7],
                 nl.2003.rivagg$pgreqabs[7],
                 nl.2004.rivagg$pgreqabs[7],
                 nl.2005.rivagg$pgreqabs[7],
                 nl.2006.rivagg$pgreqabs[7],
                 nl.2007.rivagg$pgreqabs[7],
                 nl.2008.rivagg$pgreqabs[7],
                 nl.2009.rivagg$pgreqabs[7],
                 nl.2010.rivagg$pgreqabs[7],
                 nl.2011.rivagg$pgreqabs[7],
                 nl.2012.rivagg$pgreqabs[7], 
                 nl.2013.rivagg$pgreqabs[7],
                 nl.2014.rivagg$pgreqabs[7],
                 nl.2015.rivagg$pgreqabs[7], 
                 nl.2016.rivagg$pgreqabs[7])

pts.p<- rbind(nl.1991.rivagg$pgreqabs[8],
              nl.1992.rivagg$pgreqabs[8],
              nl.1993.rivagg$pgreqabs[8],
              nl.1994.rivagg$pgreqabs[8],
              nl.1995.rivagg$pgreqabs[8],
              nl.1996.rivagg$pgreqabs[8],
              nl.1997.rivagg$pgreqabs[8],
              nl.1998.rivagg$pgreqabs[8],
              nl.1999.rivagg$pgreqabs[8],
              nl.2000.rivagg$pgreqabs[8],
              nl.2001.rivagg$pgreqabs[8],
              nl.2002.rivagg$pgreqabs[8],
              nl.2003.rivagg$pgreqabs[8],
              nl.2004.rivagg$pgreqabs[8],
              nl.2005.rivagg$pgreqabs[8],
              nl.2006.rivagg$pgreqabs[8],
              nl.2007.rivagg$pgreqabs[8],
              nl.2008.rivagg$pgreqabs[8],
              nl.2009.rivagg$pgreqabs[8],
              nl.2010.rivagg$pgreqabs[8],
              nl.2011.rivagg$pgreqabs[8],
              nl.2012.rivagg$pgreqabs[8], 
              nl.2013.rivagg$pgreqabs[8],
              nl.2014.rivagg$pgreqabs[8],
              nl.2015.rivagg$pgreqabs[8], 
              nl.2016.rivagg$pgreqabs[8])

def.p<- rbind(nl.1991.rivagg$pgreqabs[9],
              nl.1992.rivagg$pgreqabs[9],
              nl.1993.rivagg$pgreqabs[9],
              nl.1994.rivagg$pgreqabs[9],
              nl.1995.rivagg$pgreqabs[9],
              nl.1996.rivagg$pgreqabs[9],
              nl.1997.rivagg$pgreqabs[9],
              nl.1998.rivagg$pgreqabs[9],
              nl.1999.rivagg$pgreqabs[9],
              nl.2000.rivagg$pgreqabs[9],
              nl.2001.rivagg$pgreqabs[9],
              nl.2002.rivagg$pgreqabs[9],
              nl.2003.rivagg$pgreqabs[9],
              nl.2004.rivagg$pgreqabs[9],
              nl.2005.rivagg$pgreqabs[9],
              nl.2006.rivagg$pgreqabs[9],
              nl.2007.rivagg$pgreqabs[9],
              nl.2008.rivagg$pgreqabs[9],
              nl.2009.rivagg$pgreqabs[9],
              nl.2010.rivagg$pgreqabs[9],
              nl.2011.rivagg$pgreqabs[9],
              nl.2012.rivagg$pgreqabs[9], 
              nl.2013.rivagg$pgreqabs[9],
              nl.2014.rivagg$pgreqabs[9],
              nl.2015.rivagg$pgreqabs[9], 
              nl.2016.rivagg$pgreqabs[9])

tr.p<- rbind(nl.1991.rivagg$pgreqabs[10],
             nl.1992.rivagg$pgreqabs[10],
             nl.1993.rivagg$pgreqabs[10],
             nl.1994.rivagg$pgreqabs[10],
             nl.1995.rivagg$pgreqabs[10],
             nl.1996.rivagg$pgreqabs[10],
             nl.1997.rivagg$pgreqabs[10],
             nl.1998.rivagg$pgreqabs[10],
             nl.1999.rivagg$pgreqabs[10],
             nl.2000.rivagg$pgreqabs[10],
             nl.2001.rivagg$pgreqabs[10],
             nl.2002.rivagg$pgreqabs[10],
             nl.2003.rivagg$pgreqabs[10],
             nl.2004.rivagg$pgreqabs[10],
             nl.2005.rivagg$pgreqabs[10],
             nl.2006.rivagg$pgreqabs[10],
             nl.2007.rivagg$pgreqabs[10],
             nl.2008.rivagg$pgreqabs[10],
             nl.2009.rivagg$pgreqabs[10],
             nl.2010.rivagg$pgreqabs[10],
             nl.2011.rivagg$pgreqabs[10],
             nl.2012.rivagg$pgreqabs[10], 
             nl.2013.rivagg$pgreqabs[10],
             nl.2014.rivagg$pgreqabs[10],
             nl.2015.rivagg$pgreqabs[10], 
             nl.2016.rivagg$pgreqabs[10])

imm.p<- rbind(nl.1991.rivagg$pgreqabs[11],
              nl.1992.rivagg$pgreqabs[11],
              nl.1993.rivagg$pgreqabs[11],
              nl.1994.rivagg$pgreqabs[11],
              nl.1995.rivagg$pgreqabs[11],
              nl.1996.rivagg$pgreqabs[11],
              nl.1997.rivagg$pgreqabs[11],
              nl.1998.rivagg$pgreqabs[11],
              nl.1999.rivagg$pgreqabs[11],
              nl.2000.rivagg$pgreqabs[11],
              nl.2001.rivagg$pgreqabs[11],
              nl.2002.rivagg$pgreqabs[11],
              nl.2003.rivagg$pgreqabs[11],
              nl.2004.rivagg$pgreqabs[11],
              nl.2005.rivagg$pgreqabs[11],
              nl.2006.rivagg$pgreqabs[11],
              nl.2007.rivagg$pgreqabs[11],
              nl.2008.rivagg$pgreqabs[11],
              nl.2009.rivagg$pgreqabs[11],
              nl.2010.rivagg$pgreqabs[11],
              nl.2011.rivagg$pgreqabs[11],
              nl.2012.rivagg$pgreqabs[11], 
              nl.2013.rivagg$pgreqabs[11],
              nl.2014.rivagg$pgreqabs[11],
              nl.2015.rivagg$pgreqabs[11], 
              nl.2016.rivagg$pgreqabs[11])

qap.1991.t<-ifelse(is.infinite(qt(1-nl.1991.rivagg$pgreqabs/2,nl.1991.rivagg$n-1)),
                   nl.1991.rivagg$tstat,
                   qt(1-nl.1991.rivagg$pgreqabs/2,nl.1991.rivagg$n-1))

qap.1992.t<-ifelse(is.infinite(qt(1-nl.1992.rivagg$pgreqabs/2,nl.1992.rivagg$n-1)),
                   nl.1992.rivagg$tstat,
                   qt(1-nl.1992.rivagg$pgreqabs/2,nl.1992.rivagg$n-1))

qap.1993.t<-ifelse(is.infinite(qt(1-nl.1993.rivagg$pgreqabs/2,nl.1993.rivagg$n-1)),
                   nl.1993.rivagg$tstat,
                   qt(1-nl.1993.rivagg$pgreqabs/2,nl.1993.rivagg$n-1))

qap.1994.t<-ifelse(is.infinite(qt(1-nl.1994.rivagg$pgreqabs/2,nl.1994.rivagg$n-1)),
                   nl.1994.rivagg$tstat,
                   qt(1-nl.1994.rivagg$pgreqabs/2,nl.1994.rivagg$n-1))

qap.1995.t<-ifelse(is.infinite(qt(1-nl.1995.rivagg$pgreqabs/2,nl.1995.rivagg$n-1)),
                   nl.1995.rivagg$tstat,
                   qt(1-nl.1995.rivagg$pgreqabs/2,nl.1995.rivagg$n-1))

qap.1996.t<-ifelse(is.infinite(qt(1-nl.1996.rivagg$pgreqabs/2,nl.1996.rivagg$n-1)),
                   nl.1996.rivagg$tstat,
                   qt(1-nl.1996.rivagg$pgreqabs/2,nl.1996.rivagg$n-1))

qap.1997.t<-ifelse(is.infinite(qt(1-nl.1997.rivagg$pgreqabs/2,nl.1997.rivagg$n-1)),
                   nl.1997.rivagg$tstat,
                   qt(1-nl.1997.rivagg$pgreqabs/2,nl.1997.rivagg$n-1))

qap.1998.t<-ifelse(is.infinite(qt(1-nl.1998.rivagg$pgreqabs/2,nl.1998.rivagg$n-1)),
                   nl.1998.rivagg$tstat,
                   qt(1-nl.1998.rivagg$pgreqabs/2,nl.1998.rivagg$n-1))

qap.1999.t<-ifelse(is.infinite(qt(1-nl.1999.rivagg$pgreqabs/2,nl.1999.rivagg$n-1)),
                   nl.1999.rivagg$tstat,
                   qt(1-nl.1999.rivagg$pgreqabs/2,nl.1999.rivagg$n-1))

qap.2000.t<-ifelse(is.infinite(qt(1-nl.2000.rivagg$pgreqabs/2,nl.2000.rivagg$n-1)),
                   nl.2000.rivagg$tstat,
                   qt(1-nl.2000.rivagg$pgreqabs/2,nl.2000.rivagg$n-1))

qap.2001.t<-ifelse(is.infinite(qt(1-nl.2001.rivagg$pgreqabs/2,nl.2001.rivagg$n-1)),
                   nl.2001.rivagg$tstat,
                   qt(1-nl.2001.rivagg$pgreqabs/2,nl.2001.rivagg$n-1))

qap.2002.t<-ifelse(is.infinite(qt(1-nl.2002.rivagg$pgreqabs/2,nl.2002.rivagg$n-1)),
                   nl.2002.rivagg$tstat,
                   qt(1-nl.2002.rivagg$pgreqabs/2,nl.2002.rivagg$n-1))

qap.2003.t<-ifelse(is.infinite(qt(1-nl.2003.rivagg$pgreqabs/2,nl.2003.rivagg$n-1)),
                   nl.2003.rivagg$tstat,
                   qt(1-nl.2003.rivagg$pgreqabs/2,nl.2003.rivagg$n-1))

qap.2004.t<-ifelse(is.infinite(qt(1-nl.2004.rivagg$pgreqabs/2,nl.2004.rivagg$n-1)),
                   nl.2004.rivagg$tstat,
                   qt(1-nl.2004.rivagg$pgreqabs/2,nl.2004.rivagg$n-1))

qap.2005.t<-ifelse(is.infinite(qt(1-nl.2005.rivagg$pgreqabs/2,nl.2005.rivagg$n-1)),
                   nl.2005.rivagg$tstat,
                   qt(1-nl.2005.rivagg$pgreqabs/2,nl.2005.rivagg$n-1))

qap.2006.t<-ifelse(is.infinite(qt(1-nl.2006.rivagg$pgreqabs/2,nl.2006.rivagg$n-1)),
                   nl.2006.rivagg$tstat,
                   qt(1-nl.2006.rivagg$pgreqabs/2,nl.2006.rivagg$n-1))

qap.2007.t<-ifelse(is.infinite(qt(1-nl.2007.rivagg$pgreqabs/2,nl.2007.rivagg$n-1)),
                   nl.2007.rivagg$tstat,
                   qt(1-nl.2007.rivagg$pgreqabs/2,nl.2007.rivagg$n-1))

qap.2008.t<-ifelse(is.infinite(qt(1-nl.2008.rivagg$pgreqabs/2,nl.2008.rivagg$n-1)),
                   nl.2008.rivagg$tstat,
                   qt(1-nl.2008.rivagg$pgreqabs/2,nl.2008.rivagg$n-1))

qap.2009.t<-ifelse(is.infinite(qt(1-nl.2009.rivagg$pgreqabs/2,nl.2009.rivagg$n-1)),
                   nl.2009.rivagg$tstat,
                   qt(1-nl.2009.rivagg$pgreqabs/2,nl.2009.rivagg$n-1))

qap.2010.t<-ifelse(is.infinite(qt(1-nl.2010.rivagg$pgreqabs/2,nl.2010.rivagg$n-1)),
                   nl.2010.rivagg$tstat,
                   qt(1-nl.2010.rivagg$pgreqabs/2,nl.2010.rivagg$n-1))

qap.2011.t<-ifelse(is.infinite(qt(1-nl.2011.rivagg$pgreqabs/2,nl.2011.rivagg$n-1)),
                   nl.2011.rivagg$tstat,
                   qt(1-nl.2011.rivagg$pgreqabs/2,nl.2011.rivagg$n-1))

qap.2012.t<-ifelse(is.infinite(qt(1-nl.2012.rivagg$pgreqabs/2,nl.2012.rivagg$n-1)),
                   nl.2012.rivagg$tstat,
                   qt(1-nl.2012.rivagg$pgreqabs/2,nl.2012.rivagg$n-1))

qap.2013.t<-ifelse(is.infinite(qt(1-nl.2013.rivagg$pgreqabs/2,nl.2013.rivagg$n-1)),
                   nl.2013.rivagg$tstat,
                   qt(1-nl.2013.rivagg$pgreqabs/2,nl.2013.rivagg$n-1))

qap.2014.t<-ifelse(is.infinite(qt(1-nl.2014.rivagg$pgreqabs/2,nl.2014.rivagg$n-1)),
                   nl.2014.rivagg$tstat,
                   qt(1-nl.2014.rivagg$pgreqabs/2,nl.2014.rivagg$n-1))

qap.2015.t<-ifelse(is.infinite(qt(1-nl.2015.rivagg$pgreqabs/2,nl.2015.rivagg$n-1)),
                   nl.2015.rivagg$tstat,
                   qt(1-nl.2015.rivagg$pgreqabs/2,nl.2015.rivagg$n-1))

qap.2016.t<-ifelse(is.infinite(qt(1-nl.2016.rivagg$pgreqabs/2,nl.2016.rivagg$n-1)),
                   nl.2016.rivagg$tstat,
                   qt(1-nl.2016.rivagg$pgreqabs/2,nl.2016.rivagg$n-1))

d<- as.data.frame(rbind(qap.1991.t,qap.1992.t,qap.1993.t,qap.1994.t,
                        qap.1995.t,qap.1996.t,qap.1997.t,qap.1998.t,
                        qap.1999.t,qap.2000.t,qap.2001.t,qap.2002.t,
                        qap.2003.t,qap.2004.t,qap.2005.t,qap.2006.t,
                        qap.2007.t,qap.2008.t,qap.2009.t,qap.2010.t,
                        qap.2011.t,qap.2012.t,qap.2013.t,qap.2014.t,
                        qap.2015.t,qap.2016.t))

write.csv(d,"qap_tstats.csv")

d<- read.csv("qap_tstats.csv")
d<- d %>% select(-year)

d<- d %>% select(-V1)

variables.d<- d %>%
  correlate() %>%
  select(-rowname)
diag(variables.d)<- 1
dissim.var.d<- as.dist(1-variables.d)

fit.v <- cmdscale(dissim.var.d,eig=TRUE, k=2)
x.v <- fit.v$points[,1]
y.v <- fit.v$points[,2]
mds.v<- data.frame(fit.v$points[,1:2])
colnames(mds.v) <- c("Dim.1", "Dim.2")
clust.v<- kmeans(mds.v,3)$cluster %>%
  as.factor()
mds.v <- mds.v %>%
  mutate(groups = clust.v)

rownames(mds.v)<- c("Prior Refugee Flows",
                    "Democracy Gradient",
                    "Arms Flows",
                    "Strategic Rivalry",
                    "Contiguity",
                    "Wage Gradient",
                    "Security Gradient",
                    "Alliance",
                    "Trade",
                    "Immigrant Population")

tiff("Publishable_mrqap_variables_whole network_colors.tiff",width = 10, height = 7, 
     units = 'in',res = 300)

ggscatter(mds.v, x = "Dim.1", y = "Dim.2", color = "groups",
          label = rownames(mds.v),palette= c("red", "green", "blue"),
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

dev.off()


##I reshaped in Excel. You can also reshape in R to go from object d to object e

e<- read.csv("qap_tstats_reshape.csv")
e<- e %>%
  select(-X)

variables.e<- e %>%
  correlate() %>%
  select(-rowname)
diag(variables.e)<- 1
dissim.var.e<- as.dist(1-variables.e)

fit.v <- cmdscale(dissim.var.e,eig=TRUE, k=2)
x.v <- fit.v$points[,1]
y.v <- fit.v$points[,2]
mds.v<- data.frame(fit.v$points[,1:2])
colnames(mds.v) <- c("Dim.1", "Dim.2")
clust.v<- kmeans(mds.v,3)$cluster %>%
  as.factor()
mds.v <- mds.v %>%
  mutate(groups = clust.v)

rownames(mds.v)<- c("1991",
                    "1992",
                    "1993",
                    "1994",
                    "1995",
                    "1996",
                    "1997",
                    "1998",
                    "1999",
                    "2000",
                    "2001",
                    "2002",
                    "2003",
                    "2004",
                    "2005",
                    "2006",
                    "2007",
                    "2008",
                    "2009",
                    "2010",
                    "2011",
                    "2012",
                    "2013",
                    "2014",
                    "2015",
                    "2016")

tiff("Publishable_mrqap_years_whole network_colors.tiff",width = 10, height = 7, 
     units = 'in',res = 300)

ggscatter(mds.v, x = "Dim.1", y = "Dim.2", color = "groups",
          label = rownames(mds.v),palette= c("red", "green", "blue"),
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

dev.off()
