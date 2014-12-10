SDd<- mean(SD_ca[g_mem,t])/mean(SD_con[g_mem,t]);
PCCd<-((sum(abs(PCC_ca[g_mem,g_mem]))-length(g_mem))/(length(g_mem)^2-length(g_mem)))/((sum(abs(PCC_con[g_mem,g_mem]))-length(g_mem))/(length(g_mem)^2-length(g_mem)));  # the diag was filled 1

out_cnt<-50*length(g_mem);

g_index_con<-match( g_mem,colnames(PCC_con) );
g_index_ca<-match( g_mem,colnames(PCC_ca) );

g_index_con_r<-match( g_mem,rownames(PCC_con) );
g_index_ca_r<-match( g_mem,rownames(PCC_ca) );
tp_e_ca<-as.vector(PCC_ca[-g_index_ca_r,g_index_ca]); ##
names(tp_e_ca)<-paste(rownames(PCC_ca)[-g_index_ca_r],rep(colnames(PCC_ca)[g_index_ca],each=nrow(PCC_ca)-length(g_mem)),sep="_"); ##

tp_e<-as.vector(PCC_con[-g_index_con_r,g_index_con]); 

names(tp_e)<-paste(rownames(PCC_con)[-g_index_con_r],rep(colnames(PCC_con)[g_index_con],each=nrow(PCC_con)-length(g_mem)),sep="_"); ##

names<- names(sort(abs(tp_e), decreasing=T,na.last=T))[1:out_cnt];
oute[[t]][[i]]<-names;

PCCi_o<- mean(abs(tp_e_ca[names]),na.rm=T)/mean(abs(tp_e[names]));

tp_I[i]<-SDd*PCCd/PCCi_o;
