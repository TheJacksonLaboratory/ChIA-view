
IN="GAM001_COMBINED_SA.SUBGEM.bsv.gff.GEMLINE_OUT.tsv.BE2_FRAGNUM.Fragnum_PlinePgem.PlinePfrag.region"
OUT="GAM001"
dir.create(OUT)
###########################
dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
FIN=read.table(IN)
nrow(FIN)
str(FIN)
CHR_LIST<-unique(FIN$V1)
str(CHR_LIST)
for(CHROM in CHR_LIST){
  FILE01=paste0(CHROM,".tsv")
  file.remove(FILE01)
  for (row in 1:nrow(FIN)){
    if(FIN[row,1]==CHROM){write.table(FIN[row,],FILE01,sep = "\t",quote=FALSE,row.names=FALSE,col.names=FALSE,append = TRUE) }
  }
}

for(CHROM in CHR_LIST){
  FILE01=paste0(CHROM,".tsv")
  FILE02=paste0(OUT,"/",CHROM,".SUBRDS")
  saveRDS(read.table(FILE01),FILE02)
  #  file.remove(FILE01)
}

for(CHROM in CHR_LIST){
  FILE01=paste0(CHROM,".tsv")
  file.remove(FILE01)
}


