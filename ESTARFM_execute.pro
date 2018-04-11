pro ESTARFM_execute
  compile_opt idl2
  envi,/RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT
  
  WIN_SIZE=uint(61)   ;moving window size
  w=WIN_SIZE/uint(2)  ;half moving window size
  num_class=6         ;number of classes
  
  DN_min=0
  DN_max=10000
  background=-9999
  
  ;base date 1
  FineFP1    ='F:\IrinaEmelyanova\CIA\Landsat\L7_2001_281_08oct'
  FineCluFP1 ='F:\IrinaEmelyanova\CIA\Landsat\L7_2001_281_08oct_ISODATA_C6'
  CoarseFP1  ='F:\IrinaEmelyanova\CIA\MODIS\MOD09GA_A2001281'
  ;base date 2
  FineFP2    ='F:\IrinaEmelyanova\CIA\Landsat\L7_2001_306_02nov'
  FineCluFP2 ='F:\IrinaEmelyanova\CIA\Landsat\L7_2001_306_02nov_ISODATA_C6'
  CoarseFP2  ='F:\IrinaEmelyanova\CIA\MODIS\MOD09GA_A2001306'
  ;prediction date
  CoarseFP_pre='F:\IrinaEmelyanova\CIA\MODIS\MOD09GA_A2001290'
  FineFP_pre  ='F:\IrinaEmelyanova\CIA\LandsatSimClu\PF_C6_fwc_A2001290'
  
  ESTARFM_ByClass_preclassification,w,DN_min,DN_max,background,$
    FineFP1,FineCluFP1,CoarseFP1,$
    FineFP2,FineCluFP2,CoarseFP2,$
    CoarseFP_pre,FineFP_pre+'_Class',logstr=logstr
  print,logstr
  
  ESTARFM_ByThreshold,w,num_class,DN_min,DN_max,background,$
    FineFP1,CoarseFP1,FineFP2,CoarseFP2,CoarseFP_pre,FineFP_pre+'_Threshold',logstr=logstr
  print,logstr
  
  ENVI_BATCH_EXIT
end
