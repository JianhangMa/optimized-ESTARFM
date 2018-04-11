## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/JianhangMa/optimized-ESTARFM/edit/master/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/JianhangMa/optimized-ESTARFM/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.

;---------------------------------------------------------------------------------
;                          ***** ESTARFM *****            
;        The code was a modified version of original code that
;        developed by Zhu Xiaolin,email: zhuxiaolin55@gmail.com
;             Department of geography,the Ohio State University
;                      The Hong Kong Polytechnic University
;        https://xiaolinzhu.weebly.com/open-source-code.html
;        
;        Modified by Ma Jianhang, email: majh@radi.ac.cn
;        
;                               *Modifications*
;    1) Calculate spatial distance for all similar pixels with vectorized code rather than "FOR LOOP"
;    2) Calculation of correlation coefficient can be vectorized in IDL according to its computational formula.
;       So the calculation is done for all similar pixels
;       rather than for a single similar pixel with "FOR LOOP" and function "CORRELATE"
;
;Please cite the reference:
;(1)Xiaolin Zhu, Jin Chen, Feng Gao, & Jeffrey G Masek.
;   An enhanced spatial and temporal adaptive reflectance fusion model for complex
;   heterogeneous regions. Remote Sensing of Environment,2010,114,2610-2623
;(2)
;
;PURPOSE:  
;  Do pridiction with ESTARFM
;  Similar pixels were identified with two pairs of class images on base dates
;  Fine images can be pre-classified to generate class images
;PROCEDURE:
;  1) open data, query image information
;  2) determine valid pixels
;  3) devide the image to several blocks by lines of the image
;  4) do prediction of for each blocks with a processor by calling "estarfm_byClass_compute"
;  5) output the results
;INPUTS:
; w            : half moving window size. the moving window size in prediction is 2*w+1.
; DN_min       : minmuim value allowed of both fine and coarse image.
; DN_max       : maxmuim value allowed of both fine and coarse image.
; background   : background value of both fine and coarse image.
; FineFP1      : File Path of fine image 1.
; FineCluFP1   : File Path of class image 1.
; CoarseFP1    : File Path of coarse image 1.
; FineFP2      : File Path of fine image 2.
; FineCluFP2   : File Path of class image 2.
; CoarseFP2    : File Path of coarse image 2.
; CoarseFP_pre : File Path of coarse image 0. prediction date.
; FineFP_pre   : File Path to save the predicted fine image 0.
;OPTIONAL OUTPUT KEYWORDS:
; logstr : a string record the time consumed
;-------------------------------------------------------------------
pro  ESTARFM_ByClass_preclassification,w,DN_min,DN_max,background,$
  FineFP1,FineCluFP1,CoarseFP1,$
  FineFP2,FineCluFP2,CoarseFP2,$
  CoarseFP_pre,FineFP_pre,logstr=logstr
  compile_opt idl2  
  
  ;------------------------------------------------------------------------
  ;open the fine image of the first pair
  envi_open_file,FineFP1,r_fid=fid
  envi_file_query,fid,ns=ns,nl=nl,nb=nb,dims=dims,DATA_TYPE=dt,XSTART=xstart,YSTART=ystart
  fine1=fltarr(ns,nl,nb)
  For i = 0,nb-1 Do fine1[*,*,i] = Envi_Get_Data(Fid = Fid,dims = dims,pos=i)
  envi_file_mng, id=fid, /remove
  ;open the fine cluster image of the first pair
  envi_open_file,FineCluFP1,r_fid=fid
  fineClu1 = Envi_Get_Data(Fid = Fid,dims = dims,pos=0)
  envi_file_mng, id=fid, /remove
  ;open the coarse image of the first pair
  envi_open_file,CoarseFP1,r_fid=fid
  coarse1=fltarr(ns,nl,nb)
  For i = 0,nb-1 Do coarse1[*,*,i] = Envi_Get_Data(Fid = Fid,dims = dims,pos=i) 
  envi_file_mng, id=fid, /remove
  ;------------------------------------------------------------------------
  ;open the fine image of the second pair
  envi_open_file,FineFP2,r_fid=fid
  fine2=fltarr(ns,nl,nb)
  For i = 0,nb-1 Do fine2[*,*,i] = Envi_Get_Data(Fid = Fid,dims = dims,pos=i)
  envi_file_mng, id=fid, /remove
  ;open the fine cluster image of the second pair
  envi_open_file,FineCluFP2,r_fid=fid
  fineClu2 = Envi_Get_Data(Fid = Fid,dims = dims,pos=0)
  envi_file_mng, id=fid, /remove
  ;open the coarse image of the second pair
  envi_open_file,CoarseFP2,r_fid=fid
  coarse2=fltarr(ns,nl,nb)
  For i = 0,nb-1 Do coarse2[*,*,i] = Envi_Get_Data(Fid = Fid,dims = dims,pos=i)
  envi_file_mng, id=fid, /remove
  ;------------------------------------------------------------------------
  ;open the coarse image of the prediction time
  envi_open_file,CoarseFP_pre,r_fid=fid
  coarse0=fltarr(ns,nl,nb)
  For i = 0,nb-1 Do coarse0[*,*,i] = Envi_Get_Data(Fid = Fid,dims = dims,pos=i)
  envi_file_mng, id=fid, /remove

  fine0=intarr(ns,nl,nb)            ;blended result
  similarPixelCnts=uintarr(ns,nl)   ;record the similar pixel counts 
  
  ;compute the uncertainty,0.2% of each band is uncertain
  uncertain=(DN_max*0.002)*(2^0.5)
    
  valid_index =(fine1 ne background)
  valid_index*=(coarse1 ne background)
  valid_index*=(fine2 ne background)
  valid_index*=(coarse2 ne background)
  valid_index*=(coarse0 ne background)
  
  valid_index=PRODUCT(valid_index,3)
  
  
  t0=systime(1)
  for j=0,nl-1,1 do begin              ;retieve each target pixel
    for i=0,ns-1,1 do begin
      if (valid_index[i,j] eq 1) then begin    ;do not process the background
        ai=max([0,i-w])     ; the window location,sample line subscripts on the image
        bi=min([ns-1,i+w])
        aj=max([0,j-w])
        bj=min([nl-1,j+w])

        ci=float(i-ai)      ;location of target pixel,sample line subscripts on the window
        cj=float(j-aj)
        ind_wind_valid=where(valid_index[ai:bi,aj:bj] eq 1) ;valid pixels subscripts in the window

        ;similar pixel
        ;===================================================================
        CluTemp1=FineClu1[ai:bi,aj:bj]
        CluTemp2=FineClu2[ai:bi,aj:bj]
        S_S=(CluTemp1 eq CluTemp1[ci,cj]) and (CluTemp2 eq CluTemp2[ci,cj]) and (valid_index[ai:bi,aj:bj] eq 1)
        ; subscript of similar and valid pixels in the window
        indcand=where(S_S eq 1,number_cand)
        similarPixelCnts[i,j]=number_cand
        ;===================================================================

        ;if count of similar pixels is greater than 5, do prediction with ESTARFM
        if (number_cand gt 5) then begin
          ;DN values on fine and coarse image of similar pixels
          coarse_cand=fltarr(number_cand,nb*2)
          fine_cand  =fltarr(number_cand,nb*2)
          for iband=0,nb-1,1 do begin
            coarse_cand[*,iband]    = (coarse1[ai:bi,aj:bj,iband])[indcand]
            coarse_cand[*,iband+nb] = (coarse2[ai:bi,aj:bj,iband])[indcand]
            fine_cand[*,iband]    = (fine1[ai:bi,aj:bj,iband])[indcand]
            fine_cand[*,iband+nb] = (fine2[ai:bi,aj:bj,iband])[indcand]
          endfor
          ;compute the correlation
          ;Cov(X,Y)=E[XY]-E[X]E[Y]
          var1=mean(fine_cand*coarse_cand,dimension=2)-$
            mean(fine_cand,dimension=2)*mean(coarse_cand,dimension=2)   ;covariance
          ;D(X)=E[X^2]-(E[X])^2
          var2=sqrt(mean(fine_cand*fine_cand,dimension=2)-mean(fine_cand,dimension=2)^2)    ;standard deviation
          var3=sqrt(mean(coarse_cand*coarse_cand,dimension=2)-mean(coarse_cand,dimension=2)^2)  ;standard deviation
          S_D_cand=var1/(var2*var3)
          valid_S=S_D_cand ne S_D_cand    ;NAN value
          if total(valid_S) ne 0 then S_D_cand[where(valid_S)]=0.0

          ;spatial distance
          iw=indcand mod (bi-ai+1)
          jw=indcand/(bi-ai+1)
          D_D_cand=1.0+((ci-iw)^2+(cj-jw)^2)^0.5/float(w)

          ;compute weight of similar pixels
          C_D=(1.0-S_D_cand)*D_D_cand+0.0000001            ;combined distance
          weight=(1.0/C_D)/total(1.0/C_D)
          ;do prediction for each band
          for iband=0,nb-1,1 do begin
            ;compute V
            coarse_candTemp=[coarse_cand[*,iband],coarse_cand[*,iband+nb]]
            fine_candTemp  =[fine_cand[*,iband],fine_cand[*,iband+nb]]
            if ( stddev(coarse_candTemp) ge uncertain ) then begin ;to ensure changes in coarse image larger than uncertainty
              regress_result=regress(coarse_candTemp,fine_candTemp,FTEST=fvalue)
              sig=1.0-f_pdf(fvalue,1,number_cand*2-2)
              ;correct the result with no significancy or inconsistent change or too large value
              if (sig le 0.05 and regress_result[0] gt 0 and regress_result[0] le 5) then begin
                V_cand=regress_result[0]
              endif else begin
                V_cand=1.0
              endelse
            endif else begin
              V_cand=1.0
            endelse
            ;compute the temporal weight, with all valid pixels that includ the unsimilar pixels
            difc_pair1=abs(mean((coarse0[ai:bi,aj:bj,iband])[ind_wind_valid])-mean((coarse1[ai:bi,aj:bj,iband])[ind_wind_valid]))+0.01^5
            difc_pair2=abs(mean((coarse0[ai:bi,aj:bj,iband])[ind_wind_valid])-mean((coarse2[ai:bi,aj:bj,iband])[ind_wind_valid]))+0.01^5
            T_weight1=(1.0/difc_pair1)/(1.0/difc_pair1+1.0/difc_pair2)
            T_weight2=(1.0/difc_pair2)/(1.0/difc_pair1+1.0/difc_pair2)

            coarse0_cand=(coarse0[ai:bi,aj:bj,iband])[indcand]
            ;predict from pair1
            fine01=fine1[i,j,iband]+total(weight*V_cand*(coarse0_cand-coarse_cand[*,iband]))
            ;predict from pair2
            fine02=fine2[i,j,iband]+total(weight*V_cand*(coarse0_cand-coarse_cand[*,iband+nb]))
            ;the final prediction
            predFine=fix(round(T_weight1*fine01+T_weight2*fine02))
            ;revise the abnormal prediction
            if (predFine le DN_min or predFine ge DN_max) then begin
              fine01=total(weight*(fine1[ai:bi,aj:bj,iband])[indcand])
              fine02=total(weight*(fine2[ai:bi,aj:bj,iband])[indcand])
              predFine=fix(round(T_weight1*fine01+T_weight2*fine02))
            endif
            fine0[i,j,iband]=predFine
          endfor
        endif else begin   ;for the case of count of similar pixel =< 5
          for iband=0,nb-1,1 do begin
            ; compute the temporal weight
            difc_pair1=abs(mean((coarse0[ai:bi,aj:bj,iband])[ind_wind_valid])-mean((coarse1[ai:bi,aj:bj,iband])[ind_wind_valid]))+0.01^5
            difc_pair2=abs(mean((coarse0[ai:bi,aj:bj,iband])[ind_wind_valid])-mean((coarse2[ai:bi,aj:bj,iband])[ind_wind_valid]))+0.01^5
            T_weight1=(1.0/difc_pair1)/(1.0/difc_pair1+1.0/difc_pair2)
            T_weight2=(1.0/difc_pair2)/(1.0/difc_pair1+1.0/difc_pair2)
            fine0[i,j,iband]=fix(round(T_weight1*fine1[i,j,iband]+T_weight2*fine2[i,j,iband]))
          endfor
        endelse
      endif
    endfor
  endfor
  timecost=systime(1)-t0
  logstr=file_basename(FineFP_pre)+' time used:'+$
    string([floor(timecost/3600),floor((timecost mod 3600)/60),timecost mod 60],format='(i3,"h",i3,"m",i3,"s")')
  
  ;save the predicted image
  openw,lun,FineFP_pre,/GET_LUN
  writeu,lun,fine0
  free_lun,lun
  envi_setup_head,FNAME=FineFP_pre,NB=nb,$
    NS=ns,NL=nl,XSTART=xstart,YSTART=ystart,$
    INTERLEAVE=0,DATA_TYPE=2,/WRITE
    
  ;save the count of similar pixels
  similarCntsPath=FineFP_pre+'_SPC'
  openw,lun,similarCntsPath,/GET_LUN
  writeu,lun,similarPixelCnts
  free_lun,lun
  envi_setup_head,FNAME=similarCntsPath,NB=1,$
    NS=ns,NL=nl,XSTART=xstart,YSTART=ystart,$
    INTERLEAVE=0,DATA_TYPE=12,/WRITE
end
