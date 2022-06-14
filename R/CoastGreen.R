#' Pixellate images
#'
#' Resize and pixellate images
#' @param imgfile Import color image file.
#' @param rf Scale of resize, default is 0.5.
#' @param rp Pixellate image per square of rp pixels, default is 10.
#' @param rW Resize image to specific width. default is NULL.
#' @param rH Resize image to specific height default is NULL.
#' @param resize.out logical, default is FALSE. Output resized image
#' @param plot.px logical, default is FALSE. Plot pixellated image
#' @return cimg format array
#' @export
pixeliz <- function(imgfile,rf=0.5,rp=10,rW=NULL,rH=NULL,resize.out=FALSE,plot.px=FALSE){
  require(imager)
  x <- load.image(imgfile)
  rf=rf #rescale
  if(is.null(rW)){
    rW <- ceiling(width(x)/100)*100*rf #round to integer and rescale: width
  } else if(!is.null(rW)) rW=rW
  if(is.null(rH)){
    rH <- ceiling(height(x)/100)*100*rf #round to integer and rescale: height
  } else if(!is.null(rH)) rH=rH
  x <- resize(x,rW,rH)
  #plot(x,xlim=c(1,rW),ylim=c(rH,1),axes = F)

  rp <- rp #pixel size
  #pixellation
  xx <- x
  for(i in 0:((rW/rp)-1)){
    xox <- ((1:rp)+(i*rp))
    for(j in 0:((rH/rp)-1)){
      yoy <- ((1:rp)+(j*rp))
      xx[xox,yoy,,1] <- mean(xx[xox,yoy,,1])
      xx[xox,yoy,,2] <- mean(xx[xox,yoy,,2])
      xx[xox,yoy,,3] <- mean(xx[xox,yoy,,3])
    }
  }
  if(isTRUE(plot.px)) plot(xx,xlim=c(1,rW),ylim=c(rH,1),axes = F)
  if(isTRUE(resize.out)){
    invisible(return(list(x,xx)))
  } else invisible(return(xx))
}

#' Extract pixellated colors
#'
#' Extract representative colors from pixellated images
#' @param img cimg array object
#' @param rW Width of pixellated image.
#' @param rH Height of pixellated image.
#' @param rp Square of rp pixels of the pixellated image.
#' @return minimized cimg format array
#' @export
repix <- function(img,rW,rH,rp){
  require(imager)
  zx=seq(1,rW,rp)
  zy=seq(1,rH,rp)

  zidx <- cbind(zx=rep(zx,each=length(zy)),zy=rep(zy,length(zx)))
  zz <- array(NA,dim = c(length(zx),length(zy),1,3))
  zz[1:length(zx),1:length(zy),,] <- img[zx,zy,,]
  class(zz) <- class(img)
  return(zz)
}

#' Extract pixellated green value/hex color code in an image
#'
#' @param img cimg array object
#' @param output "Garea", coveragre (%) of green area in a picture. "HEX", hex color code of green area.
#' @param plot.green logical, plot green area only.
#' @export
Gscore <- function(img,output=c("Garea","HEX"),plot.green=FALSE){
  require(imager)
  y <- img
  if(all(output==c("Garea","HEX"))) output="Garea"
  yidxB <- which(y[,,,2] < y[,,,3] ,arr.ind = T)
  yidxR <- which(y[,,,2] < y[,,,1] ,arr.ind = T)
  yidx <- rbind(yidxB,yidxR)
  for(i in 1:nrow(yidx)) y[yidx[i,1],yidx[i,2],,] <- NA
  meanR <- mean(y[,,,1],na.rm = T)
  meanG <- mean(y[,,,2],na.rm = T)
  meanB <- mean(y[,,,3],na.rm = T)
  Grgb <- rgb(meanR,meanG,meanB)
  allarea <- y[,,,1]+y[,,,2]+y[,,,3]
  Garea <- sum(!is.na(allarea))/(dim(allarea)[1]*dim(allarea)[2])
  if(output=="Garea"){
    if(isTRUE(plot.green)) {
      plot(y,xlim=c(1,dim(img)[1]),ylim=c(dim(img)[2],1),axes = F)
      print(Garea)
    } else if(!isTRUE(plot.green)){
      print(Garea)
    }
    invisible((list(Garea,y)))
  } else if(output=="HEX"){
    if(isTRUE(plot.green)) {
      plot(y,xlim=c(1,dim(img)[1]),ylim=c(dim(img)[2],1),axes = F)
      print(Grgb)
    } else if(!isTRUE(plot.green)){
      print(Grgb)
    }
    invisible((list(Grgb,y)))
  }
}

#' Extract pixellated blue value/hex color code in an image
#'
#' @param img cimg array object
#' @param output "Barea", coveragre (%) of blue area in a picture. "HEX", hex color code of blue area.
#' @param plot.blue logical, plot blue area only.
#' @export
Bscore <- function(img,output=c("Barea","HEX"),plot.blue=FALSE){
  require(imager)
  y <- img
  if(all(output==c("Barea","HEX"))) output="Barea"
  yidxG <- which(y[,,,3] < y[,,,2] ,arr.ind = T)
  yidxR <- which(y[,,,3] < y[,,,1] ,arr.ind = T)
  yidx <- rbind(yidxG,yidxR)
  for(i in 1:nrow(yidx)) y[yidx[i,1],yidx[i,2],,] <- NA
  meanR <- mean(y[,,,1],na.rm = T)
  meanG <- mean(y[,,,2],na.rm = T)
  meanB <- mean(y[,,,3],na.rm = T)
  Brgb <- rgb(meanR,meanG,meanB)
  allarea <- y[,,,1]+y[,,,2]+y[,,,3]
  Barea <- sum(!is.na(allarea))/(dim(allarea)[1]*dim(allarea)[2])
  if(output=="Barea"){
    if(isTRUE(plot.blue)) {
      plot(y,xlim=c(1,dim(img)[1]),ylim=c(dim(img)[2],1),axes = F)
      print(Barea)
    } else if(!isTRUE(plot.blue)){
      print(Barea)
    }
    invisible((list(Barea,y)))
  } else if(output=="HEX"){
    if(isTRUE(plot.blue)) {
      plot(y,xlim=c(1,dim(img)[1]),ylim=c(dim(img)[2],1),axes = F)
      print(Brgb)
    } else if(!isTRUE(plot.blue)){
      print(Brgb)
    }
    invisible((list(Brgb,y)))
  }
}
