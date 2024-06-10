#' creates new column for subregions, or the individual cut faces
#' 
#' @param stb a character vector describing if a point is part of the side, top, or bottom
#' @param type_s a character vector with either NA (for top and bottom) or what direction the side is going (up or down)
#' @param x a numeric vector of x location
#' @param y a numeric vector of y location
#' @returns a numeric vector of subregion 
side_scriber_subreg <- function(stb, type_s, x, y){
  out <- vector(mode = "numeric", length = length(type_s))
  type <- "down"
  subreg <- 0
  last_new <- 1
  for(i in 1:length(stb)){
    j <- i - 1
    if(x[[i]] != x[[last_new]]){
      subreg <- 0
    }
    if(stb[[i]] == "side"){
      
      if(!is.na(type_s[[i]])){
        type = type_s[[i]]
        subreg = subreg + 1
        last_new <- i
      }
      out[[i]] <- subreg
    }
    else{
      if((i > 1)){
        if((stb[[j]] != "side") & 
           (stb[[j]] != stb[[i]]) & 
           (!is.na(stb[[j]]))){
          subreg <- subreg + 1
          last_new <- i
        }
      }
      out[[i]] <- NA
    }
  }
  return(out)
}

#' rewrites the type of side vector so that every side point has a direction
#' 
#' @param stb a character vector describing if a point is part of the side, top, or bottom
#' @param type_s a character vector with either NA (for top and bottom) or what direction the side is going (up or down)
#' @param x a numeric vector of x location
#' @param y a numeric vector of y location
#' @returns a character vector that builds on type_s and fully gives directions to every side point 
side_scriber_type <- function(stb, type_s, x, y){
  type <- "down"
  subreg <- 0
  last_new <- 1
  for(i in 1:length(stb)){
    if(stb[[i]] == "side"){
      if(x[[i]] != x[[last_new]]){
        subreg <- 0
      }
      if(!is.na(type_s[[i]])){
        type <- type_s[[i]]
        subreg <- subreg + 1
        last_new <- i
      }
      type_s[[i]] <- type
    }
  }
  return(type_s)
}


#'  creates new columns that say where the point is (top, bottom, or side), what direction the side is pointing (up or down), and what subregion a side exists in 
#'  
#'  @param data a dataframe or tibble containing X, Y, and Z columns
#'  @param upper a number, this is the Z value that all points above are marked as top
#'  @param lower a number, this is the Z value that all points below are marked as bottom
#'  @param soft_upper a number less than upper, if Z is greater than this value, the function will check if the point is a local maximum before assigning top status
#'  @param soft_lower a number greater than lower, if Z is less than this value, the function will check if the point is a local minimum before assigning bottom status
#'  @returns an object of the same type as data with three new columns 
subregion_processer <- function(data, upper, lower, soft_upper, soft_lower){
  oof <- data %>% 
    arrange(X, Y) %>% 
    mutate(better_topvbot = case_when(Z < lower ~ "bottom", 
                                      Z > upper ~ "top", 
                                      lag(Z) < lower & 
                                        lead(lead(Z)) < lower ~ "bottom", 
                                      lag(lag(Z)) < lower & 
                                        lead(Z) < lower ~ "bottom",
                                      lag(Z) > upper & 
                                        lead(lead(Z)) > upper ~ "top",
                                      lag(lag(Z)) > upper & 
                                        lead(Z) > upper ~ "top",
                                      (Z < soft_lower) & 
                                        (((Z < lag(Z)) & (Z < lead(Z))) &
                                           ((Z < (lag(lag(Z)))) & 
                                              (Z < (lead(lead(Z)))))) ~
                                        "bottom", 
                                      (Z > soft_upper) & 
                                        (((Z > lag(Z)) & (Z > lead(Z))) &
                                           ((Z > (lag(lag(Z)))) & 
                                              (Z > (lead(lead(Z)))))) ~
                                        "top", 
                                      .default = "side"),
           type_side = case_when(((better_topvbot == "side") & 
                                    (lag(better_topvbot) == "bottom")) ~
                                   "up", 
                                 ((better_topvbot == "side") & 
                                    (lag(better_topvbot) == "top")) ~ "down",
                                 .default = NA))
  
  oof$subregion <- side_scriber_subreg(oof$better_topvbot, 
                                       oof$type_side, 
                                       oof$X, 
                                       oof$Y)
  oof$type_side <- side_scriber_type(oof$better_topvbot, 
                                     oof$type_side, 
                                     oof$X, 
                                     oof$Y)
  
  return(oof)
}