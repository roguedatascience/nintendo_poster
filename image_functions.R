library(EBImage)
library(tidyverse)

.rrr_img <- function(img_file, new_size = NULL) {
    
    #img_file <- 'MalePeacockSpider.jpg'
    #new_size <- 100
    
    ## Read Image
    img <-
        EBImage::readImage(img_file)
    
    if(!is.null(new_size)){
        
        ### If width or height greater than resize, resize the largest
        if(max(dim(img)[1:2]) > new_size){
            
            if(dim(img)[1] > dim(img)[2]){
                img <- EBImage::resize(img, w = new_size)
            }
            
            else{
                img <- EBImage::resize(img, h = new_size)
            }
            
        }
        
    }
    
    
    #return(img)
    
    rgb_df <-
        data_frame(rgb_num = c(1, 2, 3),
                   rgb = c('r', 'g', 'b'))
    
    img_melt <-
        reshape::melt(img)
    
    names(img_melt) <-
        c('width', 'height', 'rgb_num', 'value')
    
    img_melt <-
        img_melt %>%
        # remove alpha value from png
        filter(rgb_num < 4) %>%
        left_join(rgb_df, by = 'rgb_num') %>%
        select(-rgb_num) %>%
        select(width, height, rgb, value) %>%
        spread(rgb, value) %>%
        select(width, height, r, g, b)
    
    return(img_melt)
    
    #img_melt <-
    #    img_melt %>%
    #    mutate(hex = rgb(r, g, b))
    #
    #img_melt_rcolor <-
    #    t(col2rgb(img_melt$hex) / 255) %>%
    #    as_data_frame() %>%
    #    rename(r2 = red, g2 = green, b2 = blue)
    #
    #img_melt_final <-
    #    bind_cols(img_melt, img_melt_rcolor)
    #    
    #
    #return(img_melt_final)
    
}


.extract_top_colors <- function(img_df, clust_num){
    
    set.seed(13)
    k <-
        kmeans(img_df %>% select(r, g, b), clust_num)
    
    clust <-
        k$cluster %>%
        table() %>%
        as.data.frame(stringsAsFactors = FALSE)
    
    names(clust) <-
        c('center', 'cnt')
    
    top_colors <-
        k$centers %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        rownames_to_column() %>%
        rename(center = rowname) %>%
        left_join(clust, by = 'center') %>%
        mutate(perc = cnt / sum(cnt)) %>%
        mutate(hex = rgb(r, g, b))
    
    hsv <-
        t(rgb2hsv(r = top_colors$r,
                  g = top_colors$g,
                  b = top_colors$b)) %>%
        as_data_frame()
    
    top_colors_final <-
        bind_cols(top_colors, hsv)
    
    return(top_colors_final)
    
}

process_image <- function(img_file,
                          new_size,
                          clust_num){
    
    message(img_file)
    
    temp <-
        .rrr_img(img_file, new_size) %>%
        .extract_top_colors(clust_num) %>%
        mutate(image = img_file)
    
    return(temp)
    
}