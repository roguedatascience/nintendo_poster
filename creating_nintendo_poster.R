library(tidyverse)
library(rvest)
library(stringr)
library(EBImage)


###########################################
#
# Scrape images
#
###########################################


links <-
    c('https://en.wikipedia.org/w/index.php?title=Category:Nintendo_Entertainment_System_game_covers',
      'https://en.wikipedia.org/w/index.php?title=Category:Nintendo_Entertainment_System_game_covers&filefrom=Dragons+of+Flame+Coverart.png#mw-category-media',
      'https://en.wikipedia.org/w/index.php?title=Category:Nintendo_Entertainment_System_game_covers&filefrom=Metal+Storm+North+American+NES+box+art.jpg#mw-category-media',
      'https://en.wikipedia.org/w/index.php?title=Category:Nintendo_Entertainment_System_game_covers&filefrom=Tecmobowlfront.jpg#mw-category-media')


for(i in 1:length(links)){
    
    message(i)
    
    temp_file <-
        read_lines(links[i])
    
    write_lines(temp_file,
                str_c('pages_repository/pages', i, '.html'))
    
}


results_pages <-
    list.files('pages_repository/') %>%
    paste0('pages_repository/', .)

get_gamecover_info <- function(results_page){
    
    #results_page <- results_pages[1]
    
    message(results_page)
    
    page <-
        read_html(results_page)
    
    image_links <-
        page %>%
        html_node('.mw-category') %>%
        html_nodes('li') %>%
        html_nodes('a') %>%
        html_attr('href')
    
    game_names <-
        page %>%
        html_node('.mw-category') %>%
        html_nodes('li') %>%
        html_nodes('a') %>%
        html_attr('title')
    
    df <-
        data_frame(
            game_name = game_names,
            image_link = image_links
        )
    
    return(df)
    
}

gamecover_info <-
    map(results_pages, get_gamecover_info)

gamecover_info_df <-
    bind_rows(gamecover_info) %>%
    mutate(image_link =
               str_c('https://en.wikipedia.org', image_link),
           game_name =
               game_name %>%
               str_replace_all('File:', '')) %>%
    mutate(game_id = 1:n()) %>%
    mutate(extension =
               str_extract(image_link,
                           '.([a-zA-Z]{2,4})$'))


url_repository <-
    list()

#for(i in 1:3){
for(i in 1:nrow(gamecover_info_df)){
    
    message(i)
    
    link <-
        gamecover_info_df$image_link[i]
    
    game_id <-
        gamecover_info_df$game_id[i]
    
    image_page <-
        read_html(link)
    
    image_url <-
        image_page %>%
        html_node('.fullImageLink') %>%
        html_node('a') %>%
        html_attr('href')
    
    url_df <-
        data_frame(
            game_id = game_id,
            image_url = image_url
        )
    
    url_repository[[i]] <-
        url_df
    
}

url_df <-
    url_repository %>%
    bind_rows()

gamecover_info_df <-
    gamecover_info_df %>%
    left_join(url_df, by = 'game_id') %>%
    mutate(image_url = str_c('https:', image_url))

write_rds(gamecover_info_df, 'gamecover_info.rds')


#for(i in 1:3){
for(i in 1:nrow(gamecover_info_df)){
    
    message(i)
    
    game_id <-
        gamecover_info_df$game_id[i]
    
    image_url <-
        gamecover_info_df$image_url[i]
    
    file_extension <-
        gamecover_info_df$extension[i]
    
    image_name <-
        str_c('image_repository/',
              game_id,
              file_extension)
    
    # https://stackoverflow.com/questions/29110903/how-to-download-and-display-an-image-from-an-url-in-r?lq=1
    download.file(image_url,
                  image_name,
                  mode = 'wb')
    
}

# Some images removed manually
# New repository saved as image_repository_trimmed



###########################################
#
# Get image sizes
#
###########################################

images <-
    list.files('image_repository_trimmed/') %>%
    str_c('image_repository_trimmed/', .)


### Get image sizes

get_image_size <- function(image_file){
    
    #image_file <- images[1]
    
    message(image_file)
    
    temp_image <-
        EBImage::readImage(image_file)
    
    temp_image <-
        EBImage::resize(temp_image, h = 100)
    
    temp_id <-
        image_file %>%
        str_extract('[0-9]{1,3}') %>%
        as.numeric()
    
    temp_width <-
        dim(temp_image)[1]
    
    temp_height <-
        dim(temp_image)[2]
    
    temp_df <-
        data_frame(
            game_id = temp_id,
            width = temp_width,
            height = temp_height
        )
    
    return(temp_df)
    
}

image_sizes <-
    map(images, get_image_size)

image_sizes_df <-
    bind_rows(image_sizes)

write_rds(image_sizes_df, 'image_sizes.rds')

# gamecover_info_df3 <-
#     gamecover_info_df2 %>%
#     mutate(width2 = width / (height / 100))






###########################################
#
# Get image order
#
###########################################


source('image_functions.R')

images <-
    list.files('image_repository_trimmed/') %>%
    str_c('image_repository_trimmed/', .)

processed_images <-
    map(images, process_image, new_size = 100, clust_num = 5)

processed_images_df <-
    bind_rows(processed_images) %>%
    arrange(image, desc(perc)) %>%
    group_by(image) %>%
    slice(1) %>%
    # summarise(wt_mean_r = weighted.mean(x = r, w = perc),
    #           wt_mean_g = weighted.mean(x = g, w = perc),
    #           wt_mean_b = weighted.mean(x = b, w = perc)) %>%
    select(image, s) %>%
    mutate(game_id =
               image %>%
               str_extract('[0-9]{1,3}') %>%
               as.numeric()) %>%
    ungroup() %>%
    select(-image) %>%
    as.data.frame() #%>%
    #column_to_rownames('game_id')

# distance_m <-
#     dist(processed_images_df)
# 
# h_clust <-
#     hclust(distance_m)
# 
# #plot(h_clust)
# 
# images_order_df <-
#     data_frame(
#         order = h_clust$order,
#         game_id = as.numeric(h_clust$labels)
#     )

images_order_df <-
    processed_images_df %>%
    arrange(s) %>%
    mutate(order = 1:n()) %>%
    select(-s)

write_rds(images_order_df, 'images_order.rds')




###########################################
#
# All together
#
###########################################

gamecover_info_df <-
    read_rds('gamecover_info.rds') %>%
    mutate(
        extension =
            str_extract(image_link,
                        '.([a-zA-Z]{2,4})$')
    ) %>%
    mutate(extension =
               ifelse(extension == '.gif', '.jpg', extension))

image_sizes_df <-
    read_rds('image_sizes.rds')

images_order_df <-
    read_rds('images_order.rds')


final_img_df <-
    gamecover_info_df %>%
    left_join(image_sizes_df, by = 'game_id') %>%
    left_join(images_order_df, by = 'game_id') %>%
    arrange(order)

size_choice_w <- 3400
height_base <- 17
size_choice_h <- height_base * 100

image_selected <- 0
width_covered <- 0
selected_images <- rep(0, nrow(final_img_df))
boundary_images <- rep(0, nrow(final_img_df))
boundary_cut <- rep(NA, nrow(final_img_df))
max_width <- size_choice_w
x_start <- rep(NA, nrow(final_img_df))
x_end <- rep(NA, nrow(final_img_df))
y_start <- rep(NA, nrow(final_img_df))
y_end <- rep(NA, nrow(final_img_df))


for(i in 1:height_base){
    
    while(width_covered < max_width &
          image_selected < nrow(final_img_df)){
        
        image_selected <-
            image_selected + 1
        
        selected_images[image_selected] <- 1
        
        x_start[image_selected] <-
            width_covered + 1
        
        #message(x_start[image_selected])
        
        y_start[image_selected] <-
            (i - 1) * 100 + 1
        
        width_covered <-
            width_covered + final_img_df$width[image_selected]
        
        #message(width_covered)
        
        x_end[image_selected] <-
            width_covered
        
        y_end[image_selected] <-
            i * 100
        
    }
    
    if(image_selected != nrow(final_img_df)){
        
         #If the image ends flush on the edge, just start clean 
         if(width_covered == max_width){
             
             width_covered <- 0
             message(paste0('      ', width_covered))
             message(image_selected)
             
         }
        
        else{
            
            # Set right width end
            x_end[image_selected] <-
                max_width
            
            extra <-
                width_covered - max_width
            
            width_covered <-
                extra
            
            boundary_images[image_selected] <- 1
            boundary_cut[image_selected] <- extra
            
        }
        
        message(
            paste(
                i,
                '---',
                image_selected,
                '---',
                extra
            )
        )
        
    }
    
    else{
        
        message('No more images')
        message(
            paste(max_width - width_covered,
            'remaining')
        )
        
        break
        
    }
    
    if(i == height_base){
        message(
            paste(
                nrow(final_img_df) - image_selected,
                'remaining images'
            )
        )
    }
    
}


final_img_df <-
    final_img_df %>%
    mutate(selected_images = selected_images,
           boundary_images = boundary_images,
           boundary_cut = boundary_cut,
           x_start = x_start,
           x_end = x_end,
           y_start = y_start,
           y_end = y_end,
           image_start = 0)

cut_images_df <-
    final_img_df %>%
    filter(boundary_images == 1) %>%
    mutate(order = order + 0.5,
           x_start = 1,
           x_end = boundary_cut,
           y_start = y_start + 100,
           y_end = y_end + 100,
           image_start = 1) %>%
    filter(y_end <= size_choice_h)

final_img_df2 <-
    bind_rows(
        final_img_df,
        cut_images_df
    ) %>%
    arrange(order) %>%
    filter(!is.na(y_end))




# 
# x <-
#     EBImage::readImage('image_repository/1.jpg') %>%
#     .@.Data
# 
# temp_img <-
#     EBImage::readImage('image_repository/1.jpg')
# 
# temp_img <-
#     EBImage::resize(temp_img, h = 100) %>%
#     .@.Data %>%
#     .[, , 1:3]
# 
# xx <-
#     x[1:2, 1:10, 1:2]

rep_array <-
    array(rep(0, size_choice_w * size_choice_h * 3),
          dim = c(size_choice_w, size_choice_h, 3))

for(i in 1:nrow(final_img_df2)){
    
    message(i)
    
    temp_img <-
        EBImage::readImage(
            str_c('image_repository_trimmed/',
                  final_img_df2$game_id[i],
                  final_img_df2$extension[i])
        ) %>%
        EBImage::resize(h = 100) %>%
        .@.Data
    
    if(final_img_df2$boundary_images[i] == 1){
        
        mid_point <-
            final_img_df2$width[i] - final_img_df2$boundary_cut[i]
        
        if(final_img_df2$image_start[i] != 1){
            
            temp_img <-
                temp_img %>%
                .[1:mid_point, , 1:3]
            
        }
        
        else{
            
            temp_img <-
                temp_img %>%
                .[(mid_point + 1):final_img_df2$width[i], , 1:3]
            
        }
        
    }
    
    else{
        
        temp_img <-
            temp_img %>%
            .[, , 1:3]
        
    }
    
    rep_array[final_img_df2$x_start[i]:final_img_df2$x_end[i],
              final_img_df2$y_start[i]:final_img_df2$y_end[i],
              1:3] <- 
        temp_img
    
}

img_x <-
    EBImage::Image(rep_array, colormode = Color)

display(img_x, method = 'raster', all = TRUE)

filename = 'nintendo_print.jpg'

dev.print(jpeg,
          filename = filename,
          width = dim(img_x)[1], height = dim(img_x)[2])

