labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

tabTable <- function(id) {
  ns <- shiny::NS(id)
  tabPanel(
    "Table",
    DTOutput(ns("table"))
  )
}


get_pdata <- function(geo_id){
  granja <- tryCatch(getGEO(geo_id), error=function(cond) {NULL})
  # Get pData for each element and concatenate tables
  if(is.null(granja)){
    return(NULL)
  }else{
    return(purrr::map(granja, pData) %>% dplyr::bind_rows())
  }
}

regfilter <- function(x, y, z){
  w <- c(x,y,z)
  w <- w[w!=""]
  if(length(w)==0){
    return("")
  }else{
    return(paste(w, collapse ="|"))
  }
}

is.all.empty <- function(a){
  return(all(!sapply(a, function(x) ifelse(is.null(x), T, x==""))))
}

makelist <- function(input){
  if(!is.null(input$keyword_protein)){
    keyword_protein <- tolower(input$keyword_protein)
    if(input$include_hto){
      keyword_rna <- ifelse(is.null(input$keyword_rna), "", tolower(input$keyword_rna))
      keyword_hto <- ifelse(is.null(input$keyword_hto), "", tolower(input$keyword_hto))
      if(keyword_protein==keyword_rna & keyword_protein==keyword_hto){
        return(list(protein = keyword_protein))
      }else if(keyword_protein==keyword_rna){
        return(list(protein = keyword_protein, hto=keyword_hto))
      }else if(keyword_protein==keyword_hto){
        return(list(protein = keyword_protein, rna=keyword_rna))        
      }else{
        return(list(protein = keyword_protein, rna=keyword_rna, hto=keyword_hto))
      }
    }else{
      keyword_rna <- ifelse(is.null(input$keyword_rna), "", tolower(input$keyword_rna))
      if(keyword_protein==keyword_rna){
        return(list(protein = keyword_protein))
      }else{
        return(list(protein = keyword_protein, rna=keyword_rna))
      }
    }
  }else{
    return(NULL)
  }
}

makelist_2 <- function(input, type){
  return(
    list(
      transpose = input[[paste0("load_",type,"-transpose")]],
      separate_samples = input[[paste0("load_",type,"-separate_samples")]],
      keyword = ifelse_(input[[paste0("load_",type,"-keywords_load")]], NULL, input[[paste0("load_",type,"-keywords_load")]]),
      class = ifelse_(input[[paste0("load_",type,"-class")]], NULL,  input[[paste0("load_",type,"-class")]]),
      h5key = ifelse_(input[[paste0("load_",type,"-extra_class-h5key")]],  NULL, input[[paste0("load_",type,"-extra_class-h5key")]]),
      altexp = ifelse_(input[[paste0("load_",type,"-extra_class-altexp")]], NULL,  input[[paste0("load_",type,"-extra_class-altexp")]]),
      access = ifelse_(input[[paste0("load_",type,"-extra_class-access")]], NULL,  input[[paste0("load_",type,"-extra_class-access")]]),
      drop = ifelse_(input[[paste0("load_",type,"-extra_class-drop")]], NULL,  input[[paste0("load_",type,"-extra_class-drop")]]),
      keep = ifelse_(input[[paste0("load_",type,"-extra_class-keep")]], NULL,  input[[paste0("load_",type,"-extra_class-keep")]]),
      feature_drop_or_keep = input[[paste0("load_",type,"-extra_class-feature_drop_or_keep")]],
      coldata = ifelse_(input[[paste0("load_",type,"-coldata")]], NULL,
                        stringr::str_trim(strsplit(x = input[[paste0("load_",type,"-coldata")]], split = ";")[[1]])),
      cells = ifelse_(input[[paste0("load_",type,"-extra_class-cells")]], NULL,  input[[paste0("load_",type,"-extra_class-cells")]]),
      replace = ifelse_(input[[paste0("load_",type,"-extra_class-replace")]], NULL,  input[[paste0("load_",type,"-extra_class-replace")]]),
      features = ifelse_(input[[paste0("load_",type,"-extra_class-features")]], NULL,  input[[paste0("load_",type,"-extra_class-features")]]),
      column = input[[paste0("load_",type,"-extra_class-column")]],
      sample_groups = ifelse_(input$samplegroups, NULL,
                              stringr::str_trim(strsplit(x = input$samplegroups, split = ";")[[1]])),
      samples = ifelse__(input$sampleoption=="na", NULL,
                         ifelse__(is.null(input$samplefile),  stringr::str_trim(strsplit(x = input$sampleid, split = ";")[[1]]),
                                  list(file=input$samplefile,
                                       key=input$samplekey,
                                       value=stringr::str_trim(strsplit(x = input$samplevalue, split = ";")[[1]]))))
    ))
}

makelist_3 <- function(input){
  if(input$separate_protocols){
    if(input$include_hto){
      return(list(
        protein = makelist_2(input, "protein"),
        rna = makelist_2(input, "rna"),
        hto = makelist_2(input, "hto")
      ))  
    }else{
      return(list(
        protein = makelist_2(input, "protein"),
        rna = makelist_2(input, "rna")
      ))
    } 
  }else{
    return(makelist_2(input, "protein"))  
  }
}

makelist_4 <- function(values, input, type="data"){
  
  if(!is.null(values[[paste0("GEOproteindata-",type)]])){
    protein <- values[[paste0("GEOproteindata-",type)]]
    if(type=="group" & length(unique(protein))==1){
      protein <- NULL
    }
    if(input$include_hto){
      rna <- ifelse__(is.null(values[[paste0("GEOrnadata-",type)]]), values[[paste0("GEOproteindata-",type)]], values[[paste0("GEOrnadata-",type)]])
      hto <- ifelse__(is.null(values[[paste0("GEOhtodata-",type)]]), values[[paste0("GEOproteindata-",type)]], values[[paste0("GEOhtodata-",type)]])
      rna_ <- ifelse__(is.null(values[[paste0("GEOrnadata-","data")]]), values[[paste0("GEOproteindata-","data")]], values[[paste0("GEOrnadata-","data")]])
      hto_ <- ifelse__(is.null(values[[paste0("GEOhtodata-","data")]]), values[[paste0("GEOproteindata-","data")]], values[[paste0("GEOhtodata-","data")]])
      
      if(type=="group" & length(unique(rna))==1){
        rna <- NULL
      }
      if(type=="group" & length(unique(hto))==1){
        hto <- NULL
      }
      if(identical(values[["GEOproteindata-data"]],rna_) & identical(values[["GEOproteindata-data"]],hto_)){
        if(!is.null(values[[paste0("GEOmetadata-",type)]])){
          other <- values[[paste0("GEOmetadata-",type)]]
          if(type=="group" & length(unique(other))==1){
            other <- NULL
          }
          return(list(protein = protein, other = other))
        }else{
          return(list(protein = protein))
        }
      }else if(identical(values[["GEOproteindata-data"]],rna_)){
        if(!is.null(values[[paste0("GEOmetadata-",type)]])){
          other <- values[[paste0("GEOmetadata-",type)]]
          if(type=="group" & length(unique(other))==1){
            other <- NULL
          }
          return(list(protein = protein, hto=hto, other = other))
        }else{
          return(list(protein = protein, hto=hto))
        }
      }else if(identical(values[["GEOproteindata-data"]],hto_)){
        if(!is.null(values[[paste0("GEOmetadata-",type)]])){
          other <- values[[paste0("GEOmetadata-",type)]]
          if(type=="group" & length(unique(other))==1){
            other <- NULL
          }
          return(list(protein = protein, rna=rna, other = other))
        }else{
          return(list(protein = protein, rna=rna))        
        }
      }else{
        if(!is.null(values[[paste0("GEOmetadata-",type)]])){
          other <- values[[paste0("GEOmetadata-",type)]]
          if(type=="group" & length(unique(other))==1){
            other <- NULL
          }
          return(list(protein = protein, rna=rna, hto=hto, other = other))
        }else{
          return(list(protein = protein, rna=rna, hto=hto))        
        }
      }
    }else{
      rna <- ifelse__(is.null(values[[paste0("GEOrnadata-",type)]]), values[[paste0("GEOproteindata-",type)]], values[[paste0("GEOrnadata-",type)]])
      rna_ <- ifelse__(is.null(values[[paste0("GEOrnadata-","data")]]), values[[paste0("GEOproteindata-","data")]], values[[paste0("GEOrnadata-","data")]])
      
      if(identical(values[["GEOproteindata-data"]],rna_)){
        if(!is.null(values[[paste0("GEOmetadata-",type)]])){
          other <- values[[paste0("GEOmetadata-",type)]]
          if(type=="group" & length(unique(other))==1){
            other <- NULL
          }
          return(list(protein = protein, other = other))
        }else{
          return(list(protein = protein))        
        }
      }else{
        if(!is.null(values[[paste0("GEOmetadata-",type)]])){
          other <- values[[paste0("GEOmetadata-",type)]]
          if(type=="group" & length(unique(other))==1){
            other <- NULL
          }
          return(list(protein = protein, other = other, rna=rna))
        }else{
          return(list(protein = protein, rna=rna))        
        }
      }
    }
  }else{
    if(!is.null(values[[paste0("Impossiblefiles-",type)]])){
      other <- values[[paste0("Impossiblefiles-",type)]]
      if((type=="group" & length(unique(other)) | type!="data")==1){
        other <- NULL
      }
      return(list(other = other))
    }else{
      return(NULL)        
    }
  }
}

ifelse__ <- function(x, y, z, extra_condition=NULL){
  if(x){
    return(y)
  }else{
    if(!is.null(extra_condition)){
      if(extra_condition){
        return(y)
      }else{
        return(z)
      }
    }else{
      return(z)
    }
  }
}

ifelse_ <- function(x, y, z, include_empty=T){
  if(is.null(x)){
    return(y)
  }else{
    if(include_empty){
      if(x==""){
        return(y)
      }else{
        return(z)
      }
    }else{
      return(z)
    }
  }
}