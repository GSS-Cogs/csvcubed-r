config_csv <- function(df,
                          id ='' ,
                          title ='' ,
                          description  ='' ,
                          summary ='',
                          publisher ='',
                          creator ='' ,
                          themes  ='',
                          keywords ='',
                          dataset.issued ='',
                          dataset.modified ='',
                          license ='',
                          public.contact.point.uri=''){
    config<- list()
    class(config) <- "configuration"
    config$`$schema` = 'https://purl.org/csv-cubed/qube-config/v1'
    if(id != ''){
      config$id = if (isSingleString(id)) id else stop('Warning: \'id\' must be a single nonempty string.')
    }
    if(title != ''){
      config$title = if (isSingleString(title)) title else stop('Warning: \'title\' must be a single nonempty string.')
    }
    if(summary != ''){
      config$summary = if (isSingleString(summary)) summary else stop('Warning: \'summary\' must be a single nonempty string.')
    }
    if(publisher != ''){
      config$publisher = if (isSingleString(publisher)&is_valid_url(publisher)) publisher else stop('Warning: \'publisher\' must be a URL.')
    }
    if(creator != ''){
      config$creator = if (isSingleString(creator) &is_valid_url(creator)) creator else  stop('Warning: \'creator\' must be a URL .')
    }
    config <- assign.themes(config, themes)
    config <- assign.keywords(config,keywords)
    if (dataset.issued!= ''){
      config$dataset_issued = if (isSingleString(dataset.issued)) anytime::iso8601(anytime::anytime(dataset.issued)) else stop('Warning: \'dataset.issued\' must be a single nonempty string.')
    }
    if (dataset.modified!= ''){
      config$dataset_modified = if (isSingleString(dataset.modified)) anytime::iso8601(anytime::anytime(dataset.modified)) else stop('Warning: \'dataset_modified\' must be a single nonempty string.')
    }
    if(license !=''){
      config$license = if (isSingleString(license) & is_valid_url(license)) license else stop('Warning: \'license\' must be an URL .')
    }
    if(public.contact.point.uri != ''){
      config$public_contact_point_uri = if (isSingleString(public.contact.point.uri) &is_valid_url(public.contact.point.uri)) public.contact.point.uri else print('Warning: \'public.contact.point.uri\' must be an URL .')
    }
    config$column_names = names(df)
    config$columns = list()
    return(config)
}


add.dimension.configuration <- function(config,
                                        label = '',
                                        description= '',
                                        from.existing='',
                                        definition.uri='',
                                        uri.override='',
                                        cell.uri.template='',
                                        code.list='',
                                        from.template=''
){
  config <- add.column.configuration(config,
                           label = label,
                           type = 'dimension',
                           description = description,
                           from.existing = from.existing,
                           definition.uri = definition.uri,
                           uri.override =uri.override,
                           cell.uri.template = cell.uri.template,
                           code.list= code.list,
                           from.template = from.template)
  return(config)
}

add.attribute.configuration(config,
                            label = '',
                            description = '',
                            from.existing ='',
                            definition_uri = '',
                            data.type = '',
                            required = '',
                            values = '',
                            from.template ='',
                            resource.or.literal = 'resource'){
  config <- add.column.configuration(config,
                                     label,
                                     type = 'attribute',
                                     description,
                                     from.existing,
                                     definition.uri,
                                     data.type,
                                     required,
                                     code.list,
                                     values,
                                     from.template)
}

# add.attribute.configuration(config, "Label", values=c(AttributeValue("Attribute value label"), AttributeValue("Another Attribute Value")))
# add.attribute.configuration(config, "Label", values=c("Attribute value label", "Another Attribute Value"))

AttributeValue <- function(label, description='', from.existing='') {
  return list(label=label, description=description, from_existing=from.existing)
}

add.column.configuration<- function(config,
                                label = '',
                                type = '',
                                description = '',
                                from.existing = '',
                                definition.uri = '',
                                uri.override='',
                                cell.uri.template='',
                                code.list='',
                                from.template='',
                                data.type = '',
                                required = ''){
  if (label %in% config$column_names){
    eval(str2expression(paste('config$columns<- append(config$columns, list(', label, '= list()',  '))', sep='')))
  } else {
    stop(paste('Warning: label \"', label, '\"is not one of the columns.', sep = ''))
  }
  eval(str2expression(paste('config$columns$', label, '<- append(', 'config$columns$', label,', list(type=\'', type, '\'))', sep='')))
  config <- assign.column.attribute.string(config, label, 'description', description)
  config <- assign.column.attribute.uri(config, label, 'from_existing', from.existing)
  config <- assign.column.attribute.uri(config, label, 'definition_uri', definition.uri)
  config <- assign.column.attribute.string(config, label, 'uri_override', uri.override)
  config <- assign.column.attribute.uri(config, label, 'cell_uri_template', uri.override)
  config <- assign.column.code.list(config, label, code.list)
  config <- assign.column.attribute.string(config,label, 'from_template',from.template)
  config <- assign.column.attribute.string(config, label, 'data_type', data.type)
  config <- assign.column.attribute.boolean(config, label, 'required', required)
  return(config)
}

generate.json.configuration <- function(config, file =''){
  config_list <- unclass(config)
  config_list <- within(config_list, rm(column_names))
  if (length(config_list$columns) == 0){
    config_list <- within(config_list, rm(columns))
  }
  config_json_str= jsonlite::toJSON(config_list, auto_unbox = T, pretty = T)
  print(config_json_str)
  write(config_json_str, file = file)
}

isSingleString <- function(input) {
  is.character(input) & length(input) == 1
}

is_valid_url <- function(string) {
  parsed_address <- url_parse(string)
  !is.na(parsed_address$scheme)
}

assign.column.attribute.uri<- function(config, label, attribute, uri){
  if (is_valid_url(uri)){
    eval(str2expression(paste('config$columns$', label, '<- append(', 'config$columns$', label,', list(', attribute, '=\'', uri, '\'))', sep='')))
  } else if(uri ==''){
    return(config)
  }
  else{
    stop(paste('Warning: ',attribute,' must a uri.', sep=''))
  }
  return(config)
}

assign.column.attribute.string<-function(config, label, attribute, string){
  if (isSingleString(string)){
    if (string != ''){
    eval(str2expression(paste('config$columns$', label, '<- append(', 'config$columns$', label,', list(', attribute, '=\'', string, '\'))', sep='')))
    }
  }
  else{
    stop(paste('Warning: ',attribute,' must a single nonempty string.', sep=''))
  }
  return(config)
}

assign.column.attribute.boolean<- function(config, label, attribute, boolean){
  if(is.boolean(boolean)){
    eval(str2expression(paste('config$columns$', label, '<- append(', 'config$columns$', label,', list(', attribute,'=', boolean, '))', sep='')))
  } else if(boolean ==''){
    return(config)
  } else{
    stop('Warning: required must be a boolean.')
  }
}

assign.column.code.list <- function(config, label, code.list){
  if (is.boolean(code.list)){
    eval(str2expression(paste('config$columns$', label, '<- append(', 'config$columns$', label,', list(code_list=', code.list, '\'))', sep='')))
  } else if(is_valid_url(code.list)){
    eval(str2expression(paste('config$columns$', label, '<- append(', 'config$columns$', label,', list(code_list=', code.list, '\'))', sep='')))
  } else if (code.list == ''){
    return(config)
  } else{
    stop('Warning: code.list must be a boolean or a uri.')
  }
}

assign.themes <- function(config, themes){
  if(length(themes)>1)
    config$themes = themes
  else if(themes != '')
    config$themes = themes
  else if(themes == '')
    return(config)
  else
    stop('Warning: \'themes\' must be a single string or a vector of strings.')
  return(config)
}

assign.keywords <- function(config, keywords){
  if(length(keywords)>1)
    config$keywords = keywords
  else if(keywords!='')
    config$keywords = keywords
  else if(keywords == '')
    return(config)
  else
    stop('Warning: \'keywords\' must be a single string or a vector of strings.')
  return(config)
}

