tabledims <- function(
    df = draaitabel1, 
    caption = NULL,
    gekozen_systeem = c("r", "g", "m"), 
    gekozen_zonering = c(
      'Aquatisch',        
      'Overstromingsvrije zone',       
      'Oevers',                        
      'Oevers/overstromingsvrije zone',
      'Oeverwal/hoge uiterwaard',      
      'Hoge uiterwaard'
    )
) {
  
  draaitabel2 <- draaitabel1 %>%
    # filter regels uit de tabel op basis van inhoud
    filter(if(!is.null(gekozen_systeem)) systeem %in% gekozen_systeem else TRUE) %>%
    filter(if(!is.null(gekozen_zonering)) ZONERING %in% gekozen_zonering else TRUE) %>%
    # filter(systeem %in% gekozen_systeem) %>%
    filter(ZONERING %in% gekozen_zonering) %>%
    # verwijder kolommen met alleen NA waarden
    select_if(~sum(!is.na(.)) > 0)
  
  dim(draaitabel2)
  
}



maak_kenmerkentabel <- function(
    df = draaitabel1, 
    caption = NULL,
    gekozen_systeem = c("r", "g", "m"), 
    gekozen_zonering = c(
      'Aquatisch',        
      'Overstromingsvrije zone',       
      'Oevers',                        
      'Oevers/overstromingsvrije zone',
      'Oeverwal/hoge uiterwaard',      
      'Hoge uiterwaard'
    )
) {
  
  draaitabel2 <- draaitabel1 %>%
    # filter regels uit de tabel op basis van inhoud
    filter(if(!is.null(gekozen_systeem)) systeem %in% gekozen_systeem else TRUE) %>%
    filter(if(!is.null(gekozen_zonering)) ZONERING %in% gekozen_zonering else TRUE) %>%
    # filter(systeem %in% gekozen_systeem) %>%
    filter(ZONERING %in% gekozen_zonering) %>%
    # verwijder kolommen met alleen NA waarden
    select_if(~sum(!is.na(.)) > 0)
  # # kombineer waarden voor systeem per ecotoop op een regel
  # pivot_wider(names_from = systeem, values_from = systeem) %>%
  # replace(is.na(.), "") %>%
  # unite("systeem", any_of(c('g','r','m')), sep = "") %>%
  # # zet kolom systeem weer op de goede plek
  # relocate(systeem, .after = STRUCT_COD) %>%
  # verwijder onnodige kolommen
  
  # draaitabel2 %>%
  #   # maak mooie tabel voor website rapportage
  #   kableExtra::kable(caption = 'Draaitabel voor zonering "Oevers"') %>%
  #   # kableExtra::kable_classic_2() %>%
  #   kableExtra::kable_styling("striped", full_width = T) %>%
  #   kableExtra::row_spec(0, angle = -90, align = "c")
  
  
  datatable(
    draaitabel2, 
    caption = caption,
    rownames = F, 
    class = c("compact"),
    options = list(
      paging = F, 
      autoWidth = F,
      searching= FALSE,
      scrollX = F,
      initComplete = JS("function(settings, json) {
                                  $(this.api().table().header()).css({
                                    'font-size' : '12px'});}"),
      headerCallback = JS("function(thead, data, start, end, display){
                                  var $ths = $(thead).find('th');
                                  $ths.css({'vertical-align': 'top', 'padding': '4px 0px', 
                                            'transform': 'rotate(180deg)', 'border': 'none'}); 
                                  var betterCells = [];
                                  $ths.each(function(){
                                    var cell = $(this);
                                    var newDiv = $('<div>', {width: '13px', float: 'left'});
                                    var newInnerDiv = $('<div>', {text: cell.text()});
                                    newDiv.css({margin: 'auto'});
                                    newInnerDiv.css({
                                      'writing-mode': 'vertical-rl',
                                      'white-space': 'nowrap',
                                      'text-align': 'left',
                                      'transform-origin': 'top left',
                                      'transform': 'rotate(0deg)',
                                      'overflow': 'visible'
                                    });
                                    newDiv.append(newInnerDiv);
                                    betterCells.push(newDiv);
                                  });
                                  $ths.each(function(i){
                                    $(this).html(betterCells[i]);
                                  });}")
    ),
    height = "auto" #200
  ) %>% 
    formatStyle(columns = c(1:ncol(draaitabel2)), `font-size` = '12px')
}
