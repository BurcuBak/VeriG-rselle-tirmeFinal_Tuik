#2022 Trafik Kazalarında Ölen Kişilerin Cinsiyet ve Durumlarına Göre Dağılımı

install.packages('ggplot2')
library(ggplot2)


traffic_data <- data.frame(
  category = c("Yolcu_Kadin", "Yolcu_Erkek", "Sürücü_Kadin", "Sürücü_Erkek", "Yaya_Kadin", "Yaya_Erkek"),
  count = c(745, 917, 58, 2291, 416, 802) 
  
  
  # Pasta grafiği oluşturma ve kategorilerin sayılarını etiketleme
  ggplot(traffic_data, aes(x="", y=count, fill=category, label=count)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    geom_label(color = "black", size = 3, position = position_stack(vjust = 0.5)) +
    theme_void() +
    labs(title="2022 Trafik Kazalarında Ölen Kişilerin Cinsiyet ve Durumlarına Göre Dağılımı") +
    scale_fill_manual(values=c("#F08080", "#87CEFA", "#FA8072", "#6495ED", "#DDA0DD", "#90EE90")) +
    theme(legend.title = element_blank())
  ------------------------------------------------------------------------------
## Taşıt türleri ve sayıları
  vehicles <- c('Otomobil', 'Motosiklet', 'Kamyonet', 'Bisiklet', 'Minibüs', 'Çekici', 'Kamyon', 'Otobüs', 'Traktör', 'İş Makinesi', 'Scooter', 'At Arabası', 'Diğer')
  fatalities <- c(832, 791, 216, 107, 13, 102, 81, 15, 130, 5, 8, 2, 44) # Ölüm yüzdeleri
  injuries <- c(48709, 61842, 12337, 7925, 1202, 2342, 1898, 491, 1237, 83, 1554, 48, 2127) # Yaralanma sayıları
  
  # Veri çerçevesini oluşturun
  data <- data.frame(vehicles, fatalities, injuries)
  
  # Grafik oluşturun
  ggplot(data, aes(x=vehicles, y=fatalities, fill='Ölümler')) +
    geom_bar(stat='identity') +
    geom_bar(aes(y=injuries, fill='Yaralanmalar'), position='dodge') +
    scale_fill_manual(values=c('Ölümler'='red', 'Yaralanmalar'='blue')) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    labs(x='Taşıt Türleri', y='Yüzde (%)', fill='Tür', title='Kazaya Karışan Taşıtların Türlerine Göre Ölüm ve Yaralanma Sayıları')
--------------------------------------------------------------------------------
    
  ### Veri çerçevesini oluşturun
    data <- data.frame(vehicles, fatalities, injuries)
  
  # Taşıt türlerine göre ölüm ve yaralanma sayılarını çizgi grafiği olarak göstermek için ggplot2 kullanın
  ggplot(data, aes(x = vehicles)) +
    geom_line(aes(y = fatalities, group = 1, colour = "Ölümler"), size = 1) +
    geom_line(aes(y = injuries, group = 1, colour = "Yaralanmalar"), size = 1) +
    scale_colour_manual(values = c("Ölümler" = "red", "Yaralanmalar" = "blue")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Taşıt Türlerine Göre Ölüm ve Yaralanma Sayıları",
         x = "Taşıt Türleri",
         y = "Sayılar",
         colour = "Kaza Türü")
  ------------------------------------------------------------------------------
  
  ### Taşıt türleri ve sayıları
    vehicles <- c('Otomobil', 'Motosiklet', 'Kamyonet', 'Bisiklet', 'Minibüs', 'Çekici', 'Kamyon', 'Otobüs', 'Traktör', 'İş Makinesi', 'Scooter', 'At Arabası', 'Diğer')
  fatalities <- c(832, 791, 216, 107, 13, 102, 81, 15, 130, 5, 8, 2, 44) # Ölüm sayıları
  injuries <- c(48709, 61842, 12337, 7925, 1202, 2342, 1898, 491, 1237, 83, 1554, 48, 2127) # Yaralanma sayıları
  
  # Gerekli paketleri yükleyin
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Veri çerçevenizi oluşturun
  data <- data.frame(vehicles, fatalities, injuries)
  
  # 'vehicles' sütununu faktör olarak dönüştürün
  data$vehicles <- factor(data$vehicles)
  
  # Uzun formata dönüştürmek için 'tidyr' paketini kullanın
  data_long <- gather(data, key = "Kaza Türü", value = "Sayılar", -vehicles)
  
  # Her bir taşıt türü için benzersiz bir renk paleti oluşturun
  renk_paleti <- rainbow(length(unique(data_long$vehicles)))
  
  # Gruplandırılmış çubuk grafikleri için ggplot kullanın
  ggplot(data_long, aes(x = vehicles, y = Sayılar, fill = vehicles)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.7) +
    geom_text(aes(label = Sayılar), 
              position = position_dodge(width = 0.75), 
              vjust = -0.9, 
              size = 2.5) +
    scale_fill_manual(values = renk_paleti) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Taşıt Türlerine Göre Ölüm ve Yaralanma Sayıları",
         x = "Taşıt Türleri",
         y = "Sayılar",
         fill = "Taşıt Türü")
  ------------------------------------------------------------------------------
    
    ##Günlere göre kazalar
    # 'Gün' sütununu faktör olarak ayarlayın
    data$Gün <- factor(data$Gün, levels = c('Pazartesi', 'Salı', 'Çarşamba', 'Perşembe', 'Cuma', 'Cumartesi', 'Pazar'))
  
  # Veri çerçevesini uzun formata dönüştürün
  data_long <- melt(data, id.vars = 'Gün', measure.vars = c('Ölü Sayısı', 'Yaralı Sayısı'))
  
  # Yığılmış histogram için ggplot kullanın
  p <- ggplot(data_long, aes(x=Gün, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge') +
    scale_fill_manual(values=c('red', 'blue'), labels=c('Ölü Sayısı', 'Yaralı Sayısı')) +
    theme_minimal() +
    labs(title='Haftanın Günlerine Göre Ölü ve Yaralı Sayısı', x='Gün', y='Sayı') +
    guides(fill=guide_legend(title="Efsane"))
  
  # Sütunların üzerine değerleri yazdırın
  p + geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
    ylim(0, 60000)
  
  ------------------------------------------------------------------------------
    #İllere Göre Kazalar
    
    install.packages("devtools")
  library(devtools)
  
  devtools::install_github("htastan/TRmaps")
  library(TRmaps)
  
  # Ayrıca {sf} paketini yüklemek gerekiyor. Bunun nedeni spatial
  # tipteki verileri görselleştirebilmektir.
  install.packages("sf")
  library(sf)
  install.packages('dplyr')
  library(dplyr)
  
  # tr_nuts3 verisi, Türkiye'de il düzeyinde görselleştirme 
  # yapabilmek için gerekli olan bilgileri taşır.
  data("tr_nuts3")
  
  
  # il düzeyinde basit bir görsel oluşturalım.
  ggplot(tr_nuts3) + 
    geom_sf()
  
  
  # Yalnızca şehir ve kaza endeksi değişkenlerinden oluşan
  # bir veriseti oluşturulması
  tr_kaza <- veri |> 
    select(İl, Kaza_Sayısı)
  
  # OLuşturulan veri setinin spatial veri noktaları ile birleştirilmesi
  tr_kaza2 <- left_join(tr_nuts3, veri, by = c("name_tr" = "İl"))
  
  #kaza ile birleştirilmesi
  ggplot(tr_kaza2) + 
    geom_sf(aes(fill = Kaza_Sayısı)) +
    theme_void()
  # tr_nuts3 verisi ile veri setini birleştirme
  tr_kaza2 <- left_join(tr_nuts3, veri, by = c("name_tr" = "İl"))
  
  # Türkiye haritası üzerinde kaza sayılarını ve il isimlerini görselleştirme
  ggplot(tr_kaza2) +
    geom_sf(aes(fill = Kaza_Sayısı)) + # Kaza sayılarına göre renklendirme
    geom_sf_text(aes(label = Kaza_Sayısı), colour = "red", size = 2.5, nudge_y = -0.3) + # Kaza sayılarını beyaz renkte ve küçük boyutta yazı
    geom_sf_text(aes(label = name_tr), colour = "black", size = 3, nudge_y = -0.1) + # İl isimlerini siyah renkte yazı
    scale_fill_gradient(low = "lightblue", high = "blue", na.value = "transparent") + # Renk skalası
    theme_void()