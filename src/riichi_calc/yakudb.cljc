(ns riichi-calc.yakudb)

(def yakudb
  {:yakuhai {:wiki "https://riichi.wiki/Yakuhai"
             :name {:ja "役牌"
                    :romaji "Yakuhai"
                    :it "Valori"
                    :en "Value tiles"}}
   :dora {:wiki "https://riichi.wiki/Dora"
          :name {:ja "ドラ"
                 :romaji "Dora"
                 :it "Dora"
                 :en "Dora"}}
   :menzen-tsumo {:wiki "https://riichi.wiki/Menzenchin_tsumohou"
                  :name {:ja "門前清自摸和"
                         :romaji "menzenchin tsumohou"
                         :it "Pescata coperta"
                         :en "Self draw"}}
   :iipeikou {:wiki "https://riichi.wiki/Iipeikou"
              :name {:ja "一盃口"
                     :romaji "īpeikou"
                     :it "Doppia sequenza"
                     :en "Double sequences"}}
   :pinfu {:wiki "https://riichi.wiki/Pinfu"
           :name {:ja "平和"
                  :romaji "pinfu"
                  :it "Tutto sequenze"
                  :en "All sequences"}}
   :ippatsu {:wiki "https://riichi.wiki/Ippatsu"
             :name {:ja "一発"
                    :romaji "ippatsu"
                    :it "Al primo colpo"
                    :en "One shot"}}
   :houtei-raoyui {:wiki "https://riichi.wiki/Haitei_raoyue_and_houtei_raoyui"
                   :name {:ja "河底撈魚"
                          :romaji "houtei raoyui"
                          :it "Ultima pescata"
                          :en "Win by last draw"}}
   :haitei-raoyue {:wiki "https://riichi.wiki/Haitei_raoyue_and_houtei_raoyui"
                   :name {:ja "海底撈月"
                          :romaji "haitei raoyue"
                          :it "Ultima scartata"
                          :en "Win by last discard"}}
   :rinshan-kaihou {:wiki "https://riichi.wiki/Rinshan_kaihou"
                    :name {:ja "嶺上開花"
                           :romaji "rinshan kaihou"
                           :it "Pescata da muro morto"
                           :en "Dead wall draw"}}
   :chankan {:wiki "https://riichi.wiki/Chankan"
             :name {:ja "搶槓"
                    :romaji "chankan"
                    :it "Kan rubato"
                    :en "Robbing a kan"}}
   :tanyao {:wiki "https://riichi.wiki/Tanyao"
            :name {:ja "断幺九"
                   :romaji "tanyao"
                   :it "Tutte semplici"
                   :en "All simples"}}
   :riichi {:wiki "https://riichi.wiki/Riichi"
            :name {:ja "立直"
                   :romaji "Riichi"
                   :it "Mano pronta (riichi)"
                   :en "Ready hand (riichi)"}}
   :chiitoitsu {:wiki "https://riichi.wiki/Chii_Toitsu"
                :name {:ja "七対子"
                       :romaji "Chiitoitsu"
                       :it "Sette coppie"
                       :en "Seven pairs"}}
   :chantaiyao {:wiki "https://riichi.wiki/Chanta"
                :name {:ja "混全帯么九"
                       :romaji "honchantaiyaochuu"
                       :it "Terminale o onore in ogni gruppo"
                       :en "Terminal or honor in each group"}}
   :sanshoku-doujun {:wiki "https://riichi.wiki/Sanshoku_doujun"
                     :name {:ja "三色同順"
                            :romaji "Sanshoku doujun"
                            :it "Tre sequenze colorate"
                            :en "Three colored straight"}}
   :ittsu {:wiki "https://riichi.wiki/Ikkitsuukan"
           :name {:ja "一気通貫"
                  :romaji "Ikkitsuukan"
                  :it "Sequenza pura"
                  :en "Pure straight"}}
   :toitoi {:wiki "https://riichi.wiki/Toitoihou"
            :name {:ja "対々和"
                   :romaji "Toitoihou"
                   :it "Tutto tris"
                   :en "All triplets"}}
   :sanankou {:wiki "https://riichi.wiki/Sanankou"
              :name {:ja "三暗刻"
                     :romaji "Sanankou"
                     :it "Tre tris nascosti"
                     :en " Three concealed triplets"}}
   :sanshoku-doukou {:wiki "https://riichi.wiki/Sanshoku_doukou"
                     :name {:ja "三色同刻"
                            :romaji "Sanshoku doukou"
                            :it "Tre tris colorati"
                            :en "Three colored triplets"}}
   :sankantsu {:wiki "https://riichi.wiki/Sankantsu"
               :name {:ja "三槓子"
                      :romaji "Sankantsu"
                      :it "Tre kan"
                      :en "Three kans"}}
   :honroutou {:wiki "https://riichi.wiki/Honroutou"
               :name {:ja "混老頭"
                      :romaji "Honroutou"
                      :it "Terminali e onori"
                      :en "Terminals and honors"}}
   :shousangen {:wiki "https://riichi.wiki/Shousangen"
                :name {:ja "小三元"
                       :romaji "Shousangen"
                       :it "Tre piccoli draghi"
                       :en "Small three dragons"}}
   :double-riichi {:wiki "https://riichi.wiki/Daburu_riichi"
                   :name {:ja "両立直"
                          :romaji "Daburu riichi"
                          :it "Doppio pronto"
                          :en "Double ready"}}
   :honitsu {:wiki "https://riichi.wiki/Honiisou"
             :name {:ja "混一色"
                    :romaji "Honiisou"
                    :it "Colore misto"
                    :en "Half flush"}}
   :junchan-taiyao {:wiki "https://riichi.wiki/Junchantaiyaochuu"
                    :name {:ja "純全帯么九"
                           :romaji "Junchantaiyaochuu"
                           :it "Terminale in ogni gruppo"
                           :en "Terminal in each meld"}}
   :ryanpeikou {:wiki "https://riichi.wiki/Ryanpeikou"
                :name {:ja "二盃口"
                       :romaji "Ryanpeikou"
                       :it "Due doppie sequenze identiche"
                       :en "Two sets of identical sequences"}}
   :chinitsu {:wiki "https://riichi.wiki/Chiniisou"
              :name {:ja "清一色"
                     :romaji "Chiniisou"
                     :it "Colore puro"
                     :en "Full flush"}}
   :kokushi-musou {:wiki "https://riichi.wiki/Kokushi_Musou"
                   :name {:ja "国士無双"
                          :romaji "Kokushi musou"
                          :it "Tredici orfani"
                          :en " Thirteen orphans"}}
   :kokushi-musou-juusan-menmachi {:wiki "https://riichi.wiki/Kokushi_Musou"
                                   :name {:ja "国士無双１３面待ち"
                                          :romaji "Kokushi musou juusan menmachi"
                                          :it "Tredici orfani con 13 attese"
                                          :en " Thirteen orphans with 13-way wait"}}
   :suuankou {:wiki "https://riichi.wiki/Suuankou"
              :name {:ja "四暗刻"
                     :romaji "Suuankou"
                     :it "Quattro tris nascosti"
                     :en "Four concealed triplets"}}
   :suuankou-tanki {:wiki "https://riichi.wiki/Suuankou"
                    :name {:ja "四暗刻単騎"
                           :romaji "Suuankou tanki"
                           :it "Quattro tris nascosti con attesa singola"
                           :en "Four concealed triplets with pair wait"}}
   :daisangen {:wiki "https://riichi.wiki/Daisangen"
               :name {:ja "大三元"
                      :romaji "Daisangen"
                      :it "Tre grandi draghi"
                      :en "Big three dragons"}}
   :shousuushii {:wiki "https://riichi.wiki/Suushiihou"
                 :name {:ja "大四喜"
                        :romaji "Shousuushii"
                        :it "Quattro piccoli venti"
                        :en " Little four winds"}}
   :daisuushii {:wiki "https://riichi.wiki/Suushiihou"
                :name {:ja "大四喜"
                       :romaji "Daisuushii"
                       :it "Quattro grandi venti"
                       :en "Big four winds"}}
   :tsuuiisou {:wiki "https://riichi.wiki/Tsuuiisou"
               :name {:ja "字一色"
                      :romaji "Tsuuiisou"
                      :it "Tutti onori"
                      :en "All honors"}}
   :chuuren-poutou {:wiki "https://riichi.wiki/Chuuren_poutou"
                    :name {:ja "九連宝燈"
                           :romaji "Chuuren poutou"
                           :it "Nove cancelli"
                           :en "Nine gates"}}
   :junsei-chuuren {:wiki "https://riichi.wiki/Chuuren_poutou"
                    :name {:ja "純正九連宝燈"
                           :romaji "junsei chuuren poutou"
                           :it "Nove cancelli puri"
                           :en "Pure nine gates"}}
   :suukantsu {:wiki "https://riichi.wiki/Suukantsu"
               :name {:ja "四槓子"
                      :romaji "Suukantsu"
                      :it "Quattro kan"
                      :en "Four kans"}}
   :chinroutou {:wiki "https://riichi.wiki/Chinroutou"
                :name {:ja "清老頭"
                       :romaji "Chinroutou"
                       :it "Tutti terminali"
                       :en "All terminals"}}})
