
#Salmonella

#AMC and COT omitted because they're combination drugs reported as fractions that don't necessarily make sense in this context

#Salmonella
salmonella <-  function(source, which_year) { 
  wy <- which_year
  source %>%
  subset(GENUS == "S" & Year ==  wy) %>%
  select(GENUS,HOST_SPECIES, GEN, Str, AMC,  FOX, TIO, AXO, FIS,  COT, AZI, AMP,
         CHL,  CIP, NAL, TET) %>%
  transmute(genus = GENUS, host = HOST_SPECIES,
            gen = ifelse(GEN <= 4, "S", ifelse(GEN >= 16, "R", "I")),
            str = ifelse(Str <= 16, "S", ifelse(Str >= 32, "R", "I")),
            #amc.bp = ifelse(AMC <= 4, "S", ifelse(AMC >= 16, "R", "I")),
            cefoxitin = ifelse(FOX <= 8, "S", ifelse(FOX >= 32, "R", "I")),
            ceftiofur = ifelse(TIO <= 2, "S", ifelse(TIO >= 8, "R", "I")),
            ceftriaxone = ifelse(AXO <= 1, "S", ifelse(AXO >= 4, "R", "I")),
            sulfisoxazole = ifelse(FIS <= 256, "S", ifelse(FIS >= 512, "R", "I")),
            #trimsulf.bp = ifelse(COT <= 4, "S", ifelse(COT >= 16, "R", "I")),
            azi = ifelse(AZI <= 16, "S", ifelse(AZI >= 32, "R", "I")),
            amp = ifelse(AMP <= 8, "S", ifelse(AMP >= 32, "R", "I")),
            chloramphenicol = ifelse(CHL <= 8, "S", ifelse(CHL >= 32, "R", "I")),
            cip = ifelse(CIP <= 0.06, "S", ifelse(CIP >= 1, "R", "I")),
            nal = ifelse(NAL <= 16, "S", ifelse(NAL >= 32, "R", "I")),
            tet = ifelse(TET <= 4, "S", ifelse(TET >= 16, "R", "I")))
  }

# Ecoli

ecoli <- function(source, which_year) { 
  wy <- which_year
  source %>%
  subset(GENUS == "EC" & Year == wy) %>%
  select(GENUS, HOST_SPECIES, GEN, Str, AMC, FOX, TIO, AXO, FIS, COT, AZI, AMP,
         CHL, CIP, NAL, TET) %>%
  transmute(genus = GENUS, host = HOST_SPECIES, 
            gen = ifelse(GEN <= 4, "S", ifelse(GEN >= 16, "R", "I")),
            str = ifelse(Str <= 16, "S", ifelse(Str >= 32, "R", "I")),
            #amc.bp = ifelse(AMC <= 4, "S", ifelse(AMC >= 16, "R", "I")),
            cefoxitin = ifelse(FOX <= 8, "S", ifelse(FOX >= 32, "R", "I")),
            ceftiofur = ifelse(TIO <= 2, "S", ifelse(TIO >= 8, "R", "I")),
            ceftriaxone = ifelse(AXO <= 1, "S", ifelse(AXO >= 4, "R", "I")),
            sulfisoxazole = ifelse(FIS <= 256, "S", ifelse(FIS >= 512, "R", "I")),
            #trimsulf.bp = ifelse(COT <= 4, "S", ifelse(COT >= 16, "R", "I")),
            azi = ifelse(AZI <= 16, "S", ifelse(AZI >= 32, "R", "I")),
            amp = ifelse(AMP <= 8, "S", ifelse(AMP >= 32, "R", "I")),
            chloramphenicol = ifelse(CHL <= 8, "S", ifelse(CHL >= 32, "R", "I")),
            cip = ifelse(CIP <= 0.06, "S", ifelse(CIP >= 1, "R", "I")),
            nal = ifelse(NAL <= 16, "S", ifelse(NAL >= 32, "R", "I")),
            tet = ifelse(TET <= 4, "S", ifelse(TET >= 16, "R", "I")))
  }


#Enterococcus

entc <- function(source, which_year) {
  wy <- which_year
  source %>%
  subset(GENUS == "E" & Year == wy) %>%
  select(GENUS, HOST_SPECIES, GEN, KAN, Str, VAN, TGC, LIN, DAP, ERY, TYL, NIT, 
         LZD, PEN, CHL, CIP, QDA, TET) %>%
  transmute(genus = GENUS, host = HOST_SPECIES,
            gen = ifelse(GEN <= 500, "S", ifelse(GEN > 500, "R", "I")),
            kan = ifelse(KAN <= 512, "S", ifelse(KAN >= 1024, "R", "I")),
            strep = ifelse(Str <= 512, "S", ifelse(Str >= 1024, "R", "I")), 
            van = ifelse(VAN <= 4, "S", ifelse(VAN >= 32, "R", "I")),
            tgc = ifelse(TGC <=0.24, "S", "R"),
            lin = ifelse(LIN <= 2, "S", ifelse(LIN >= 8, "R", "I")), 
            dap = ifelse(DAP <= 4, "S", ifelse(DAP >= 8, "R", "I")),
            ery = ifelse(ERY <= .5, "S", ifelse(ERY >= 8, "R", "I")),
            tyl = ifelse(TYL <= 8, "S", ifelse(TYL >= 32, "R", "I")),
            nit = ifelse(NIT <= 32, "S", ifelse(NIT >= 128, "R", "I")),
            lzd = ifelse(LZD <= 2, "S", ifelse(LZD >= 8, "R", "I")),
            pen = ifelse(PEN <= 8, "S", ifelse(PEN >= 16, "R", "I")),
            chl = ifelse(CHL <= 8, "S", ifelse(CHL >= 32, "R", "I")),
            cip = ifelse(CIP <= 1, "S", ifelse(CIP >= 4, "R", "I")),
            qda = ifelse(QDA <= 1, "S", ifelse(QDA >= 4, "R", "I")), 
            tet = ifelse(TET <= 4, "S", ifelse(TET >= 16, "R", "I")))
}


# classes

salm_ecoli_class <- function(x) {
  x %>% transmute(
    genus = genus,
    host = host,
    aminoglycosides = ifelse(gen == "R" |
                               str == "R", "R", "S"),
    cephems = ifelse(cefoxitin == "R" |
                       ceftiofur == "R" |
                       ceftriaxone == "R", "R", "S"),
    folate_pw = ifelse(sulfisoxazole == "R", "R", "S"),
    macrolides = ifelse(azi == "R", "R", "S"),
    pencillins = ifelse(amp == "R", "R", "S"),
    phenicols = ifelse(chloramphenicol == "R", "R", "S"),
    quinolones = ifelse(cip == "R" |
                          nal == "R", "R", "S"),
    tetracyclines = ifelse(tet == "R", "R", "S")
  )
}

entc_class <- function(x) {
  x %>% transmute(
    genus = genus,
    host = host,
    aminoglycosides = ifelse(gen == "R" |
                               kan == "R" | strep == "R", "R", "S"),
    glycopeptides = ifelse(van == "R", "R", "S"),
    glycylcycline = ifelse(tgc == "R", "R", "S"),
    lincosamides = ifelse(lin == "R", "R", "S"),
    lipopeptides = ifelse(dap == "R", "R", "S"),
    macrolides = ifelse(ery == "R" | tyl == "R", "R", "S"),
    nitrofurans = ifelse(nit == "R", "R", "S"),
    oxazolidinones = ifelse(lzd == "R", "R", "S"),
    penicillins = ifelse(pen == "R", "R", "S"),
    phenicols = ifelse(chl == "R", "R", "S"),
    quinolone = ifelse(cip == "R", "R", "S"),
    streptogramins = ifelse(qda == "R", "R", "S"),
    tetracyclines = ifelse(tet == "R", "R", "S")
  )
}
# Resistant percentage

resistant <- function(df_name, colnum) {
  df_name[ , 3:colnum]  <- lapply(df_name[ , 3:colnum] , 
                                  FUN = function(x) recode(x, S= 0, R= 1, I = 0))
  df_name$rowsum <- rowSums(df_name[,3:colnum], na.rm=TRUE)
  df_name <- df_name %>% select(genus, host, rowsum) %>% 
    transmute(genus= genus, host = host, threeormore = ifelse(rowsum >= 3, "yes", "no")) %>% 
    group_by(genus, host, threeormore) %>% 
    count() %>% 
    spread(threeormore, n) %>% 
    mutate(percent = (yes/(yes + no))*100)
}

#combine the last two steps
salm_ecoli_resistant <- function(asdf, num_cols) {
  asdf <- salm_ecoli_class(asdf)
  asdf <- resistant(asdf, num_cols)
}

entc_resistant <- function(asdf, num_cols) {
  asdf <- entc_class(asdf)
  asdf <- resistant(asdf, num_cols)
}