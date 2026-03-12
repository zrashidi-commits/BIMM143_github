# class19
Zahra Rashidi (A18561538)

``` r
library(bio3d)

a <-read.fasta("A18561538_mutant_seq.fa")
a
```

                   1        .         .         .         .         .         60 
    wt_healthy     MTAIIKEIVSRNKRRYQEDGFDLDLTYIYPNIIAMGFPAERLEGVYRNNIDDVVRFLDSK
    mutant_tumor   MTAIIKEIVSRNKRRYQEDGFDLDLTYIYPNIIAMGFPAERLEGVYRNNIDDVVRFLDSK
                   ************************************************************ 
                   1        .         .         .         .         .         60 

                  61        .         .         .         .         .         120 
    wt_healthy     HKNHYKIYNLCAERHYDTAKFNCRVAQYPFEDHNPPQLELIKPFCEDLDQWLSEDDNHVA
    mutant_tumor   HKNHYKIYNLCAERHYDTAKFNCRVAQYPFEDHNPPQLELIKPFCEVLDQELSRDDNHVA
                   ********************************************** *** ** ****** 
                  61        .         .         .         .         .         120 

                 121        .         .         .         .         .         180 
    wt_healthy     AIHCKAGKGRTGVMICAYLLHRGKFLKAQEALDFYGEVRTRDKKGVTIPSQRRYVYYYSY
    mutant_tumor   AIHCKAGKGYTGVMICAYLLHRGKFLKAQEALDFYGEVRTRDKKGVTIPSQRRYVYYYSY
                   ********* ************************************************** 
                 121        .         .         .         .         .         180 

                 181        .         .         .         .         .         240 
    wt_healthy     LLKNHLDYRPVALLFHKMMFETIPMFSGGTCNPQFVVCQLKVKIYSSNSGPTRREDKFMY
    mutant_tumor   LLKNHLDYRPVALLFHKMMFETIPMFSGGTCNPQFVVCQLKVKIYSSNSGPTRREDKFMY
                   ************************************************************ 
                 181        .         .         .         .         .         240 

                 241        .         .         .         .         .         300 
    wt_healthy     FEFPQPLPVCGDIKVEFFHKQNKMLKKDKMFHFWVNTFFIPGPEETSEKVENGSLCDQEI
    mutant_tumor   FEFPQPLPVCGDIKVEFFHKQNKMLKKDKMFHFWVNTFFIPGPEETSEKVENGSLCDQEI
                   ************************************************************ 
                 241        .         .         .         .         .         300 

                 301        .         .         .         .         .         360 
    wt_healthy     DSICSIERADNDKEYLVLTLTKNDLDKANKDKANRYFSPNFKVKLYFTKTVEEPSNPEAS
    mutant_tumor   DSICSIERADNDKEYLVLTLTKNDLDKANKDKANRYFSPNFKVKLYFTKTVEEPSNPEAS
                   ************************************************************ 
                 301        .         .         .         .         .         360 

                 361        .         .         .         .  403 
    wt_healthy     SSTSVTPDVSDNEPDHYRYSDTTDSDPENEPFDEDQHTQITKV
    mutant_tumor   SSTSVTPDVSDNEPDHYRYSDTTDSDPENEPFDEDQHTQITKV
                   ******************************************* 
                 361        .         .         .         .  403 

    Call:
      read.fasta(file = "A18561538_mutant_seq.fa")

    Class:
      fasta

    Alignment dimensions:
      2 sequence rows; 403 position columns (403 non-gap, 0 gap) 

    + attr: id, ali, call
