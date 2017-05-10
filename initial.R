library(tidyverse)
library(stringr)

kv_2017_teat_maa <- read_delim("C:/Kannu/mystuff/abcdelections/kv-2017_teat_maa.csv",
";", escape_double = FALSE, col_names = FALSE,
locale = locale(encoding = "ISO-8859-1"),
trim_ws = TRUE)

guess_encoding("kv-2017_teat_maa.csv")

hel <- filter(kv_2017_teat_maa,X6=="HEL")

tmpK <- hel %>% 
  filter(X5=="****") %>% 
  mutate(s=as.numeric(X35)) %>% 
  group_by(X15) %>% 
  mutate(m=as.numeric(X35)) %>% 
  select(X15,m)

tmpA <- hel %>% 
  filter(X5!="****") %>% 
  group_by(X15) %>% 
  summarise(s=sum(as.numeric(X35)))

summary((full_join(tmpK,tmpA) %>% mutate(sama=s==m))$sama)

table((hel %>% filter(X4=="K"))$X5=="****")
# eli poimi vaan X4==K eli koko kunnan tulokset

tmp <- hel %>% select(X10,X11,X12) %>% distinct
#X12 on lista jonka alkupäätä meidän hypoteesi koskee
#X11 olis vaaliliitto, mutta ei kai niitä kukaan kato?

tmp <- kv_2017_teat_maa %>% select(X3) %>% distinct

# eli X3 on kunta, X4:stä filtteröidään ==K,
# X10 on listan numero, X15 on ehdokasnumero, X35 on äänimäärä


d <- kv_2017_teat_maa %>% 
  filter(X4=="K") %>% 
#  filter(X6=="HEL") %>% 
  select(X3,X10,X12,X15,X35,X19,X18) %>%
  transmute(kunta=X3,
            lista=X10,
            puolue=X12,
            kl=str_c(kunta,lista,sep="."),
            ehdokas=as.numeric(X15),
            aania=as.numeric(X35),
            nimi=str_c(X19,X18,sep=" ")) %>% 
  group_by(kl) %>% 
  arrange(ehdokas) %>% 
  mutate(rank=rank(ehdokas),
         namerank=rank(nimi),
         n=n(), #listan koko
         relrank=cume_dist(ehdokas),
         namerelrank=cume_dist(namerank),
         votesum=sum(aania), #listan summa
         voteprop=aania/votesum) %>%
  #filter(n>1) %>% 
  arrange(kunta,lista,ehdokas) %>% 
  ungroup() 



listat <- d %>% 
  select(kl,rank,namerank) %>%
  group_by(kl) %>% 
  summarise(rank_agree=all(rank==namerank),
            agree_prop=mean(rank==namerank))

d <- d %>% left_join(listat)

if(TRUE) {
d <- d %>% filter(rank_agree) %>% 
  mutate(gnamerank=rank(nimi))
} else {
d <- d %>% mutate(gnamerank=rank(nimi))
}



ggplot(d)+geom_col(aes(cut_number(gnamerank,5),aania))+coord_flip()

tuningp <- 0.2
Ts <- (d %>% 
  mutate(smallrank=gnamerank<=tuningp*nrow(d)) %>% 
  group_by(smallrank) %>% 
  summarise(n=n(),vsum=sum(aania)) %>% 
  filter(smallrank))$vsum

p <- rep(0,1000)
for(i in seq_along(p)) p[i] <-sum(sample_frac(d,tuningp)$aania) 
summary(p)
tibble(p=p) %>% ggplot(aes(p))+
  geom_density()+
  geom_vline(xintercept = Ts)

tmp <- d %>% filter(rank==1) %>% 
  mutate(kerroin=aania/(votesum/n))
median(tmp$kerroin)
ecdf(tmp$kerroin)(1)
mean(tmp$kerroin)


tmp <- d %>% mutate(alku=rank<=n/2,
                    loppu=rank>(n/2+0.5)) %>% 
  filter(alku|loppu) %>% 
  group_by(kl) %>% 
  summarise(vsumalku=sum(aania*alku),
            vsumloppu=sum(aania*loppu)) %>% 
  mutate(agreeH=vsumalku>vsumloppu,
         disagreeH=vsumalku<vsumloppu,
         sama=vsumalku==vsumloppu)
summary(tmp)

pbinom(752,752+675,0.5,lower.tail = FALSE)
