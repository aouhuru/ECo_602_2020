Read Data

bird.sub= read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sub.csv")
hab.sub= read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sub.csv")

birdhab= data.frame(merge(bird.sub$basin, hab.sub$sub))
birdhab1= merge(bird.sub$sub, hab.sub$basin)
dim(birdhab1)

birdhab= merge(basin * sub , data= bird.sub+ hab.sub)