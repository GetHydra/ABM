tvec=c()
for(j in 1:100)
{
	zx=sample(seq(50, 59, by=.5), 100, replace=TRUE)
	zy=sample(seq(50, 100, by=.5), 100, replace=TRUE)
	zs=sample(seq(.1,1,by=.1),100,replace=TRUE)
	t=0
		while(max(zx)<100)
			{
				t=t+1
					for (i in 1:length(zx))
						{
							if(zx[i] <= 60 & zy[i] >= 75)
							{
								zx[i]=zx[i]+zs[i]
								zy[i]=zy[i]-zs[i]
								if(zx[i] > 60) {zx[i]=59.5}
							}else if (zx[i] <= 60 & zy[i] <= 70)
							{
								zx[i]=zx[i] + zs[i]
								zy[i]=zy[i] + zs[i]
								if (zx[i] > 60) {zx[i]=59.5}
								} else if (zy[i] > 70 & zy[i] < 75) {zx[i]=zx[i]+zs[i]}
							}
							}
	tvec[j]=t
}


plot(density(tvec), main="Distribution Of Time To Door")
abline(v=mean(tvec),col="red")
