#Like the previous model, the real power comes in the fact that we can run
#this many times to get an idea of what our player is doing! Lets run this
#model 100 times and keep track of each path that the player uses

Lx=list()
Ly=list()
for (j in 1:100)
{
	zx=sample(seq(50, 100, by=.5), 100, replace=TRUE)
	zy=sample(seq(50, 75, by=.5), 100, replace=TRUE)
	zs=sample(seq(.1,.5,by=.1),100,replace=TRUE)
	hx=75
	hy=100
	t=1
	Hx=hx
	Hy=hy
	while(hy>50)
		{
		if(min(sqrt(((zx-(hx))^2)+((zy-(hy))^2)))<.2) break
		t=t+1
			if(length(which(zx>hx-10 & zx <hx+10 & zy>hy-10 & zy<hy+10))==0){hx=hx; hy=hy-1}
			else{
			d1=min(sqrt(((zx-(hx+1))^2)+((zy-(hy-1))^2)))
			d2=min(sqrt(((zx-(hx-1))^2)+((zy-(hy-1))^2)))
			d3=min(sqrt(((zx-(hx))^2)+((zy-(hy-1))^2)))
			dvec=c(d1,d2,d3)
			if(length(which(dvec==max(dvec)))>1)
			{dvec[which(dvec==max(dvec))][1]=dvec[which(dvec==max(dvec))][1]+.1}
			if (max(dvec)==dvec[1]){hx=hx+1; hy=hy-1}
			if (max(dvec)==dvec[2]){hx=hx-1; hy=hy-1}
			if (max(dvec)==dvec[3]){ hy=hy-1}
			}
		dis=sqrt(((zx-(hx))^2)+((zy-(hy))^2))
		Nx=hx+(((dis-zs)/dis)*(zx-hx))
		Ny=hy+(((dis-zs)/dis)*(zy-hy))
		zx=Nx
		zy=Ny
		Hx=c(Hx,hx)
		Hy=c(Hy,hy)
		}
Lx[[j]]=Hx
Ly[[j]]=Hy
}

#Now plot the results!!

par(bg="grey")
plot(50,50, xlim=c(50,100), ylim=c(50,100))
for (i in 1:length(Lx))
	{
	lines(Lx[[i]],Ly[[i]],col='red',lwd=2)
	}

#What about when the player is caught?

ct=c()
for(i in 1:length(Lx))
	{
	if (min(Ly[[i]]) > 50) {ct[i]=1} else{ct[i]=0}
	}
lines(Lx[which(ct > 0)],Ly[which(ct > 0)],col='cyan',lwd=2)
for (i in 1:length(which(ct > 0)))
	{
	lines(Lx[[which(ct >0)[i]]],Ly[[which(ct > 0)[i]]],col='cyan',lwd=2)
	}


