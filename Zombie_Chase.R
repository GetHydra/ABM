#Lets look at something more complex. What if you know that you will at
#some point need to run through a group of zombies to reach safety. How
#many zombies is too many? That is to say how many zombies will there have
#to be before you can no longer duck and weave your way to safety?
#This is a perfect job for a ABM!!
#This will need to be a far more complex model, we need to model zombies
#who need to be trying to catch a “player” and we need to model a “player”
#who is trying NOT to be caught!
#Luckily we can use equations! Yay math!! We can set equations for the
#zombies such that they are always trying to minimize their distance between
#themselves and the player. Then for the player we can use an equation to
#maximize the distance between them and the zombies!
#Lets look at our space!

plot(50,50, xlim=c(50,100), ylim=c(50,100))

#Now like before we create zombies and their speeds

zx=sample(seq(50, 100, by=.5), 100, replace=TRUE)
zy=sample(seq(50, 75, by=.5), 100, replace=TRUE)
zs=sample(seq(.1,.5,by=.1),100,replace=TRUE)

#Add them in!

points(zx,zy, col='darkgreen', pch=16)

#Lets set the player at the top middle

hx=75

hy=100

#And set our time to 1

t=1

#For this model we need to keep track of some extra things, so we make
#copies of variables to pass to loop

Hx=hx
Hy=hy

#Now begin our simulation, and stop if the player makes it to the bottom, or
#stop it if he is caught by the zombies!!

while(hy>50)
{
	if(min(sqrt(((zx-(hx))^2)+((zy-(hy))^2)))<.2) break
	t=t+1
	points(zx,zy, col='darkgreen', pch=1, cex=.5)
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
	Sys.sleep(.3)
	Hx=c(Hx,hx)
	Hy=c(Hy,hy)
	lines(Hx,Hy,col='blue',lwd=2)
}





