#The zombie apocalypse is upon us......but you know they way out!

	plot (50, 50, xlim=c(50, 100), ylim=c(50,100))
	lines(c(60,60), c(40, 70))
	lines(c(60, 60), c(75, 120))

#We want to know how much time we have to unlock the door and escape
#before the zombies reach us! So the first order of business is to generate
#some zombies!

	zx=sample(seq(50, 59, by=.5), 100, replace=TRUE)
	zy=sample(seq(50, 100, by=.5), 100, replace=TRUE)

#Now we will assume that the zombies do not all move at the same speed, so
#lets generate some variation in their ability to move!

	zs=sample(seq(.1,1,by=.1),100,replace=TRUE)

#Great, lets add them to our space!

	points(zx,zy)

#Before we start our model, lets set our time to zero

	t=0

#We will run the simulation until the zombies reach the other side, keeping
#track of the time 

	while(max(zx)<100)
		{
			points(75,100,pch=16,col='white',cex=6)
			points(zx,zy,pch=16,col='white',cex=6)
			lines(c(60,60),c(40,70))
			lines(c(60,60),c(75,120))
		###time goes up by 1 for each iteration
			t=t+1
		###loop to update plot with new positions of zombies
			for (i in 1:length(zx))
				{
					#points(75,100,pch=16,col='white',cex=4)
					#points(zx,zy,col='white')
					# lines(c(60,60),c(40,70))
					
                                        #lines(c(60,60),c(75,120))
					###here is the logic to deal with the wall
					if(zx[i] <= 60 & zy[i] >= 75){
					zx[i]=zx[i]+zs[i]
					zy[i]=zy[i]-zs[i]
					if(zx[i] > 60) {zx[i]=59.5}
					}else if (zx[i] <= 60 & zy[i] <= 70)
				{
			zx[i]=zx[i] + zs[i]
			zy[i]=zy[i] + zs[i]
			if (zx[i] > 60) {zx[i]=59.5}
			}else if (zy[i] > 70 & zy[i] < 75) {zx[i]=zx[i]+zs[i]}
				}
			##stuff for Windows to deal with plotting continuously
		points(zx,zy,col='red')
		text(75,100,t)


		Sys.sleep(.1)
		}





