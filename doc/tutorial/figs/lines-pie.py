#!/usr/bin/env python

from pylab import *

# create figure
figwidth  = 10   # inches
figheight = 10   # inches
figure(1, figsize=(figwidth, figheight))
rcParams['font.size'] = 40.0
rcParams['axes.titlesize'] = 24.0
rcParams['xtick.labelsize'] = 24.0
rcParams['legend.fontsize'] = 28.0
# 568 aggregates
# 464 state
explode=(0.03, 0.06, 0.04, 0.05, 0.0)
colors=('r','b','y','m','g')
Ncols = 1
plotheight = figwidth/Ncols
H = plotheight/figheight
W = 1.0 / Ncols
margin = 0.05
left = [W*margin, W*(1+margin), W*(2+margin)]
bottom = H*2*margin
width = W*(1-2*margin)
height = H*(1-2*margin)

tot = 5200.0
ops = (1329.0 / tot)*100.0
state = (464.0 / tot)*100.0
aggregates = (568.0 / tot)*100.0
backends = (1442.0 / tot)*100.0
core = 100.0 - (backends + ops + aggregates + state)
fracs = [ops, state, aggregates, backends, core]
axes([left[0], bottom, width, height])
patches = pie(fracs, colors=colors, explode=explode, autopct='%1.f%%', shadow=True)
# title('')
legend((patches[0], patches[0], patches[0], patches[0], patches[0], patches[0]),
       ('ops', 'state', 'aggregates', 'backends', 'etc'), loc=(0,-.05))

savefig('linecount')
show()
