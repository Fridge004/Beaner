#-------------------------------------------------------------------------------
# Copyright (c) 2010 Oliver Tearne (tearne at gmail dot com).
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
prob <- rep(1,100)
for(i in 2:100){
	prob[i] = prob[i-1]*0.99+(1-prob[i-1])*0.01	
}
plot(1:100, prob)

## mean percentage in
a=0.01
1/3*(a*(1-a)^2+a^3+a^2*(1-a))+
2/3*(a*(1-a)^2+2*a^2*(1-a))+
(1-a)^3

(0.99+0.9802+0.970596)/3
