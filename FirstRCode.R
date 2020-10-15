#concatenation
a="how"
b="are"
c="you"
paste0(a,b,c)

#subtsr sytax (str, start position, stop position) output includes end position
# start pos starts with 1 and not 0 like in Python, c etc

# sub and gsub - for substitute function with g standing for global.

x=c(1,4,5,8,12,67)
y=seq(10, 15, length=5) # sequence generater with length or by operator
x+y # if they are of diff legth then R repeats the shorter vector. If longer 
# vector is not a multiple of shorter vector then it throws warning message.
#another ex:
paste0(c("a","b"), 1:10)
z=rep(c("a","b"),4) # we can use "each" operator to repeat each value
paste(z, collapse = "-")


x=c("a", "d", "1", "6", "g")
y= letters

match(x, y)  #gives position of matching attribute
x%in%y  #gives True/False for match

i=2:25
i%%2==0
which(i%%2==0)
i[which(i%%2==0)]

# sort(), rev()

set.seed(5)
sample(i, 4) # when we have set the seed, whenever we re-run the sample with 
# same seed we start getting same output
# above is an example of sampling without replacement which is default behaviour

mysample = sample(i, 10, replace = T) # with replacement, where entire dataset is available 
# for selection every time causing repeatitions. Like Dice roll.
table(mysample) # create cross tab for distinct values and the count for each.

# unique()
# constants like letters, LETTERS, month.abb, month.name, 

runif(5) # this is random uniform funct. it give random values btw 0 and 1 
# uniformly distributed

# list() can be of diff lenths, the data frames are specific case of list 
# where all vectors should be of same length.

l1 = sample(c(1:50), 10, replace = T)
l2 = sample(letters, 10, replace = T)
mylist = list(l1, l2)
names(mylist) = c("Vector1", "Vector2") 
mylist[1:5]


# comment shortcut ctr+shift+c
# execution shortcut ctrl+enter