import Char
g x|x<'0'||elem x"aoyeui"=""|1>0='.':[x]
main=interact$concatMap (g.toLower)
