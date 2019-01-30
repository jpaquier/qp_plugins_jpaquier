file_in = open("text.dat",'r')
file_out= open("text2.dat",'w')
lines = file_in.read()

c=0
while(c<len(lines)):
  print(lines[c:c+60])
  c=c+60
#file_out.write(lines[0:10])

file_in.close()
file_out.close()
