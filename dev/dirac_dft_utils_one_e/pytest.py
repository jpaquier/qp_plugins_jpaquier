file_in = open("test.txt",'r')
file_out= open("test2.txt",'w')
lines = file_in.read()

c=0
while(c<len(lines)):
# print(lines[c:c+60])
  file_out.write(lines[c:c+60])
  c=c+60

file_in.close()
file_out.close()
