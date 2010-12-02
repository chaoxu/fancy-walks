importPackage(java.io);
importPackage(java.lang);
var br = new BufferedReader(new InputStreamReader(System['in']));
var line = br.readLine();
while (line != '42') {
	print(line);
	line = br.readLine();
}

