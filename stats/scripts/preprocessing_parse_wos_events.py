import csv


reader = csv.DictReader(open("../data/raw/all_2008.txt", "r"), delimiter="\t", quoting=csv.QUOTE_NONE)
writer = open("../data/derived/wos_2008_events_raw.txt", "w")
writer.write("journalRaw\tid\tyearOfCitation\tyearOfCiter\tmonthOfCiter\tdateOfCiter\ttitleOfCiter\n")

titles = []
i = 0
a = reader.next()
good = True
while (a):
	try:
		a = reader.next()
		print i
		i = i+1
		if a['TI'] not in titles:
			titles.append(a['TI'])
			#print(a['PD'])
			#print(a['PY'])
			#print(a['DI'])
			#print(a['TI'])
			#print(a['CR'])
			refs = a['CR'].split(";")
			plos_refs = [ref for ref in refs if 'PLOS' in ref]
			for plos_ref in plos_refs:
				ref_parts = plos_ref.split(",")
				#print ref_parts
				try:
					yearOfCitation = ref_parts[1].strip()
					journal = ref_parts[2].strip()
					number = ref_parts[-1].split(" ")[-1]
					year = a['PY']
					month = a['PD'].split(" ")[0]
					if len(a['PD'].split(" ")) > 1:
						days = a['PD'].split(" ")[1]
					else:
						days = "NA"
					line = journal + "\t" + number + "\t" + yearOfCitation + "\t" + year + "\t"+ month + "\t" + days + "\t" + a["TI"]
					print line
					writer.write(line + "\n")
				except IndexError:
					print("not valid biblio")
		else:
			print("dup!")
		#print("\n")
	except:
		pass
		
writer.flush()
writer.close()



