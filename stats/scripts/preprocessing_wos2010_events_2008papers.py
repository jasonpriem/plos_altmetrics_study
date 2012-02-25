## Take in the "Cited by" exports from ISI Web of Science and export a file of one-to-one associations between each Cited By and PLoS article

import csv
import os

reader = csv.DictReader(open("../data/raw/wos_plos_2008_citedby.txt", "r"), delimiter="\t", quoting=csv.QUOTE_NONE)
writer = open("../data/derived/events_raw_wos_2008.txt", "w")
writer.write("journalRaw\tid\tyearOfCitation\tyearOfCiter\tmonthOfCiter\tdateOfCiter\ttitleOfCiter\n")

titles = []
i = 0
a = reader.next()
good = True
print "starting long loop"
while (a):
	try:
		a = reader.next()
	except StopIteration:
		break
	except csv.Error:
	    print "csv import error, skipping one"
	    continue
	#print i
	#print ".",
	i = i+1
	# there are duplicate extractions, so be careful not to count things twice
	if a['TI'] not in titles:
		titles.append(a['TI'])
		# get the list of references and split them into a list of individual references
		refs = a['CR'].split(";")
		# only keep ones that have PLOS in them
		plos_refs = [ref for ref in refs if 'PLOS' in ref]
		for plos_ref in plos_refs:
			# parse out bits of the reference
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
				#print line
				writer.write(line + "\n")
			except IndexError:
				#print("not valid biblio")
				pass
	else:
		#print("dup!")
		pass
	#print("\n")

print "finished long loop"
		
writer.flush()
writer.close()

print("gzip ../data/derived/events_raw_wos_2008.txt")
os.system("gzip ../data/derived/events_raw_wos_2008.txt")
# then copy from derived to raw, so it can be the publicly available version

