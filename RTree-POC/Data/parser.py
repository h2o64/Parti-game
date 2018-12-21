import csv
import math
import sys
data_file = open('objectif-zero-pesticide-en-ile-de-france.csv','rt', encoding="utf8")
csv_file = csv.reader(data_file,delimiter=',',quotechar='|')

def get_raw():
	ret = []
	for row in csv_file:
		#if "Zéro pesticide total" in row[4]:
		if row[0] != 'dep':
			raw_polygon = ("".join(row[14:]))[19:-3]
			ret.append(raw_polygon)
	return ret

example_poly = "[[2.465176547057712 48.54147098158752] [2.445601112718157 48.546562921086846] [2.425165596050208 48.544477820202644] [2.402774167892405 48.563195660270864] [2.412753598869674 48.56521540050725] [2.422234048097816 48.573222979020514] [2.439394862714953 48.57706523154249] [2.442294558670131 48.57719579971013] [2.456355291836143 48.5638753819719] [2.456770495696642 48.55781418163532] [2.466471529200265 48.556661659750596] [2.468750459207878 48.55375724513956] [2.465176547057712 48.54147098158752]]"

def raw_to_value(raw_data):
	x,y = '',''
	startX,startY = False,False
	for s in raw_data:
		if s == '[': startX = True
		elif s == ' ':
			startY = True
			startX = False
		elif s == ']': break
		elif startX: x += s
		elif startY: y += s
	return [float(x),float(y)]

def raw_to_values(raw_data):
	buffer = ''
	ret = []
	for s in raw_data[1:-1]:
		buffer += s
		if s == ']':
			ret.append(raw_to_value(buffer))
			buffer = ''
	return ret

def polar_to_carthesian(vect,phi):
	r = 6371000 / 10000
	# phi = 48.856614
	return [int(r*vect[0]*math.cos(phi)),int(r*vect[1])]

def carthesian_polygon(poly,phi):
	return [polar_to_carthesian(x,phi) for x in poly]

def poly_to_rectangle(poly):
	poly.sort()
	return [poly[0],poly[-1]]

def poly_center(poly):
	sum_x = 0
	sum_y = 0
	for point in poly:
		sum_x += point[0]
		sum_y += point[1]
	return [int(sum_x/len(poly)),int(sum_y/len(poly))]

def list_to_array(l):
	return "[|" + str(l[0]) + ";" + str(l[1]) + "|]"

def adapt_poly(min_x,min_y,poly):
	for i in range(len(poly)):
		poly[i][0] -= min_x
		poly[i][1] -= min_y

def inv(l):
	return l #[l[1],l[0]]

def get_all_rectangles():
	ret = []
	raw_data = get_raw()
	count = 0
	big_list = '['
	# Get the average latitude
	avg_phi = 0
	avg_phi_count = 0
	for x in raw_data:
		values = raw_to_values(x)
		for val in values:
			avg_phi += val[0]
			count += 1
	avg_phi = avg_phi / count
	# Get the minimum
	min_x = sys.maxsize
	min_y = sys.maxsize
	for x in raw_data:
		poly = carthesian_polygon(raw_to_values(x),avg_phi)
		rect = poly_to_rectangle(poly)
		min_x = min(min_x,rect[0][0])
		min_y = min(min_y,rect[0][1])
	for x in raw_data:
		poly = carthesian_polygon(raw_to_values(x),avg_phi)
		adapt_poly(min_x,min_y,poly)
		rect = poly_to_rectangle(poly)
		center = poly_center(poly)
		print("let rect" + str(count) + " = Rect.create " + list_to_array(inv(rect[0])) + " " + list_to_array(inv(rect[1])) + ";;")
		print("let ind" + str(count) + " = { pos = " + list_to_array(inv(center)) + "; data = " + str(count) + "};;")
		print("let leaf" + str(count) + " = (rect" + str(count) + ",ind" + str(count) + ");;")
		big_list += "leaf" + str(count) + ";"
		count += 1
	big_list += ']'
	print("let db_test = " + big_list)

get_all_rectangles()

print("(* Build the tree *)")
print("let rec build_tree_test l cur_tree = match l with")
print("	| (r,idx)::t -> build_tree_test t (insert r idx cur_tree)")
print("	| [] -> cur_tree;;")
