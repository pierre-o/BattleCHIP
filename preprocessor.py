import collections
import re
import sys

# This file needs serious cleanup.

# The only part of the emulator using this preprocessor is the decode_execute
# function in cpu.ml.

n = 0

def replace(line):
	global n

	if line[-1] not in {'\n'}:
		assert n == 0
		n += 1

	match = re.search('(.*)0X([A-F,a-z,0-9,_]{4}) ->(.*)', line)
	if match is None:
		return line
	before = match.group(1)
	s = match.group(2)
	code = match.group(3)

	vars = collections.defaultdict(lambda: '0000')
	mask = '0x'
	masked = '0x'
	for (i, c) in enumerate(s):
		if 'A' <= c <= 'F' or '0' <= c <= '9':
			mask += 'F'
			masked += c
		elif 'g' <= c <= 'z':
			vars[c] = list(vars[c])
			vars[c][i] = 'F'
			vars[c] = ''.join(vars[c])
			mask += '0'
			masked += '0'
		else:
			mask += '0'
			masked += '0'

	decomposition = ''
	for c in vars:
		decomposition += 'let %s = (opcode land 0x%s) >> %d in ' % (c * vars[c].count('F'), vars[c], (3 - vars[c].rindex('F')) * 4)

	return before + 'opcode when opcode land ' + mask + ' = ' + masked + ' -> ' + decomposition + code + '\n'


f = open(sys.argv[1], 'r')
if sys.argv[1] == 'cpu.ml':
	for line in f:
		print(replace(line), end='')
else:
	for line in f:
		print(line, end='')
