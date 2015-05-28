import os

import socket


TCP_IP = '127.0.0.1'
TCP_PORT = 8889
BUFFER_SIZE = 20
MESSAGE = "ok"

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((TCP_IP, TCP_PORT))

s.send(MESSAGE)
i = 0
while True:
	i += 1
	data = s.recv(BUFFER_SIZE)
	print "received data:", data, i
	if data == "ok":
		s.send("join")
	if data == "its accepted":
		s.send("thanks")
	
	s.send("xxx")
s.close()
