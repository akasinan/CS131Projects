import asyncio
import urllib.parse
import sys
import json
import datetime
import aiohttp

API_KEY="AIzaSyAGf5eEMLbZUji9PpUNcHbG3c3-01ivM1M"
API_ENDPOINT="https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

PORT_NUM = {
	'Alford': 20005,
	'Ball': 20006,
	'Hamilton': 20007,
	'Holiday': 20008,
	'Welsh': 20009
}

NEIGHBORS = {
	'Alford': ['Hamilton', 'Welsh'],
	'Ball': ['Holiday', 'Welsh'],
	'Hamilton': ['Holiday', 'Alford'],
	'Holiday': ['Hamilton', 'Ball'],
	'Welsh': ['Ball', 'Alford']
}

COMMAND = ["IAMAT", "AT", "WHATSAT"]
LENGTH_OR_ARGS = {"IAMAT": 4, "AT": 6, "WHATSAT": 4, "MIN": 4}

server_name = ""
connectionNum = 0
fp = None
clients = {}


def isFloat(i):
	try:
		float(i)
		return True
	except ValueError:
		return False
	
async def connectToGoogle(url):
	#The original GET REQUEST CODE IS COMMENTED OUT
	print(url)
	#line = getRequest(url)
	loop = asyncio.get_event_loop()
	#reader, writer = await asyncio.open_connection("maps.googleapis.com", port=443, ssl=True, loop=loop)
	#writer.write(line.encode())
	async with aiohttp.ClientSession() as session:
		async with session.get(url) as response:
			line = await response.text()
			#print(line)
	#fp.write("Server Sent: \n" + line.strip() + "\n")
	#receivedLine = await reader.read()
	#receivedLine.decode()
	fp.write("Received: \n" + line + "\n")
	#writer.close()
	#receivedLine.decode()
	return line

async def errorReceived(fp, line, error, loop, rd ,wr):
	message = "? " + line
	wr.write(message.encode())
	fp.write("Error: {0} \nServer Response: {1}\n".format(error,message))
	writer.close()
	
async def lineReceived(line, rd, wr):
	global fp
	loop = asyncio.get_event_loop()
	if not len(line):
		await errorReceived(fp, line, "Empty line", loop,rd,wr)
		return
	toProcess = line.strip()
	clientMessage = toProcess.split()
	print(clientMessage)
	if len(clientMessage) < LENGTH_OR_ARGS["MIN"]:
		await errorReceived(fp, toProcess, "Too few arguments", loop, rd, wr)
		return
		
	fp.write("Client Sent: \n" + toProcess + "\n")
	if clientMessage[0] == COMMAND[0]:
		await handleIAMAT(fp, clientMessage,rd,wr)
	elif clientMessage[0] == COMMAND[1]:
		await handleAT(fp, clientMessage,rd,wr)
	elif clientMessage[0] == COMMAND[2]:
		await handleWHATSAT(fp, clientMessage,rd,wr)
	else:
		await errorReceived(fp, toProcess, "Command unrecognized. Please use IAMAT or WHATSAT.", loop, rd, wr)

def generateATMessage(client_time, stored_info):
	d = datetime.datetime.now()
	infoList = stored_info.split()
	print(infoList)
	server = infoList[0]
	rest_stored = " ".join(infoList[1:])
	timeDifference = str(d.timestamp() - float(client_time))
	if timeDifference[0] != "-":
		timeDifference = "+" + timeDifference
	return " ".join([COMMAND[1], server, timeDifference, rest_stored])
	
def parseLocation(location):
	return location.replace("+", " +").replace("-", " -").split()
	
async def propagate(fp, line, originServer, excludePropagator = True):
	global server_name
	loop = asyncio.get_event_loop()
	neighbors = NEIGHBORS[server_name]
	for server in neighbors:
		if excludePropagator or server != originServer:
			fp.write("Propogating to " + server + "\n")
			try:
				reader, writer = await asyncio.open_connection("localhost", PORT_NUM[server], loop=loop)
			except ConnectionRefusedError:
				print("Connection Refused to {0}".format(server))
				continue
			writer.write(line.encode())
			writer.close()

async def handleIAMAT(fp, message, rd, wr):
	loop = asyncio.get_event_loop()
	print("IAMAT")
	if len(message) != LENGTH_OR_ARGS["IAMAT"]:
		await errorReceived(fp," ".join(message), "Number of inputs for IAMAT incorrect",loop,rd,wr)
		return
	name = message[1]
	location = message[2]
	if "+" not in location and "-" not in location:
		await errorReceived(fp," ".join(message), "Invalid location param in input",loop,rd,wr)
		return
	location = parseLocation(location)
	if len(location) != 2 or (not isFloat(location[0]) or not isFloat(location[1])):
		await errorReceived(fp," ".join(message), "Invalid location param in input",loop,rd,wr)
		return
	clientTime = message[3]
	if not isFloat(clientTime):
		await errorReceived(fp," ".join(message), "Invalid time param in input",loop,rd,wr)
		return
	toStore = server_name + " "
	toStoreRest = " ".join(message[1:])
	toStore = toStore + toStoreRest
	line = generateATMessage(clientTime,toStore)
	print(line)
	await propagate(fp, line, server_name, False)
	line = line + "\n"
	wr.write(line.encode())
	
async def handleAT(fp, message, rd, wr):
	print("AT")
	loop = asyncio.get_event_loop()
	if len(message) != LENGTH_OR_ARGS["AT"]:
		await errorReceived(fp, " ".join(message), "Number of inputs for AT incorrect", loop,rd,wr)
		return
	originServer = message[1]
	if originServer not in PORT_NUM:
		await errorReceived(fp, " ".join(message), "Invalid server name", loop,rd,wr)
		return
	client = message[3]
	clientTime = message[-1]
	if not isFloat(clientTime):
		await errorReceived(fp, " ".join(message), "Invalid time in input", loop,rd,wr)
		return
	
	server = message[1]
	toStore = server + " "
	toStoreRest = " ".join(message[3:])
	toStore = toStore + toStoreRest
	print(toStore)
	originATMessage = " ".join(message)
	ATMessage = " ".join(message)
	if client not in clients:
		clients[client] = toStore
		fp.write("Propagation Received: \n" + " ".join(message) + "\n")
		await propagate(fp, ATMessage, originServer, False)
	elif clients[client] != toStore:
		storeTime = clients[client].split()[-1]
		if float(storeTime) < float(clientTime):
			clients[client] = toStore
			fp.write("Propagation Received: \n" + " ".join(message) + "\n")
			await propagate(fp, ATMessage, originServer, False)

async def handleWHATSAT(fp, message, rd, wr):
	loop = asyncio.get_event_loop()
	print("WHATSAT")
	if len(message) != LENGTH_OR_ARGS["WHATSAT"]:
		await errorReceived(" ".join(message), "Number of parameters for WHATSAT incorrect",rd,wr)
		return
	client = message[1]
	radius = message[2]
	upperBound = message[3]
	if not isFloat(radius) or not upperBound.isdigit():
		await errorReceived(" ".join(message), "Radius or Upper bound format for WHATSAT incorrect",rd,wr)
		return
	radius = float(radius)
	upperBound = int(upperBound)
	if radius <= 0 or radius > 50 or upperBound <= 0 or upperBound > 20:
		await errorReceived(" ".join(message), "Radius or Upper bound for WHATSAT incorrect",rd,wr)
		return
	if client not in clients:
		await errorReceived(fp, " ".join(message), "Client ID not found",loop,rd,wr)
		return
		
	storedInfo = clients[client].split()
	print(storedInfo)
	location = parseLocation(storedInfo[2])
	radius *= 1000
	url = "{0}".format(API_ENDPOINT) + "location={0},{1}&radius={2}&key={3}".format(location[0], location[1], radius, API_KEY)
	getResult = await connectToGoogle(url) 
	start = getResult.index("{")
	end = getResult.rindex("}") + 1
	jsonLine = getResult[start:end]
	await handleJSON(fp, jsonLine, upperBound, client, rd, wr)

async def handleJSON(fp, data, upperBound, client, rd, wr):
	placeData = json.loads(data)
	placeData["results"] = placeData["results"][: upperBound]
	JSONStr = json.dumps(placeData,  indent = 4, separators = ( ',', ': '))
	storedInfo = clients[client]
	clientTime = storedInfo.split()[-1]
	response = generateATMessage(clientTime, storedInfo) + "\n" + JSONStr
	response = response.rstrip("\n") + "\n\n"
	await propagate(fp, response, server_name, False)
	wr.write(response.encode())
	fp.write("Server response:\n" + response + "\n")
	
#def handleGoogleError(error, message):
	#errorReceived(" ".join(message), error)
	
def startLogging(name):
	file = name + "Log.txt"
	global fp
	fp = open(file,"a")
	fp.write("Created log file for {0}\n".format(name))

#FUNCTION TO MAKE GET REQUEST
def getRequest(url): 
	host = url[8:27]
	print(host)
	uri = url[27:]
	print(uri)
	line = "GET {0} HTTP/1.1\r\nHost: {1}\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n\r\n".format(uri,host)
	return line
	
async def serverConnected(reader, writer):
	readLine = await reader.readline()
	global connectionNum 
	connectionNum += 1
	message = "New client connected. Clients Connected = {0}".format(connectionNum)
	fp.write(message + "\n")
	print(readLine.decode())
	await lineReceived(readLine.decode(),reader,writer)
	
def startLoop(fp, portno):
	loop = asyncio.get_event_loop()
	coro = asyncio.start_server(serverConnected, "localhost", portno, loop=loop)
	server = loop.run_until_complete(coro)
	
	try:
		loop.run_forever()
	except KeyboardInterrupt:
		pass
	server.close()
	loop.run_until_complete(server.wait_closed())
	loop.close()
	
def main():
	if(len(sys.argv) != 2):
		print("Incorrect usage: [Server Name]")
		exit(1)
	portno = 0
	global server_name
	server_name = sys.argv[1]
	if not PORT_NUM.get(server_name, 0):
		print("Server name invalid: Use Alford, Ball, Hamilton, Holiday or Welsh")
		exit(1)
	else:
		portno = PORT_NUM[server_name]
	fp = startLogging(server_name)
	print("start")
	startLoop(fp, portno)

	
if __name__ == "__main__": main()