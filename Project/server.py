import time, datetime
import asyncio
import sys
import ssl
import json

# lnxsrv06
serverToPort = {}
serverToPort['Goloman'] = 11605
serverToPort['Hands'] = 11606
serverToPort['Holiday'] = 11607
serverToPort['Welsh'] = 11608
serverToPort['Wilkes'] = 11609

passesTo = {}
passesTo['Goloman'] = ['Hands', 'Holiday', 'Wilkes']
passesTo['Hands'] = ['Goloman', 'Wilkes']
passesTo['Holiday'] = ['Goloman', 'Welsh', 'Wilkes']
passesTo['Welsh'] = ['Holiday']
passesTo['Wilkes'] = ['Goloman', 'Hands', 'Holiday']


HOST = '127.0.0.1'
API_KEY = 'AIzaSyCO_AmJ3UtwUcNjmULruO55BddH25sSTf0'

def print_error(msg):
    log_write(msg)
    sys.stderr.write(msg)
    sys.exit(1)


def send_message(transport, message):
    transport.write(str.encode(message))
    log_write('Sent message: {}'.format(message))

def close_connection(transport):
    transport.close()
    log_write('Closing connection with {}'.format(transport.get_extra_info('peername')))


def log_write(msg):
    d = datetime.datetime.now()
    log_str = d.strftime('%Y-%m-%d %I:%M:%S %p')
    log_str += ' - (' + server_name + ') -: ' + msg + '\n'
    log_file.write(log_str)


class ServerClientProtocol(asyncio.Protocol):
    """
    This class defines how servers interact with clients and other servers
    within the herd.
    """
    ClientTimeStamps = {}
    def __init__(self, name):
        self.name = name
    
    def connection_made(self, transport):
        peername = transport.get_extra_info('peername')
        log_write('Connection from {}'.format(peername))
        print('Connection from {}'.format(peername))
        self.transport = transport
    
    def data_received(self, data):
        message = data.decode()
        # log data
        print('Data received: {!r}'.format(message))
        log_write('Data received: {!r}'.format(message))

        # parse incoming data
        args = message.split(' ')
        # check for leading or trailing whitespace
        if args[0] == ' ':
            args = args[1:]
        if len(args) > 0 and args[len(args)-1] == ' ':
            args = args[:-1]
        args[-1] = args[-1].strip()
        
        # handle commands
        if self.verifyArgs(args):
            # we have valid arguments
            if args[0] == 'IAMAT': # add further checks of each input to make sure valid
                self.handle_IAMAT(args)
            elif args[0] == 'WHATSAT':
                self.handle_WHATSAT(args)
                return
            elif args[0] == 'AT':
                self.handle_AT(args)
        else:
            self.transport.write(b'? ' + str.encode(message) + b'\n')
            log_write('? ' + message)
        
        close_connection(self.transport)
    
    def verifyArgs(self, args):
        """
        Checks each argument for validity to ensure no hiccups going forward
        """
        # check for IAMAT
        if args[0] == "IAMAT":
            # check correct length
            if len(args) != 4:
                log_write('IAMAT - invalid number of args!')
                return False
            # check longitude and latitude within correct bounds
            try:
                loc = args[2]
                if '+' in loc[0]:
                    if '-' in loc:
                        loc = ''.join(loc.split('+')).split('-')
                    else:
                        loc = loc.split('+')[1:]
                else:
                    if '+' in loc:
                        loc = ''.join(loc.split('-')).split('+')
                    else:
                        loc = loc.split('-')[1:]

                lat, long = loc[0], loc[1]
            except IndexError:
                log_write('IAMAT - Location is in wrong format!')
                return False
            # check for valid long/lat
            try:
                if abs(float(lat)) > 90 or abs(float(long)) > 180:
                    return False
            except ValueError:
                log_write('IAMAT - Latitude or Longitude values invalid!')
                return False
            # check time
            try:
                datetime.datetime.utcfromtimestamp(float(args[3]))
            except ValueError:
                log_write('IAMAT - Invalid POSIX time!')
                return False
        elif args[0] == "WHATSAT":
            # check correct length
            if len(args) != 4:
                log_write('WHATSAT - invalid number of args!')
                return False
            # check valid radius
            radius = args[2]
            infoBound = args[3]
            try:
                if int(radius) > 50 or int(infoBound) > 20:
                    return False
            except ValueError:
                log_write('WHATSAT - invalid radius or information bound value!')
                return False
            # check client exists
            try:
                ServerClientProtocol.ClientTimeStamps[args[1]]
            except KeyError:
                log_write('WHATSAT - client does not exists!')
                return False
        elif args[0] == "AT":
            # AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997 [VISITED Servers]
            # check minimum length
            if len(args) < 7:
                print('wrong number of args')
                log_write('AT - invalid number of args!')
                return False
            # check for valid server names
            for visited_server in args[6:]:
                if visited_server not in serverToPort.keys():
                    print('not a recognized server')
                    log_write('AT - not a recognized server name!')
                    return False
        else:
            # invalid first arg
            return False
        
        return True

    
    def handle_AT(self, args):
        """
        Handles AT messages given by other servers. Servers will read the 
        message and update the time stamp of the corresponding client then
        propagate the information to the other servers it is connected to.

        :input ex:
            AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997 [Visited Servers]
        """
        og_server = args[-1]
        visited = args[6:]

        #peername = self.transport.get_extra_info('peername')
        print('Got some data from {}'.format(og_server))
        log_write('Got some data from {}'.format(og_server))
        
        # update client stamp
        client_name = args[3]
        stamp = ' '.join(args[:6])
        ServerClientProtocol.ClientTimeStamps[client_name] = stamp # store as a string
        # mark server to one we've visited.        
        if self.name not in visited:
            visited.append(self.name)
        
        print('{}\'s client stamps: {}'.format(self.name, ServerClientProtocol.ClientTimeStamps))
        self.floodAndPropogate(stamp, visited)
        self.transport.write(str.encode('{} received updated location'.format(self.name))) # not really necessary
    

    def floodAndPropogate(self, stamp, visited):
        """
        Floods each server according to relationships indicating in spec. We
        attempt to keep track of the servers which have already been visited
        to not repeat ourselves
        """
        for receiver in passesTo[self.name]:
            if receiver not in visited:
                # new = [x for x in kon if x != self.name and x not in visited]
                update = [server for server in passesTo[self.name] if server != receiver and server not in visited]
                update.extend(visited)
                #update = list(passesTo[self.name])
                #update.remove(receiver)
                #update.extend(visited) # add all other servers except itself
                self.propagate(receiver, stamp, update, serverToPort[receiver]) # need separate function for coro
    

    def propagate(self, name, stamp, visited, port):
        """
        Creates a new coroutine, and connects to the server specified
        """
        coro = loop.create_connection(
            lambda: ServerToServerProtocol(stamp, name, visited),
            '127.0.0.1', 
            port
        )
        loop.create_task(coro)

    
    def handle_IAMAT(self, args):
        """
        Handles IAMAT messages. Tells server the client id, along with location coordinates,
        and time in POSIX format. Responds with an AT message repeating the time stamp but
        also specifying which server it was stored at.

        :input ex:
            IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
            [0] -> cmd, [1]-> client id, [2]->Long-Lat Coords, [3]->Posix Time
        :output ex:
            AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
        """
        response = "AT {} ".format(self.name)

        # get time difference
        args[-1] = args[-1].strip()
        posix_time = float(args[-1]) # there is a trailing \n
        d = datetime.datetime.now()
        cur_posix_time = float(time.mktime(d.timetuple()))
        diff = cur_posix_time - posix_time

        if diff > 0:
            response += '+'
        client = args[1]
        response += '{} {} {}'.format(str(diff), client, ' '.join(args[2:])) 

        # update client timestamp
        ServerClientProtocol.ClientTimeStamps[client] = response # don't store the newline character
        
        # generate Server AT Message: AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.06-118.445 15200239 Goloman
        self.floodAndPropogate(response, [self.name]) # propogate data

        # write AT response back to client
        log_write('Wrote AT response back to client: {}'.format(response))
        self.transport.write(str.encode(response))


    def handle_WHATSAT(self, args):
        """
        Handles WHATSAT messages. Asks server to return most recent time stamp for a partiuclar 
        client id. Server also acts as a client and sends a GET request to Google places API
        to obtain place info around the requested client.

        :input ex:
            WHATSAT kiwi.cs.ucla.edu 10 5
            [0] -> cmd, [1]-> client id, [2]->radius around client, [3]->number of places info
        :output ex:
            AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
            JSON of Google data
        """
        args[-1] = args[-1].strip() # get rid of \n
        client = args[1]
        stamp = ServerClientProtocol.ClientTimeStamps[client] # get most recent time stamp

        # get parameters
        radius = args[2]

        # break up response
        stampData = stamp.split(' ')
        loc = stampData[4]

        # handle location signs
        if '+' in loc[0]:
            if '-' in loc:
                loc = ''.join(loc.split('+')).split('-')
                loc[0], loc[1] = '+' + loc[0], '-' + loc[1]
            else:
                loc = loc.split('+')[1:]
                loc[0], loc[1] = '+' + loc[0], '+' + loc[1]
        else:
            if '+' in loc:
                loc = ''.join(loc.split('-')).split('+')
                loc[0], loc[1] = '-' + loc[0], '+' + loc[1]
            else:
                loc = loc.split('-')[1:]
                loc[0], loc[1] = '-' + loc[0], '-' + loc[1]

        
        latitude, longitude = loc[0], loc[1]
        # now we send out HTTP request
        req = self.gatherHTTPRequest(latitude, longitude, radius, API_KEY)
        
        # we can create an SSL context to base our connections
        ssl_context = ssl.create_default_context()
        ssl_context.check_hostname = False
        ssl_context.verify_mode = ssl.CERT_NONE

        # create new coroutine
        log_write('Sent HTTP request: {}'.format(req))
        coro = loop.create_connection(
            lambda: HTTPClientProtocol(self.transport, args, req, stamp),
            'maps.googleapis.com', # Host
            443, # HTTPS Port
            ssl=ssl_context
        )
        loop.create_task(coro)


    def gatherHTTPRequest(self, latitude, longitude, radius, api_key):
        """
        Builds GET request string
        """
        req = 'GET /maps/api/place/nearbysearch/json?' + \
              'location={},{}&radius={}&key={} '.format(latitude, longitude, radius, api_key) + \
              'HTTP/1.1\r\nHost: maps.googleapis.com\r\n\r\n'
        #req = 'GET /maps/api/place/nearbysearch/json?location=34.0689,-118.445&radius=10&key=AIzaSyCO_AmJ3UtwUcNjmULruO55BddH25sSTf0 HTTP/1.1\r\nHost: maps.googleapis.com\r\n'
        return req
                

class HTTPClientProtocol(asyncio.Protocol):
    """
    This class defines how the HTTP request is sent out and received
    by the servers acting as clients when performing the GET request.
    """
    def __init__(self, og_transport, args, request, stamp):
        self.og_transport = og_transport
        self.args = args
        self.request = request
        self.stamp = stamp
        self.data = '' # for decoding
    
    def connection_made(self, transport):
        self.transport = transport
        # send out GET request
        self.transport.write(str.encode(self.request))
    
    def data_received(self, data):
        self.data += data.decode()
        if self.data.count('\r\n\r\n') >= 2:
            self.gatherJson(self.data) # change data to correct JSON format
            send_message(self.og_transport, self.data)
            close_connection(self.og_transport)
            self.transport.close()
    
    def gatherJson(self, data):
        cleanData = data.split('\r\n\r\n')[1]
        indices = (cleanData.index('{'), cleanData.rindex('}') + 1)
        jsonData = cleanData[indices[0]:indices[1]]
        jsonData = jsonData.strip().replace('\r\n','').replace('\n','')
        infoBound = int(self.args[3])

        jsonObj = json.loads(jsonData)
        if infoBound < len(jsonObj['results']): # follow certain bound
            jsonObj['results'] = jsonObj['results'][:infoBound]
        
        # set data to new cleaned version
        self.data = '{}\n{}\n\n'.format(self.stamp, json.dumps(jsonObj, indent=3))


class ServerToServerProtocol(asyncio.Protocol):
    """
    This class defines how servers communicate with each other to 
    propagate the client time stamps
    """
    def __init__(self, stamp, server_name, visited):
        self.stamp = stamp
        self.name = server_name
        self.visited = visited
    
    def connection_made(self, transport):
        peername = transport.get_extra_info('peername')
        print('Connected to: {}.'.format(self.name))
        log_write('Connected to: {}'.format(self.name))
        newAT = self.stamp + ' ' + ' '.join(self.visited)
        self.transport = transport
        self.transport.write(str.encode(newAT))
        # where did we write
        print('Propogated location to server {}.'.format(self.name))
        log_write('Propagated location to server {}'.format(self.name))
    
    def connection_lost(self, transport):
        print('Dropped connection with {}'.format(self.name))
        log_write('Dropped connection with {}'.format(self.name))
        self.transport.close()


if __name__ == '__main__':

    if (len(sys.argv) != 2):
        print_error("Incorrect number of arguments!")
    
    # gather server and port names
    server_name = str(sys.argv[1])
    if server_name not in serverToPort.keys():
        print_error("Server name not valid! Only Goloman, Hands, Holiday, Welsh, Wilkes")
    
    port_num = serverToPort[server_name]
    
    # set up log file
    log_file = open('./logs/' + server_name.lower() + '.log', 'w+')

    # get the server started
    loop = asyncio.get_event_loop()
    coro = loop.create_server(lambda: ServerClientProtocol(server_name), HOST, port_num)
    server = loop.run_until_complete(coro)

    # run until we receive keyboard interrupt (from asyncio docs)
    log_write('Serving on: {}'.format(server.sockets[0].getsockname()))
    print('Serving on: {}'.format(server.sockets[0].getsockname()))
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        print('Keyboard interrupt. Closing now')
        pass

    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()


