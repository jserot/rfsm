QMAKE=/Users/jserot/Qt2/6.9.1/macos/bin/qmake
SOCK_PATH=/tmp/rfsm_sock
SERVER_PID=/tmp/rfsm_pid

all:
	(cd client; make)
qmake:
	(cd client; $(QMAKE))
clean:
	(cd client; make clean)
#	rm -f $(SOCK_PATH)
clobber:
	(cd client; make distclean)
#	rm -f $(SOCK_PATH)
run:
	rm -f $(SOCK_PATH)
# Server is now launched by client
	(cd client/client.app/Contents/MacOS; ./client)
#	kill -9 `cat $(SERVER_PID)`
