SOCKET_PATH=/tmp/mysock
SERVER_PID=/tmp/mypid
SERVER_EXE=/Users/jserot/Dev/ml/rfsm/_build/default/src/guests/std/bin/rfsmc.exe
SERVER_ARGS=-server_mode -socket_path $(SOCKET_PATH)

run:
	rm -f $(SOCKET_PATH)
	$(SERVER_EXE) $(SERVER_ARGS) & echo "$$!" > $(SERVER_PID)
	@sleep 0.5 # Allow the server to start !
	nc -U $(SOCKET_PATH)
#	kill -9 $(SERVER_PID)

#REQUEST='{"version":""}'
REQUEST='{"compile":{"args":["-dot","./sample.fsm"]}}'
#REQUEST='{"check":{"inps":[{"id":"h","type":"event"}],outps:[{"id":"o","type":"bool"}],"vars":[{"id":"k","type":"int"}],"obj":"sval o=1"}}'
#REQUEST='{"check":{"inps":[{"id":"h","type":"event"}],outps:[{"id":"o","type":"bool"}],"vars":[{"id":"k","type":"int"}],"obj":"sval o=2"}}'

test:
	rm -f $(SOCKET_PATH)
	$(SERVER_EXE) $(SERVER_ARGS) & echo "$$!" > $(SERVER_PID)
	sleep 0.5
	echo $(REQUEST) | nc -U $(SOCKET_PATH)
#	kill -9 `cat $(SERVER_PID)`

clean:
	rm -f $(SOCKET_PATH)

