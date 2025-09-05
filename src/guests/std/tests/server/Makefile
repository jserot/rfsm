SOCK_PATH=./rfsm_sock
SERVER_PID=/tmp/rfsmc_pid

build:
	dune build

clean:
	dune clean
	rm -f $(SOCK_PATH)

run:
	rm -f $(SOCK_PATH)
#	pkill -l server.exe # Not required if the server process has been killed properly as below...
	../../../../../_build/default/src/guests/std/bin/rfsmc.exe -server_mode -socket_path $(SOCK_PATH) & echo "$$!" > $(SERVER_PID)
	@sleep 0.5 # Allow the server to start !
	../../../../../_build/default/src/guests/std/tests/server/client.exe $(SOCK_PATH)
#	kill -9 `cat $(SERVER_PID)`
