## my-web-server

# init project

run `stack build` and `stack exec my-web-server-exe`.

Folder with an example configuration file has already been created.
* confLimit - maximum user watch
* confConnectionInfo - connection to dataBase
* confPortServer - connection to Server
* confLogger - configuration for logger
* confMaxLimit - maximum news watch

# Structure

The project structure is divided into handler, implementation, servant api.

Top of source directory: handler interface, types, general configuration.

In child directory nodes: implementation.

In more detail:
* types, utils, get configuration - in directory Data.
* handler and implementation have hierarchic from server to servis.
