# my-web-server

## init project

Firstly we mast initialized data base. 

For Linux:
* `sudo -i -u postgres`
* `psql`
* `CREATE USER youruser WITH PASSWORD 'yourpassword';`
* `CREATE DATABASE yourdatabase OWNER youruser;`

Record user name and database name in config.

Run `stack build` and `stack exec my-web-server-exe`.

Folder with an example configuration file has already been created.
* confLimit - maximum user watch
* confConnectionInfo - connection to dataBase
* confPortServer - connection to Server
* confLogger - configuration for logger
* confMaxLimit - maximum news watch

## Structure

The project structure is divided into handler, implementation, servant api.

Top of source directory: handler interface, types, general configuration.

In child directory nodes: implementation.

In more detail:
* types, utils, get configuration - in directory Data.
* handler and implementation have hierarchic from server to servis.
