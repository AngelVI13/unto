To handle db migrations [golang-migrate](https://github.com/golang-migrate/migrate) tool is used  

[migrate CLI docs](https://github.com/golang-migrate/migrate/tree/master/cmd/migrate)

To install `migrate` tool:  
`go install -tags 'sqlite3' github.com/golang-migrate/migrate/v4/cmd/migrate@latest`  

To write new migration:  
- create 2 new .sql file for up and down migrations, following current naming format.
- "up" file contains logic to migrate to the next version, "down" file - to migrate to previous version (revert operation)

How to make db migrations if db schema is changed (from version 001 to 002):
`migrate -path=./pkg/db/migrations -database=sqlite3://./elt.db goto 002`
