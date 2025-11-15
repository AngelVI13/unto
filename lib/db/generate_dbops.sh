#!/usr/bin/bash
sqlgg -gen caml -name DbOps -params named ./queries.sql > db_ops.ml
sqlgg -gen caml -name UsersOps -params named ./users.sql > users_ops.ml
