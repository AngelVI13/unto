#!/usr/bin/bash
sqlgg -gen caml -name DbOps -params named ./queries.sql > db_ops.ml
