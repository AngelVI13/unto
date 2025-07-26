#!/usr/bin/bash
sqlgg -gen caml -name DbOps ./queries.sql > db_ops.ml
