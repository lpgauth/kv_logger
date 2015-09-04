#!/bin/bash
exec erl -sname ms_logger_dev \
         -pa ebin deps/*/ebin test \
         -config config/dev \
         -boot start_sasl \
         -s ms_logger_app \
         -setcookie secret
