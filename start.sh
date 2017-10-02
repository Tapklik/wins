#!/bin/sh
NODE=wins@$(hostname)
APP=wins
cd `dirname $0`
exec erl -smp auto +P 134217727 +K true +A 64 -pa $PWD/_build/default/lib/*/ebin -boot start_sasl -s lager -s $APP $@ -config app.config -sname $NODE -setcookie chat_cookie
