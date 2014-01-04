PROJECT = qlglicko_core

ERLC_OPTS = +debug_info '+{parse_transform, lager_transform}'

DEPS = lager poolboy epgsql gproc jsx safetyvalve sfmt glicko2 uuid hackney

dep_uuid = https://github.com/jlouis/uuid master
dep_hackney = https://github.com/benoitc/hackney master
dep_lager = git://github.com/basho/lager.git 2.0.0
dep_poolboy = git://github.com/devinus/poolboy.git master
dep_epgsql = git://github.com/wg/epgsql.git master
dep_gproc  = git://github.com/uwiger/gproc.git master
dep_jsx    = git://github.com/talentdeficit/jsx.git master
dep_safetyvalve =  git://github.com/jlouis/safetyvalve.git master
dep_sfmt = git://github.com/jj1bdx/sfmt-erlang.git master
dep_glicko2 = git://github.com/jlouis/erl-glicko2.git master

include erlang.mk
