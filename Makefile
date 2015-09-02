REBAR=rebar3

compile:
	${REBAR} compile | sed -e 's|_builds/default/lib/qlglicko_core/||'


