REBAR=rebar3

compile:
	${REBAR} compile | sed -e 's|_build/default/lib/qlglicko_core/||'

dialyzer:
	${REBAR} dialyzer | sed -e 's|_build/default/lib/qlglicko_core/||'

clean:
	${REBAR} clean
