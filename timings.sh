#!/bin/bash
# shellcheck disable=SC2001

erl_version() {
    erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell
}

echo "ERLANG VERSION: $(erl_version)"

export PROFILE=inline

rebar3 compile

if [ "$#" -eq 0 ]; then
    DAYS=(apps/aoc/src/day*.erl)
else
    DAYS=("$@")
fi
for d in "${DAYS[@]}"; do
    DAY=$(echo "$d" | sed -e 's|.*/\(day.*\).erl|\1|')
    CMD=("$HOME"/src/github/erlperf/_build/default/bin/erlperf
         "$DAY:solve()."
         -pa "_build/default/lib/aoc/ebin"
         -w 5
        )
    if [ "$d" = "${DAYS[0]}" ]; then
        "${CMD[@]}"
    else
        "${CMD[@]}" | tail -n 1
    fi
done
