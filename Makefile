PROJECT = $(notdir $(shell pwd))
ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
CT_SUITES = edgar

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2

dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

include erlang.mk

init_test:
	cd test/edgar_SUITE_data && test -f ar_ref.ar || ar crU ar_ref.ar lorem_ipsum.txt erlang-logo.png
	cd test/edgar_SUITE_data && test -f ar_ref_long.ar || ar crU ar_ref_long.ar lorem_ipsum_long.txt erlang-logo_long.png
	cd test/edgar_SUITE_data && test -f ar_ref_mixte1.ar || ar crU ar_ref_mixte1.ar lorem_ipsum.txt erlang-logo_long.png
	cd test/edgar_SUITE_data && test -f ar_ref_mixte2.ar || ar crU ar_ref_mixte2.ar lorem_ipsum_long.txt erlang-logo.png
