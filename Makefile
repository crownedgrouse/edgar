PROJECT = $(notdir $(shell pwd))
ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
CT_SUITES = edgar

# Use optional local.mk for any local need (added to .gitignore)
-include local.mk

include erlang.mk

# Generate ar reference files before using tests
init_test:
	cd test/edgar_SUITE_data && test -f ar_ref.ar || ar crU ar_ref.ar lorem_ipsum.txt erlang-logo.png
	cd test/edgar_SUITE_data && test -f ar_ref_long.ar || ar crU ar_ref_long.ar lorem_ipsum_long.txt erlang-logo_long.png
	cd test/edgar_SUITE_data && test -f ar_ref_mixte1.ar || ar crU ar_ref_mixte1.ar lorem_ipsum.txt erlang-logo_long.png
	cd test/edgar_SUITE_data && test -f ar_ref_mixte2.ar || ar crU ar_ref_mixte2.ar lorem_ipsum_long.txt erlang-logo.png
