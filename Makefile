PROJECT = marray
PROJECT_DESCRIPTION = mutable array
PROJECT_VERSION = 0.0.1

EUNIT_OPTS = verbose


compile: all


devel: all shell


clean::
	@rm -f c_src/env.mk


include erlang.mk
